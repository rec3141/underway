"""Bounded, immutable map observations shared by all dashboard time windows."""
from __future__ import annotations

import hashlib
import json
import os
import re
import time
from pathlib import Path

import numpy as np
import pandas as pd

from .config import VARIABLES
from .derive import MAX_PLAUSIBLE_KN, haversine_km

CACHE_VERSION = 3  # Bump when selection or serialization semantics change.
CHUNK_ROWS = 2048
SPACINGS_KM = (1.0, 0.1, 0.025, 0.005, 0.0)


def _select(frame, spacing):
    """Keep native observations, limiting travelled distance and bend error."""
    n = len(frame)
    if spacing == 0 or n < 3:
        return np.arange(n)
    lat, lon = frame.lat.to_numpy(), frame.lon.to_numpy()
    steps = haversine_km(lat[:-1], lon[:-1], lat[1:], lon[1:])
    distance = np.r_[0, np.cumsum(steps)]
    # Preserve bends using Ramer-Douglas-Peucker in a local metric projection.
    x = np.unwrap(np.deg2rad(lon)) * 6371.0088 * np.cos(np.deg2rad(np.mean(lat)))
    y = np.deg2rad(lat) * 6371.0088
    endpoints = list(range(0, n - 1, CHUNK_ROWS - 1)) + [n - 1]
    keep = set(endpoints)
    stack = list(zip(endpoints[:-1], endpoints[1:]))
    while stack:
        a, b = stack.pop()
        if b <= a + 1:
            continue
        dx, dy = x[b] - x[a], y[b] - y[a]
        denominator = dx * dx + dy * dy
        u = np.clip(((x[a+1:b] - x[a]) * dx + (y[a+1:b] - y[a]) * dy) / denominator, 0, 1) if denominator else 0
        errors = np.hypot(x[a+1:b] - x[a] - u * dx, y[a+1:b] - y[a] - u * dy)
        k = a + 1 + int(np.argmax(errors))
        if errors[k-a-1] > spacing / 10:
            keep.add(k)
            stack.extend(((a, k), (k, b)))
    # Select the last native observation BEFORE exceeding the spacing budget.
    # A native gap larger than the budget cannot be repaired without invention.
    anchor = 0
    for i in range(1, n):
        if distance[i] - distance[anchor] > spacing:
            if i - 1 > anchor:
                keep.add(i - 1)
                anchor = i - 1
            if distance[i] - distance[anchor] > spacing:
                keep.add(i)
                anchor = i
    return np.array(sorted(keep))


def _numbers(values, digits=4):
    return [round(float(v), digits) if pd.notna(v) and np.isfinite(v) else None for v in values]


def _payload(frame, segment):
    n = len(frame)
    result = {"n": n, "segment": segment,
              "t": frame.index.as_unit("ms").asi8.tolist(),
              "lat": _numbers(frame.lat, 6), "lon": _numbers(frame.lon, 6),
              "dist_km": _numbers(frame.dist_km, 3),
              "leg": [int(v) if pd.notna(v) else None for v in frame.leg],
              "vars": {v.name: _numbers(frame[v.name]) for v in VARIABLES if v.name in frame
                       and v.name not in ("Time elapsed (h)", "Distance travelled (km)")}}
    for name in ("pump_low", "provisional"):
        result[name] = [bool(v >= .5) if pd.notna(v) else False for v in frame[name]] if name in frame else [False] * n
    return result


def publish_track(frame: pd.DataFrame, root: Path) -> dict:
    """Publish leg/day chunks; return the small manifest embedded by build.py.

    Segment ids mark genuine breaks. Adjacent day/size chunks in a segment
    share one observation, allowing independent rendering without lost edges.
    dist_km remains absolute; the client applies its selected-window origin.
    """
    manifest = {"version": 1, "start": None, "end": None,
                "dist_start_km": None, "dist_end_km": None,
                "levels": [{"spacing_km": s, "chunks": []} for s in SPACINGS_KM]}
    if frame.empty:
        return manifest
    frame = frame.sort_index()
    if "pump_low" in frame:
        # Match chart masking: one adjacent native sample covers intake flushing.
        low = frame["pump_low"].fillna(0).ge(.5)
        frame = frame.copy(deep=False)
        frame["pump_low"] = low | low.shift(1, fill_value=False) | low.shift(-1, fill_value=False)
    stamp = frame.index.as_unit("ms").asi8
    manifest.update(start=int(stamp[0]), end=int(stamp[-1]))
    dist = frame.dist_km.dropna()
    if len(dist):
        manifest.update(dist_start_km=float(dist.iloc[0]), dist_end_km=float(dist.iloc[-1]))
    valid = np.isfinite(frame.lat.to_numpy()) & np.isfinite(frame.lon.to_numpy())
    dt = np.diff(stamp) / 3600000
    with np.errstate(divide="ignore", invalid="ignore"):
        speed = haversine_km(frame.lat.to_numpy()[:-1], frame.lon.to_numpy()[:-1], frame.lat.to_numpy()[1:], frame.lon.to_numpy()[1:]) / dt / 1.852
    breaks = np.r_[True, (frame.leg.to_numpy()[1:] != frame.leg.to_numpy()[:-1]) | (dt > 1) | (dt <= 0) | (speed > MAX_PLAUSIBLE_KN) | ~valid[:-1] | ~valid[1:]]
    boundaries = np.r_[np.flatnonzero(breaks), len(frame)]
    destination = Path(root) / "data" / "track"
    destination.mkdir(parents=True, exist_ok=True)
    for a, b in zip(boundaries[:-1], boundaries[1:]):
        if not valid[a]:
            continue
        segment_frame = frame.iloc[a:b]
        segment = f"{stamp[a]}:{segment_frame.leg.iloc[0]}"
        days = segment_frame.index.normalize()
        day_ends = np.r_[np.flatnonzero(days[1:] != days[:-1]) + 1, len(segment_frame)]
        day_begin = 0
        for day_end in day_ends:
            # Partition before simplification: completed days remain reusable
            # when new observations arrive. Keep a native seam in both days.
            daily = segment_frame.iloc[max(0, day_begin - 1):day_end]
            day_begin = int(day_end)
            identity = json.dumps([CACHE_VERSION, CHUNK_ROWS, SPACINGS_KM, segment, list(daily.columns),
                                   [v.name for v in VARIABLES]], ensure_ascii=False).encode()
            fingerprint = hashlib.sha256(identity + pd.util.hash_pandas_object(daily, index=True).values.tobytes()).hexdigest()[:24]
            cache_path = destination / (fingerprint + ".index")
            cached = None
            if cache_path.exists():
                try:
                    cached = json.loads(cache_path.read_text())
                    if len(cached) != len(SPACINGS_KM) or not all((Path(root) / c["file"]).exists() for chunks in cached for c in chunks):
                        cached = None
                except (ValueError, OSError, TypeError, KeyError):
                    cached = None
            if cached is not None:
                # At most one metadata write per day for repeatedly reused days.
                if cache_path.stat().st_mtime < time.time() - 86400:
                    os.utime(cache_path, None)
                for level, chunks in zip(manifest["levels"], cached):
                    level["chunks"].extend(chunks)
                continue
            cached = []
            for level in manifest["levels"]:
                selected = daily.iloc[_select(daily, level["spacing_km"])]
                entries = []
                begin = 0
                while begin < len(selected):
                    end = min(begin + CHUNK_ROWS, len(selected))
                    chunk = selected.iloc[begin:end]
                    payload = _payload(chunk, segment)
                    encoded = json.dumps(payload, separators=(",", ":"), ensure_ascii=False, allow_nan=False).encode()
                    name = hashlib.sha256(encoded).hexdigest()[:24] + ".json"
                    path = destination / name
                    if not path.exists():
                        temporary = path.with_suffix(".tmp")
                        temporary.write_bytes(encoded)
                        temporary.replace(path)
                    # The index must cover the short crossing at the dateline,
                    # not the almost-worldwide interval between raw longitudes.
                    longitude = np.rad2deg(np.unwrap(np.deg2rad(payload["lon"])))
                    entries.append({"file": f"data/track/{name}", "n": len(chunk),
                        "bounds": [round(float(longitude.min()), 6), min(payload["lat"]), round(float(longitude.max()), 6), max(payload["lat"])],
                        "start": payload["t"][0], "end": payload["t"][-1], "segment": segment,
                        "leg": payload["leg"][0]})
                    if end == len(selected):
                        break
                    begin = end - 1
                level["chunks"].extend(entries)
                cached.append(entries)
            temporary = cache_path.with_suffix(".tmp")
            temporary.write_text(json.dumps(cached, separators=(",", ":")))
            temporary.replace(cache_path)
    return manifest


def prune_track(root: Path, manifest: dict, grace_days: float = 7) -> int:
    """Remove unreferenced old payloads only after the new page is published.

    Reused fingerprint indexes refresh their age at most once per day.
    The grace period lets existing browser sessions finish loading their version.
    """
    referenced = {entry["file"] for level in manifest.get("levels", [])
                  for entry in level.get("chunks", [])}
    cutoff = time.time() - grace_days * 86400
    removed = 0
    destination = Path(root) / "data" / "track"
    if not destination.exists():
        return 0
    for path in destination.iterdir():
        if not re.fullmatch(r"[0-9a-f]{24}\.(json|index)", path.name):
            continue
        if f"data/track/{path.name}" in referenced:
            continue
        try:
            if path.stat().st_mtime < cutoff:
                path.unlink()
                removed += 1
        except FileNotFoundError:
            pass
    return removed
