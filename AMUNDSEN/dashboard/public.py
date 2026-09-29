"""What the public copy of the dashboard may carry.

The web copy (``tools/publish-web.sh``, on grid) releases only what Amundsen
Science has already released itself. This module rewrites grid's mirror of
the web root in place before it goes to the web server:

* the schedule's whiteboard note is the ship's internal notice board, so it
  is emptied, and a "latest change" that quotes it is dropped;
* casts are listed and served only for legs whose profiles are in Amundsen
  Science's catalogue (ERDDAP ``amundsen12713``, which ends with 2024); the
  station markers stay, since the public event log carries their times and
  places;
* window files the public manifest does not list are withheld.

What is withheld is listed in ``.public-filter`` as rsync hide rules
(``H /path``): hidden from the sender only, so the upload's ``--delete``
also removes a copy already on the web server.

The underway record is thinned earlier, in grid's own rebuild of the windows
and the track (``dashboard build --tracks-only --public``), by
``public_frame``: data.amundsen.ulaval.ca publishes a position every 5
minutes and a reading every 15, and for 2025 ERDDAP carries TSG and
navigation per minute (amundsen12715, amundsen12447).

Every rewrite is atomic (a temporary file beside the target, then a rename),
so an interrupted run leaves the previous file standing.
"""

from __future__ import annotations

import json
import os
import re
import tempfile
from pathlib import Path

import numpy as np
import pandas as pd

from .config import VARIABLES, WINDOWS, Window

# Before this instant ERDDAP carries the TSG and navigation per minute; from
# it on, only data.amundsen.ulaval.ca's 5-minute track and 15-minute readings.
PER_MINUTE_UNTIL = pd.Timestamp("2026-01-01", tz="UTC")
POSITION_STEP = "5min"
READING_STEP = "15min"
READING_STEP_S = 900
# windows shorter than this hold too few 15-minute readings to draw
MIN_WINDOW_HOURS = 12
# navigation as ERDDAP's amundsen12447 has it (position, heading, speed,
# roll, pitch, heave), per minute for 2025
NAV_VARIABLES = {"Heading (°)", "Ship speed (kn)", "Sea state · 4σ heave (m)", "Roll & pitch RMS (°)"}
# the last year whose casts are in the catalogue
CASTS_PUBLISHED_THROUGH = 2024


def _write(path: Path, data) -> None:
    fd, tmp = tempfile.mkstemp(prefix=f".{path.name}-", suffix=".tmp", dir=path.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(data, stream, separators=(",", ":"))
        os.chmod(tmp, path.stat().st_mode & 0o777 if path.exists() else 0o644)
        os.replace(tmp, path)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def strip_whiteboard(root: Path, manifest: dict) -> bool:
    """Empty the whiteboard in ``data/calendar.json`` and drop the manifest's
    latest-change note when it is the whiteboard's. True when anything changed."""
    changed = False
    update = (manifest.get("calendar") or {}).get("update")
    if isinstance(update, dict) and update.get("kind") == "whiteboard":
        del manifest["calendar"]["update"]
        changed = True
    path = root / "data/calendar.json"
    if path.is_file():
        cal = json.loads(path.read_text())
        schedule = cal.get("schedule")
        if isinstance(schedule, dict) and schedule.get("whiteboard"):
            schedule["whiteboard"] = ""
            _write(path, cal)
            changed = True
    return changed


def _bin(df: pd.DataFrame, rule: str, circular: set[str]) -> pd.DataFrame:
    """Per-``rule`` means; circular columns (degrees) by their mean direction,
    leg by its first value, flags by their maximum."""
    if df.empty:
        return df
    flags = [c for c in ("provisional", "pump_low") if c in df]
    angles = [c for c in df.columns if c in circular]
    plain = [c for c in df.columns if c not in circular and c not in flags and c != "leg"]
    g = df.resample(rule)
    out = g[plain].mean() if plain else pd.DataFrame(index=g.size().index)
    for c in flags:
        out[c] = g[c].max()
    if "leg" in df:
        out["leg"] = g["leg"].first()
    for c in angles:
        rad = np.deg2rad(df[c])
        out[c] = np.rad2deg(np.arctan2(np.sin(rad).resample(rule).mean(), np.cos(rad).resample(rule).mean())) % 360
    return out[[c for c in df.columns if c in out]]


def _thin(df: pd.DataFrame, position_rule: str, fine: set[str], circular: set[str]) -> pd.DataFrame:
    """Rows every ``position_rule`` carrying position and the ``fine``
    columns; every other column only on the 15-minute rows."""
    if df.empty:
        return df
    reading = [c for c in df.columns if c not in fine]
    base = _bin(df[[c for c in df.columns if c in fine]], position_rule, circular)
    slow = _bin(df[reading], READING_STEP, circular)
    for c in reading:
        base[c] = slow[c].reindex(base.index)
    base = base[list(df.columns)]
    return base.dropna(how="all", subset=[c for c in base.columns if c not in ("leg", "provisional", "pump_low")])


def public_frame(frame: pd.DataFrame, cutoff: pd.Timestamp = PER_MINUTE_UNTIL) -> pd.DataFrame:
    """The analysed record at the resolution Amundsen Science has released:
    before ``cutoff``, TSG and navigation per minute and the rest per 15
    minutes; from it on, position per 5 minutes and every reading per 15."""
    circular = {v.name for v in VARIABLES if v.circular}
    position = {"lat", "lon", "dist_km", "leg", "provisional", "pump_low"}
    fine_before = position | {v.name for v in VARIABLES if v.tsg} | NAV_VARIABLES | {"Distance travelled (km)"}
    idx = frame.index if frame.index.tz is not None else frame.index.tz_localize("UTC")
    early = frame[idx < cutoff]
    late = frame[idx >= cutoff]
    parts = [_thin(early, "1min", fine_before & set(frame.columns), circular),
             _thin(late, POSITION_STEP, (position | {"Distance travelled (km)"}) & set(frame.columns), circular)]
    parts = [p for p in parts if not p.empty]
    return pd.concat(parts).sort_index() if parts else frame.iloc[:0]


def public_windows(windows: tuple[Window, ...] = WINDOWS) -> tuple[Window, ...]:
    """The chart windows long enough to show 15-minute readings, none finer."""
    return tuple(Window(w.label, w.hours, max(w.step_s, READING_STEP_S)) for w in windows if w.hours >= MIN_WINDOW_HOURS)


def _leg_year(leg: str) -> int | None:
    m = re.match(r"(\d{4})", leg or "")
    return int(m.group(1)) if m else None


def restrict_casts(root: Path, manifest: dict) -> list[str]:
    """Keep in ``data/casts/index.json`` (and the LADCP index) only the casts
    of catalogued years; return the web-root paths no longer to be served."""
    index_path = root / "data/casts/index.json"
    if not index_path.is_file():
        return []
    index = json.loads(index_path.read_text())
    kept, hidden = [], []
    for c in index.get("casts", []):
        year = _leg_year(c.get("leg", ""))
        (kept if year is not None and year <= CASTS_PUBLISHED_THROUGH else hidden).append(c)
    withheld = sorted({c["file"] for c in hidden if c.get("file")})
    hidden_ids = {c.get("id") for c in hidden}
    if hidden:
        index["casts"] = kept
        _write(index_path, index)
    ladcp_path = root / (index.get("ladcp_file") or "data/casts/ladcp.json")
    if ladcp_path.is_file():
        ladcp = json.loads(ladcp_path.read_text())
        before = ladcp.get("casts", [])
        after = [c for c in before if c.get("parent_cast_id") not in hidden_ids
                 and (_leg_year(str(c.get("parent_cast_id", ""))) or 0) <= CASTS_PUBLISHED_THROUGH]
        if len(after) != len(before):
            ladcp["casts"] = after
            _write(ladcp_path, ladcp)
    if isinstance(manifest.get("casts"), dict):
        manifest["casts"]["n"] = len(kept)
    return withheld


def stale_windows(root: Path, manifest: dict) -> list[str]:
    """Window files in the web root that the manifest does not list."""
    listed = {w.get("file") for w in manifest.get("windows", [])}
    return sorted(f"data/{p.name}" for p in (root / "data").glob("w-*.json") if f"data/{p.name}" not in listed)


def restrict(root: Path) -> dict:
    """Apply every rule to the web root at ``root`` in place, and write its
    ``.public-filter``."""
    root = Path(root)
    manifest_path = root / "data/manifest.json"
    manifest = json.loads(manifest_path.read_text())
    strip_whiteboard(root, manifest)
    withheld = restrict_casts(root, manifest) + stale_windows(root, manifest)
    if manifest.get("default_window") not in {w.get("label") for w in manifest.get("windows", [])} and manifest.get("windows"):
        manifest["default_window"] = manifest["windows"][0]["label"]
    _write(manifest_path, manifest)
    rules = "".join(f"H /{path}\n" for path in withheld)
    (root / ".public-filter").write_text(rules)
    return {"withheld": len(withheld)}
