"""Produce the static dashboard from every leg together.

Each leg's store is synced incrementally, the frames are concatenated in time
order with a leg code per row, and the combined record is sliced into windows
that reach back from the latest observation. Files are written to a temporary
name and renamed into place so a browser polling the directory never reads a
half-written file.
"""

from __future__ import annotations

import csv
import json
import logging
import math
import os
import shutil
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd
from jinja2 import Environment, FileSystemLoader

from . import __version__
from .config import (CAMERA_OUTPUT, DEFAULT_WINDOW, INTRANET_BASE, INTRANET_LINKS, LOCAL_TZ, LOW_FLOW_V, MAP_KM_STEP, QUANTILE_LIMITS, SURPRISE_ALERT, SURPRISE_ALERT_SCALE,
                     SURPRISE_SCALES, VARIABLES, WINDOWS, WINDOW_FILLED, Window)
from .derive import Analysis, build_analysis, needed_keys
from .ingest import Store, sync
from .legs import Leg, discover

log = logging.getLogger(__name__)
PKG = Path(__file__).parent


def atomic_write(path: Path, text: str) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text)
    os.replace(tmp, path)


# ---------------------------------------------------------------- stations

def read_stations(path: Path | None, leg_id: str) -> list[dict]:
    """CTD logbook rows; tolerant of a missing or partial file, since a leg's
    logbook does not exist until its first cast."""
    if not path or not path.is_file():
        return []
    out = []
    with path.open(encoding="utf-8-sig", newline="") as fh:
        for row in csv.DictReader(fh):
            row = {k.strip().lower(): (v or "").strip() for k, v in row.items() if k}
            try:
                lat, lon = float(row.get("latitude", "")), float(row.get("longitude", ""))
            except ValueError:
                continue
            if not (abs(lat) <= 90 and abs(lon) <= 180):
                continue
            out.append({
                "leg": leg_id, "cast": row.get("cast", ""), "label": row.get("label", ""),
                "station": row.get("station", ""), "time": row.get("date_utc", ""),
                "lat": lat, "lon": lon,
                "bottom_m": _num(row.get("bottom_m")), "depth_m": _num(row.get("depth_m")),
                "type": row.get("type_cast", ""), "comments": row.get("comments", ""),
            })
    return out


def _num(s):
    try:
        v = float(s)
        return None if math.isnan(v) else v
    except (TypeError, ValueError):
        return None


# ---------------------------------------------------------------- windows

def _circular_mean_deg(x: pd.Series) -> float:
    """Unweighted compass mean; a vanishing resultant has no direction."""
    r = np.radians(x.dropna().to_numpy())
    if r.size == 0:
        return np.nan
    s, c = np.sin(r).mean(), np.cos(r).mean()
    if np.hypot(s, c) < 1e-12:
        return np.nan
    angle = float(np.degrees(np.arctan2(s, c)) % 360)
    return 0.0 if angle >= 360 - 1e-10 else angle


def camera_index(output: Path, frame: pd.DataFrame | None) -> list[dict]:
    """One entry per daily timelapse under ``output`` (``<leg>/days/<day>/``,
    or ``days/<day>/`` for a single-leg output): when it was shot, where (the
    median of the shots' own positions, else the ship's track at the middle
    of the day) and the URL the page plays it from."""
    if not output.is_dir():
        return []
    out = []
    for meta in sorted(output.glob("*/days/????????/latest.json")) + sorted(output.glob("days/????????/latest.json")):
        try:
            d = json.loads(meta.read_text())
        except (OSError, ValueError):
            continue
        rel = meta.parent.relative_to(output)
        leg = rel.parts[0] if len(rel.parts) == 3 else None
        fr = d.get("frames") or []
        lats = [f["lat"] for f in fr if f.get("lat") is not None]
        lons = [f["lon"] for f in fr if f.get("lon") is not None]
        lat = lon = None
        if lats and lons:
            lat, lon = float(np.median(lats)), float(np.median(lons))
        start, end = pd.Timestamp(d.get("start_utc")), pd.Timestamp(d.get("end_utc"))
        mid = start + (end - start) / 2
        if lat is None and frame is not None and not frame.empty:
            pos = frame[["lat", "lon"]].dropna()
            if not pos.empty:
                i = pos.index.get_indexer([mid], method="nearest")[0]
                if abs((pos.index[i] - mid).total_seconds()) <= 3600:
                    lat, lon = float(pos["lat"].iloc[i]), float(pos["lon"].iloc[i])
        out.append({"leg": leg, "day": meta.parent.name, "start_utc": d.get("start_utc"), "end_utc": d.get("end_utc"),
                    "mid_utc": mid.isoformat(), "frames": len(fr), "complete": bool(d.get("complete_day")),
                    "layout": d.get("layout", "mosaic"), "width": d.get("width"),
                    "lat": None if lat is None else round(lat, 5), "lon": None if lon is None else round(lon, 5),
                    "url": "camera/" + "/".join(rel.parts) + "/latest.mp4"})
    return out


def _latest_heading(frame: pd.DataFrame, at: pd.Timestamp) -> float | None:
    """The ship's heading for the map glyph: a circular mean over the ten
    minutes up to the latest fix (a ship on station swings about), rounded;
    None when that stretch holds no heading at all, so the glyph is not drawn
    pointing somewhere made up."""
    if "Heading (°)" not in frame.columns:
        return None
    h = frame["Heading (°)"]
    h = h[(h.index > at - pd.Timedelta(minutes=10)) & (h.index <= at)].dropna()
    if h.empty:
        return None
    return round(_circular_mean_deg(h), 1)


def slice_window(a: Analysis, w: Window, end: pd.Timestamp) -> dict:
    df = a.frame[a.frame.index >= end - pd.Timedelta(hours=w.hours)]
    if df.empty:
        return {"label": w.label, "step_s": w.step_s, "n": 0, "t": [], "lat": [], "lon": [],
                "dist_km": [], "leg": [], "vars": {}, "limits": {}}

    rule = f"{w.step_s}s"
    agg: dict[str, object] = {"lat": "mean", "lon": "mean", "dist_km": "max", "leg": "first"}
    if "pump_low" in df.columns:
        agg["pump_low"] = "max"                     # one stopped minute marks the bin
    for v in VARIABLES:
        if v.name not in df.columns or v.name in WINDOW_FILLED:
            continue
        if v.name.startswith("Surprise"):
            agg[v.name] = "max"                     # keep spikes visible when binning
        elif v.circular:
            agg[v.name] = _circular_mean_deg
        else:
            agg[v.name] = "mean"
    g = df.resample(rule).agg(agg)
    # A bin is labelled by the mean time of its samples, not the grid edge, so
    # its mean position sits at the instant it represents (an hour bin labelled
    # at its start would put a mid-transit position half an hour early and
    # break the along-track spacing below).
    tmean = pd.Series(df.index.as_unit("ns").asi8.astype("float64"), index=df.index).resample(rule).mean()
    # However coarse the time step, the track keeps at least one point every
    # MAP_KM_STEP km along the way: the first raw record in each distance
    # bucket joins the time bins, so a transit does not thin to a dotted line
    # at the long spans.
    if w.step_s >= 600 and "dist_km" in df.columns:
        bucket = np.floor(df["dist_km"].ffill() / MAP_KM_STEP)
        extra = df.loc[bucket.diff().fillna(1) != 0, list(agg.keys())]
        extra = extra[~extra.index.isin(g.index)]
        if len(extra):
            g = pd.concat([g, extra]).sort_index()
    # An empty bin becomes a null, which breaks the plotted line. One null per
    # gap is enough for that, so long runs of empty bins (a port call, the
    # months between seasons) collapse to a single row rather than a grid.
    empty = g.drop(columns=["leg"]).isna().all(axis=1)
    keep = ~empty | (empty & ~empty.shift(fill_value=False))
    g = g[keep]
    tm = tmean.reindex(g.index)
    stamped = pd.to_datetime(tm.to_numpy(), unit="ns")
    if g.index.tz is not None:
        stamped = stamped.tz_localize("UTC").tz_convert(g.index.tz)
    g.index = pd.DatetimeIndex(stamped.where(tm.notna().to_numpy(), g.index), name=g.index.name)
    g = g[~g.index.duplicated(keep="first")].sort_index()
    g = _break_discontinuities(g)

    t0 = g.index.min()
    elapsed = (g.index - t0).total_seconds() / 3600.0
    dist = g["dist_km"].ffill()
    dist_rel = dist - dist.dropna().iloc[0] if dist.notna().any() else dist

    def col(s: pd.Series, nd=4) -> list:
        return [None if (x is None or not np.isfinite(x)) else round(float(x), nd) for x in s.to_numpy()]

    vars_out = {}
    for v in VARIABLES:
        if v.name == "Time elapsed (h)":
            vars_out[v.name] = [round(float(x), 3) for x in elapsed]
        elif v.name == "Distance travelled (km)":
            vars_out[v.name] = col(dist_rel, 3)
        elif v.name in g.columns:
            vars_out[v.name] = col(g[v.name], 4)

    # Colour scales and axes of the TSG variables are set by the bins with the
    # intake pump running: a stopped pump reads the stagnant line, not the sea.
    # A bin counts as stopped if any of its minutes was, and so do its two
    # neighbours: the line takes a while to flush once the pump is back on.
    low = None
    if "pump_low" in g.columns:
        raw_low = [bool(x >= 0.5) if np.isfinite(x) else None for x in g["pump_low"].to_numpy(dtype=float)]
        low = [bool(raw_low[i]) or bool(i > 0 and raw_low[i - 1]) or bool(i + 1 < len(raw_low) and raw_low[i + 1])
               for i in range(len(raw_low))]
    tsg_vars = {v.name for v in VARIABLES if v.tsg}

    def limits(name: str, vals: list) -> list | None:
        if low and name in tsg_vars:
            pumped = [x for x, l in zip(vals, low) if not l]
            if len(pumped) >= 2:
                vals = pumped
        return _limits(vals)

    return {
        "label": w.label, "step_s": w.step_s, "hours": w.hours, "n": int(len(g)),
        "start": t0.isoformat(), "end": g.index.max().isoformat(),
        "t": (g.index.as_unit("ms").asi8).tolist(),
        "lat": col(g["lat"], 6), "lon": col(g["lon"], 6),
        "dist_km": col(dist_rel, 3),
        "leg": [None if (x is None or not np.isfinite(x)) else int(x) for x in g["leg"].to_numpy()],
        "vars": vars_out,
        "pump_low": low,
        "limits": {name: limits(name, vals) for name, vals in vars_out.items()},
    }


def _break_discontinuities(g: pd.DataFrame) -> pd.DataFrame:
    """Insert a null row wherever consecutive positioned bins imply a speed no
    ship reaches. Such a jump is a discontinuity in the log, and drawing it
    would put a straight chord across the map."""
    from .derive import MAX_PLAUSIBLE_KN, haversine_km
    pos = g[["lat", "lon"]].dropna()
    if len(pos) < 2:
        return g
    la, lo = pos["lat"].to_numpy(), pos["lon"].to_numpy()
    dt_h = np.diff(pos.index.as_unit("s").asi8) / 3600.0
    with np.errstate(divide="ignore", invalid="ignore"):
        kn = haversine_km(la[:-1], lo[:-1], la[1:], lo[1:]) / dt_h / 1.852
    jumps = np.flatnonzero(kn > MAX_PLAUSIBLE_KN)
    if not len(jumps):
        return g
    # a null row one microsecond after the bin before each jump
    breaks = pd.DataFrame(np.nan, index=pos.index[jumps] + pd.Timedelta(microseconds=1), columns=g.columns)
    out = pd.concat([g, breaks]).sort_index()
    log.debug("inserted %d line breaks at position discontinuities", len(jumps))
    return out


def aggregate(a: Analysis, rule: str) -> dict:
    """Mean/min/max/count of every panel variable per time bin, with the mean
    position and the leg, over the whole record. Bins with no data are
    dropped, so the table is only as long as the data."""
    df = a.frame
    names = [v.name for v in VARIABLES if v.name in df.columns and v.name not in WINDOW_FILLED]
    names = [n for n in names if n in df.columns]
    g = df[names + ["lat", "lon", "leg"]].resample(rule)
    mean, mn, mx, cnt = g[names].mean(), g[names].min(), g[names].max(), g[names].count()
    circular = {v.name for v in VARIABLES if v.circular and v.name in names}
    for n in circular:
        mean[n] = g[n].agg(_circular_mean_deg)
    pos = g[["lat", "lon"]].mean()
    leg = g["leg"].first()
    keep = cnt.sum(axis=1) > 0
    rows = []
    for t in mean.index[keep]:
        row = {"t": int(t.value // 10**6),          # Timestamp.value is always ns -> ms
               "leg": None if pd.isna(leg[t]) else int(leg[t]),
               "lat": None if pd.isna(pos.at[t, "lat"]) else round(float(pos.at[t, "lat"]), 5),
               "lon": None if pd.isna(pos.at[t, "lon"]) else round(float(pos.at[t, "lon"]), 5)}
        for n in names:
            c = int(cnt.at[t, n])
            value = _num(mean.at[t, n])
            value = None if value is None else round(value, 4)
            if n in circular and value is not None:
                value %= 360                 # rounding must not produce 360°
            row[n] = None if c == 0 else [value, round(float(mn.at[t, n]), 4),
                                          round(float(mx.at[t, n]), 4), c]
        rows.append(row)
    return {"rule": rule, "variables": names, "columns": ["mean", "min", "max", "n"], "rows": rows}


def _limits(vals: list) -> list | None:
    arr = np.array([v for v in vals if v is not None], dtype=float)
    if arr.size == 0:
        return None
    lo, hi = np.quantile(arr, QUANTILE_LIMITS)
    if lo == hi:
        lo, hi = lo - 0.5, hi + 0.5
    return [float(lo), float(hi)]


# ---------------------------------------------------------------- build

def build(root: Path, title: str, links: list[dict]) -> dict:
    started = datetime.now(timezone.utc)
    legs = discover()
    if not legs:
        raise SystemExit("no legs found under the data roots")

    # 1. sync every leg's store, collect the union of columns
    stores: list[tuple[Leg, Store]] = []
    union_keys: dict[str, str] = {}
    for leg in legs:
        s = sync(leg.indirs, leg.db)
        log.info("%s sync: %d files, %d loaded", leg.id, s["files_total"], s["files_loaded"])
        st = Store(leg.db)
        if st.time_range() is None:
            st.close()
            continue
        union_keys.update(st.display_map())
        stores.append((leg, st))
    if not stores:
        raise SystemExit("all stores are empty")

    keys = list(union_keys)
    want, res, pos_pairs, feats = needed_keys(keys, union_keys)

    # 2. read the needed columns from each leg and concatenate in time order
    frames, coverage, files_total, latest_file, columns_seen = [], {}, 0, None, {}
    for i, (leg, st) in enumerate(stores):
        df = st.frame_columns(want)
        for k in want:
            if k not in df.columns:
                df[k] = np.nan
        df = df[want]
        df["leg"] = i
        frames.append(df)
        present = set(st.column_map())
        coverage[leg.id] = {r.variable.name: (r.variable.derived or (r.key in present)) for r in res}
        for d, f in st.conn.execute("SELECT display, first_seen FROM columns"):
            columns_seen.setdefault(d, f"{leg.id}:{f}")
        files_total += st.conn.execute("SELECT COUNT(*) FROM files").fetchone()[0]
        lf = st.conn.execute("SELECT MAX(name) FROM files").fetchone()[0]
        latest_file = max(filter(None, [latest_file, lf]))
        st.close()
    df = pd.concat(frames).sort_index()
    df = df[~df.index.duplicated(keep="last")]
    log.info("combined record: %d rows, %d legs, %s -> %s", len(df), len(stores),
             df.index.min().strftime("%Y-%m-%d"), df.index.max().strftime("%Y-%m-%d %H:%M"))

    # 3. derive, then slice every window
    leg_codes = df.pop("leg")
    from .tsg import minute_frame
    try:
        tsg = minute_frame([leg for leg, _ in stores])
    except Exception:                       # noqa: BLE001 — the TSG files are extra, never required
        log.exception("TSG files not read")
        tsg = None
    a = build_analysis(df, res, pos_pairs, feats, union_keys, tsg=tsg)
    a.frame["leg"] = leg_codes.reindex(a.frame.index).to_numpy()
    end = a.frame.index.max()

    root.mkdir(parents=True, exist_ok=True)
    (root / "data").mkdir(exist_ok=True)
    windows_meta = []
    for w in WINDOWS:
        payload = slice_window(a, w, end)
        fn = f"w-{w.label}.json"
        atomic_write(root / "data" / fn, json.dumps(payload, separators=(",", ":")))
        windows_meta.append({"label": w.label, "hours": w.hours, "step_s": w.step_s,
                             "file": f"data/{fn}", "n": payload["n"],
                             "start": payload.get("start"), "end": payload.get("end")})
        log.info("window %-4s %6d points", w.label, payload["n"])

    # time-aggregated tables for the data tab
    agg_meta = {}
    for label, rule in (("1h", "1h"), ("1d", "1D")):
        payload = aggregate(a, rule)
        atomic_write(root / "data" / f"agg-{label}.json", json.dumps(payload, separators=(",", ":")))
        agg_meta[label] = {"file": f"data/agg-{label}.json", "n": len(payload["rows"])}
        log.info("aggregate %-3s %6d rows", label, len(payload["rows"]))

    # casts and calendar are independent of the underway record; a failure in
    # either must not take the dashboard down
    from .casts import build_casts
    from .calendar import build_calendar
    try:
        casts_idx = build_casts([leg for leg, _ in stores], root)
    except Exception:                       # noqa: BLE001
        log.exception("cast build failed")
        casts_idx = {"casts": [], "variables": []}
    try:
        cal = build_calendar([leg for leg, _ in stores], root, frame=a.frame)
    except Exception:                       # noqa: BLE001
        log.exception("calendar build failed")
        cal = {}

    last = a.frame[["lat", "lon"]].dropna()
    latest = None
    if not last.empty:
        lt = last.index[-1]
        latest = {"time": lt.isoformat(), "lat": float(last["lat"].iloc[-1]), "lon": float(last["lon"].iloc[-1]),
                  "heading": _latest_heading(a.frame, lt)}

    stations = [s for leg, _ in stores for s in read_stations(leg.stations, leg.id)]
    try:
        cameras = camera_index(CAMERA_OUTPUT, a.frame)
    except Exception:                       # noqa: BLE001 — the timelapses are extra, never required
        log.exception("camera index failed")
        cameras = []
    manifest = {
        "cameras": cameras,
        "title": title, "version": __version__,
        "generated_utc": started.isoformat(timespec="seconds"),
        "local_tz": LOCAL_TZ, "default_window": DEFAULT_WINDOW,
        "windows": windows_meta,
        "legs": [dict(leg.meta(), index=i) for i, (leg, _) in enumerate(stores)],
        "live": next((leg.id for leg, _ in stores if leg.live), None),
        "variables": [{
            "name": r.variable.name, "unit": r.variable.unit, "derived": r.variable.derived,
            "log_ok": r.variable.log_ok, "circular": r.variable.circular, "tsg": r.variable.tsg, "cmap": r.variable.cmap,
            "resolved": r.resolved, "source": r.display,
            "coverage": {leg_id: cov[r.variable.name] for leg_id, cov in coverage.items()},
        } for r in res],
        "position_source": a.position_source,
        "surprise": {"features": [union_keys.get(f, f) for f in a.surprise_features], "note": a.surprise_note,
                     "scales": [list(x) for x in SURPRISE_SCALES], "alert": {"scale": SURPRISE_ALERT_SCALE, "level": SURPRISE_ALERT}},
        "stations": stations,
        "data_range": {"start": a.frame.index.min().isoformat(), "end": end.isoformat()},
        "latest": latest,
        "files": {"total": files_total, "latest": latest_file,
                  "inputs": sorted({str(p) for leg, _ in stores for p in leg.indirs})},
        "columns_seen": [{"display": d, "first_seen": f} for d, f in sorted(columns_seen.items(), key=lambda kv: kv[1])],
        "links": links,
        "aggregates": agg_meta,
        "casts": {"index": "data/casts/index.json", "n": len(casts_idx["casts"]), "variables": casts_idx["variables"]},
        "calendar": {"file": "data/calendar.json", **cal},
        "intranet": [{"label": l, "url": f"{INTRANET_BASE}/{path}"} for l, path in INTRANET_LINKS],
    }
    atomic_write(root / "data" / "manifest.json", json.dumps(manifest, indent=1))

    # 4. site shell
    static = root / "static"
    static.mkdir(exist_ok=True)
    for f in (PKG / "static").rglob("*"):
        if f.is_file():
            dest = static / f.relative_to(PKG / "static")
            dest.parent.mkdir(parents=True, exist_ok=True)
            if not dest.exists() or dest.stat().st_size != f.stat().st_size or dest.stat().st_mtime < f.stat().st_mtime:
                shutil.copy2(f, dest)
    geo_layers = sorted(p.name for p in (PKG / "static" / "geo").glob("*.geojson")) if (PKG / "static" / "geo").is_dir() else []
    # the marker sprite is referenced by a base URL that MapLibre completes with
    # .json/.png/@2x, so it cannot carry a query string: versioned copies of the
    # four files carry a content hash in their name instead (a browser must never
    # pair a week-cached sprite image with a fresh index)
    import hashlib
    sprite_src = sorted((PKG / "static" / "geo").glob("sprite*.*"))
    sprite_version = hashlib.sha1(b"".join(f.read_bytes() for f in sprite_src)).hexdigest()[:8] if sprite_src else ""
    for f in sprite_src:
        dest = static / "geo" / f.name.replace("sprite", f"sprite-{sprite_version}", 1)
        if not dest.exists():
            shutil.copy2(f, dest)
    # asset URLs carry a content hash so browsers pick up a new app.js/style.css
    # immediately instead of serving a heuristically cached one
    import hashlib
    h = hashlib.sha1()
    for name in ("data.js", "app.js", "tabs.js", "chat.js", "style.css"):
        h.update((PKG / "static" / name).read_bytes())
    # a raster tile pyramid (tools/make_gebco_tiles.sh) lives on local disk —
    # too many files for the share or the repository — and the server maps
    # /static/tiles/ onto it; it is used when present
    from .serve import TILES_DIR
    tiles = TILES_DIR / "gebco"
    raster = None
    if tiles.is_dir():
        zooms = sorted(int(p.name) for p in tiles.iterdir() if p.name.isdigit())
        if zooms:
            # tiles are cached for a week; the pyramid's own mtime versions the
            # URL so a re-render is picked up by browsers immediately
            v = int(tiles.stat().st_mtime)
            raster = {"url": f"static/tiles/gebco/{{z}}/{{x}}/{{y}}.png?v={v}", "minzoom": zooms[0], "maxzoom": zooms[-1],
                      "attribution": "GEBCO Compilation Group (2024) GEBCO 2024 Grid"}
    site = {"title": title, "links": links, "version": __version__, "local_tz": LOCAL_TZ,
            "intranet": [{"label": l, "url": f"{INTRANET_BASE}/{path}"} for l, path in INTRANET_LINKS],
            "default_window": DEFAULT_WINDOW, "geo_layers": geo_layers, "raster": raster, "low_flow_v": LOW_FLOW_V,
            "sprite": f"static/geo/sprite-{sprite_version}" if sprite_version else "static/geo/sprite",
            "asset_version": h.hexdigest()[:10],
            "plotly_version": str((PKG / "static" / "plotly.min.js").stat().st_size)}
    env = Environment(loader=FileSystemLoader(str(PKG / "templates")), autoescape=True)
    atomic_write(root / "index.html", env.get_template("index.html.j2").render(site=site, m=manifest))

    took = (datetime.now(timezone.utc) - started).total_seconds()
    unresolved = [r.variable.name for r in res if not r.resolved]
    log.info("built %s in %.1fs; unresolved: %s", root, took, unresolved or "none")
    return {"seconds": took, "unresolved": unresolved, "legs": len(stores),
            "windows": {w["label"]: w["n"] for w in windows_meta}}
