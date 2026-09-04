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
from .config import DEFAULT_MINIMISED, DEFAULT_WINDOW, LOCAL_TZ, MAP_KM_STEP, QUANTILE_LIMITS, VARIABLES, WINDOWS, Window
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
    r = np.radians(x.dropna().to_numpy())
    if r.size == 0:
        return np.nan
    return float(np.degrees(np.arctan2(np.sin(r).mean(), np.cos(r).mean())) % 360)


def slice_window(a: Analysis, w: Window, end: pd.Timestamp) -> dict:
    df = a.frame[a.frame.index >= end - pd.Timedelta(hours=w.hours)]
    if df.empty:
        return {"label": w.label, "step_s": w.step_s, "n": 0, "t": [], "lat": [], "lon": [],
                "dist_km": [], "leg": [], "vars": {}, "limits": {}}

    rule = f"{w.step_s}s"
    agg: dict[str, object] = {"lat": "mean", "lon": "mean", "dist_km": "max", "leg": "first"}
    for v in VARIABLES:
        if v.name not in df.columns or (v.derived and not v.name.startswith("Surprise")):
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

    return {
        "label": w.label, "step_s": w.step_s, "hours": w.hours, "n": int(len(g)),
        "start": t0.isoformat(), "end": g.index.max().isoformat(),
        "t": (g.index.as_unit("ms").asi8).tolist(),
        "lat": col(g["lat"], 6), "lon": col(g["lon"], 6),
        "dist_km": col(dist_rel, 3),
        "leg": [None if (x is None or not np.isfinite(x)) else int(x) for x in g["leg"].to_numpy()],
        "vars": vars_out,
        "limits": {name: _limits(vals) for name, vals in vars_out.items()},
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
    names = [v.name for v in VARIABLES if v.name in df.columns and (not v.derived or v.name.startswith("Surprise"))]
    names = [n for n in names if n in df.columns]
    g = df[names + ["lat", "lon", "leg"]].resample(rule)
    mean, mn, mx, cnt = g[names].mean(), g[names].min(), g[names].max(), g[names].count()
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
            row[n] = None if c == 0 else [round(float(mean.at[t, n]), 4), round(float(mn.at[t, n]), 4),
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
    a = build_analysis(df, res, pos_pairs, feats, union_keys)
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
        cal = build_calendar([leg for leg, _ in stores], root)
    except Exception:                       # noqa: BLE001
        log.exception("calendar build failed")
        cal = {}

    last = a.frame[["lat", "lon"]].dropna()
    latest = None
    if not last.empty:
        lt = last.index[-1]
        latest = {"time": lt.isoformat(), "lat": float(last["lat"].iloc[-1]), "lon": float(last["lon"].iloc[-1])}

    stations = [s for leg, _ in stores for s in read_stations(leg.stations, leg.id)]
    manifest = {
        "title": title, "version": __version__,
        "generated_utc": started.isoformat(timespec="seconds"),
        "local_tz": LOCAL_TZ, "default_window": DEFAULT_WINDOW,
        "windows": windows_meta,
        "legs": [dict(leg.meta(), index=i) for i, (leg, _) in enumerate(stores)],
        "live": next((leg.id for leg, _ in stores if leg.live), None),
        "variables": [{
            "name": r.variable.name, "unit": r.variable.unit, "derived": r.variable.derived,
            "log_ok": r.variable.log_ok, "circular": r.variable.circular, "cmap": r.variable.cmap,
            "resolved": r.resolved, "source": r.display,
            "coverage": {leg_id: cov[r.variable.name] for leg_id, cov in coverage.items()},
        } for r in res],
        "position_source": a.position_source,
        "surprise": {"features": [union_keys.get(f, f) for f in a.surprise_features], "note": a.surprise_note},
        "default_minimised": list(DEFAULT_MINIMISED),
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
    # asset URLs carry a content hash so browsers pick up a new app.js/style.css
    # immediately instead of serving a heuristically cached one
    import hashlib
    h = hashlib.sha1()
    for name in ("app.js", "tabs.js", "style.css"):
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
            "default_window": DEFAULT_WINDOW, "geo_layers": geo_layers, "raster": raster,
            "asset_version": h.hexdigest()[:10],
            "plotly_version": str((PKG / "static" / "plotly.min.js").stat().st_size)}
    env = Environment(loader=FileSystemLoader(str(PKG / "templates")), autoescape=True)
    atomic_write(root / "index.html", env.get_template("index.html.j2").render(site=site, m=manifest))

    took = (datetime.now(timezone.utc) - started).total_seconds()
    unresolved = [r.variable.name for r in res if not r.resolved]
    log.info("built %s in %.1fs; unresolved: %s", root, took, unresolved or "none")
    return {"seconds": took, "unresolved": unresolved, "legs": len(stores),
            "windows": {w["label"]: w["n"] for w in windows_meta}}
