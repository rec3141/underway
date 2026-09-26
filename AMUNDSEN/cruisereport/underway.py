"""The ship's continuous record around an event, from the underway stores.

The dashboard keeps one SQLite store per leg (``<underway db>/<leg>.db``,
10-second ACSD rows in ``obs``, columns named in ``columns``). A reading at
an event is the median over ``UNDERWAY_HALF_WINDOW_S`` either side, which
rides over single bad scans. The stores are opened read-only.

True wind direction is AVOS's own; it agrees with the bridge log, while
heading plus relative direction does not (the anemometer carries a mounting
offset the files do not record). The files hold no true wind speed, so it is
given only while the ship is holding station (under ``STATION_SOG_KN``),
where relative and true wind are the same to within the ship's drift.

TSG readings taken while the intake pump was off or restricted are flagged,
not removed: the dashboard publishes those episodes as "TSG pump" events in
its calendar.
"""

from __future__ import annotations

import json
import logging
import sqlite3
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path

import numpy as np
import pandas as pd

from .config import UNDERWAY_DB_DIR, UNDERWAY_HALF_WINDOW_S

log = logging.getLogger(__name__)


@dataclass(frozen=True)
class Var:
    key: str            # our key
    source: str         # canonical key in the store's `columns` table
    label: str
    unit: str
    tsg: bool = False
    circular: bool = False


VARS = [
    Var("mb_depth_m", "multibeam — bottom depth (m)", "Bottom depth (multibeam)", "m"),
    Var("ek60_depth_m", "ek60 — bottom depth (m)", "Bottom depth (EK60)", "m"),
    Var("sog_kn", "posmv — speed (knt)", "Ship speed over ground", "kn"),
    Var("heading_deg", "posmv — heading (deg)", "Ship heading", "°", circular=True),
    Var("cog_deg", "posmv — track (deg)", "Ship course over ground", "°", circular=True),
    Var("rel_wind_kn", "avos — relative wind speed (knt)", "Relative wind speed", "kn"),
    Var("true_wind_dir_deg", "avos — true wind direction (deg)", "True wind direction (from)", "°",
        circular=True),
    Var("air_c", "avos — air temperature (deg c)", "Air temperature", "°C"),
    Var("pressure_hpa", "avos — atmospheric pressure (hpa)", "Atmospheric pressure", "hPa"),
    Var("humidity_pct", "avos — air humidity (%)", "Relative humidity", "%"),
    Var("visibility_m", "ats_cs125 — visibility distance (m)", "Visibility", "m"),
    Var("precip_mm_h", "ats_cs125 — intensity range (mm/hr)", "Precipitation intensity", "mm/h"),
    Var("sw_wm2", "ats_starboard — short wave radiation (w/m2)", "Shortwave radiation", "W/m²"),
    Var("sst_c", "tsg — hull temperature (deg c)", "Sea-surface temperature (TSG intake)", "°C",
        tsg=True),
    Var("sss", "tsg — salinity (psu)", "Sea-surface salinity (TSG)", "PSU", tsg=True),
    Var("fluo_ugl", "tsg — fluorescence (ug/l)", "Surface fluorescence (TSG)", "µg/L", tsg=True),
    Var("o2_mll", "tsg — oxygene (ml/l)", "Surface oxygen (TSG)", "mL/L", tsg=True),
    Var("cdom", "tsg — ecocdom (mg/m3)", "Surface CDOM (TSG)", "mg/m³", tsg=True),
]
BY_KEY = {v.key: v for v in VARS}
STATION_SOG_KN = 1.0


def db_path(leg: str) -> Path:
    return UNDERWAY_DB_DIR / f"{leg}.db"


@lru_cache(maxsize=16)
def _columns(path: str) -> dict[str, str]:
    con = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    try:
        return {key: col for col, key in con.execute("SELECT col, key FROM columns")}
    finally:
        con.close()


def _circ_median(deg: np.ndarray) -> float | None:
    """Mean direction of a short window (a median is not defined on a circle)."""
    deg = deg[np.isfinite(deg)]
    if not len(deg):
        return None
    r = np.radians(deg)
    return float(np.degrees(np.arctan2(np.sin(r).mean(), np.cos(r).mean())) % 360)


def around(leg: str, when: str, half: int = UNDERWAY_HALF_WINDOW_S) -> dict:
    """Window readings at an ISO UTC time; missing values are None, never guessed."""
    p = db_path(leg)
    if not p.is_file():
        return {}
    cols = _columns(str(p))
    wanted = [(v, cols[v.source]) for v in VARS if v.source in cols]
    if not wanted:
        return {}
    t = int(pd.Timestamp(when).tz_localize("UTC").timestamp()) if pd.Timestamp(when).tzinfo is None \
        else int(pd.Timestamp(when).timestamp())
    con = sqlite3.connect(f"file:{p}?mode=ro", uri=True)
    try:
        rows = con.execute(
            f"SELECT {', '.join(c for _, c in wanted)} FROM obs WHERE t BETWEEN ? AND ?",
            (t - half, t + half)).fetchall()
    finally:
        con.close()
    if not rows:
        return {"n": 0}
    arr = np.array(rows, dtype=float)
    out: dict = {"n": len(rows)}
    for i, (v, _) in enumerate(wanted):
        col = arr[:, i]
        if v.circular:
            out[v.key] = _circ_median(col)
        else:
            col = col[np.isfinite(col)]
            out[v.key] = float(np.median(col)) if len(col) else None
    sog = out.get("sog_kn")
    out["true_wind_kn"] = out.get("rel_wind_kn") if sog is not None and sog < STATION_SOG_KN else None
    out["tsg_pump_off"] = pump_off(when)
    return out


def _calendar_files() -> list[Path]:
    www = UNDERWAY_DB_DIR.parent / "www" / "data"
    return [p for p in (www / "calendar.json", www / "calendar-archive.json") if p.is_file()]


@lru_cache(maxsize=2)
def _pump_episodes(stamp: tuple) -> tuple[tuple[pd.Timestamp, pd.Timestamp], ...]:
    eps = []
    for name, _ in stamp:
        try:
            data = json.loads(Path(name).read_text())
        except Exception as e:
            log.warning("%s: %s", name, e)
            continue
        events = data.get("events", data) if isinstance(data, dict) else data
        for e in events if isinstance(events, list) else []:
            if isinstance(e, dict) and e.get("activity") == "TSG pump":
                try:
                    eps.append((pd.Timestamp(e["time_utc"]).tz_localize(None),
                                pd.Timestamp(e["end_utc"]).tz_localize(None)))
                except (KeyError, ValueError, TypeError):
                    continue
    return tuple(eps)


def _episodes():
    return _pump_episodes(tuple((str(p), p.stat().st_mtime) for p in _calendar_files()))


def pump_off(when: str) -> bool:
    t = pd.Timestamp(when).tz_localize(None)
    return any(a <= t <= b for a, b in _episodes())


def pump_mask(index: pd.DatetimeIndex) -> np.ndarray:
    """True where a time falls in a pump-off episode."""
    mask = np.zeros(len(index), dtype=bool)
    for a, b in _episodes():
        mask |= (index >= a) & (index <= b)
    return mask
