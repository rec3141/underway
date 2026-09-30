"""The ship's continuous record at its arrival on station, from the underway stores.

The dashboard keeps one SQLite store per leg (``<underway db>/<leg>.db``,
10-second ACSD rows in ``obs``, columns named in ``columns``). Conditions
at a station are taken at the ship's arrival there (``arrival``: when its
speed over ground last fell below ``STATION_SOG_KN`` before the first
deployment) as the mean over ``ARRIVAL_MEAN_S`` from then (``at``). The
stores are opened read-only.

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

from .config import ARRIVAL_MEAN_S, SEA_STATE_WINDOW_S, UNDERWAY_DB_DIR

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
    Var("lat", "posmv — latitude (deg n)", "Latitude", "°N"),
    Var("lon", "posmv — longitude (deg e)", "Longitude", "°E"),
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
    Var("heave_m", "posmv — heave (m)", "Heave", "m"),
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


def _epoch(when) -> int:
    t = pd.Timestamp(when)
    return int((t.tz_localize("UTC") if t.tzinfo is None else t).timestamp())


def _rows(leg: str, t0: int, t1: int, keys: list[str] | None = None):
    """(epoch seconds, values, vars) of the store between t0 and t1, or None."""
    p = db_path(leg)
    if not p.is_file():
        return None
    cols = _columns(str(p))
    wanted = [(v, cols[v.source]) for v in VARS if v.source in cols and (keys is None or v.key in keys)]
    if not wanted:
        return None
    con = sqlite3.connect(f"file:{p}?mode=ro", uri=True)
    try:
        rows = con.execute(
            f"SELECT t, {', '.join(c for _, c in wanted)} FROM obs WHERE t BETWEEN ? AND ? ORDER BY t",
            (t0, t1)).fetchall()
    finally:
        con.close()
    if not rows:
        return None
    arr = np.array(rows, dtype=float)
    return arr[:, 0], arr[:, 1:], [v for v, _ in wanted]


def _circ_mean(deg: np.ndarray) -> float | None:
    deg = deg[np.isfinite(deg)]
    if not len(deg):
        return None
    r = np.radians(deg)
    return float(np.degrees(np.arctan2(np.sin(r).mean(), np.cos(r).mean())) % 360)


def arrival(leg: str, first_start: str, search_h: float = 6) -> tuple[str, str]:
    """When the ship came onto station before ``first_start`` (the visit's first
    deployment): just after the last moment within ``search_h`` hours that its
    speed over ground was at or above STATION_SOG_KN. (ISO time, how found);
    the deployment itself when the ship was still moving then (a trawl), was
    already stopped for the whole search, or the record has no speed."""
    t1 = _epoch(first_start)
    got = _rows(leg, t1 - int(search_h * 3600), t1, ["sog_kn"])
    if got is None:
        return first_start, "first deployment (no speed record)"
    t, v, _ = got
    sog = v[:, 0]
    ok = np.isfinite(sog)
    t, sog = t[ok], sog[ok]
    moving = np.nonzero(sog >= STATION_SOG_KN)[0]
    if not len(sog) or not len(moving):
        return first_start, "first deployment"
    i = moving[-1]
    if i == len(sog) - 1:
        return first_start, "first deployment (ship under way)"
    when = pd.Timestamp(t[i + 1], unit="s").isoformat()
    return when, "ship slowed below 1 kn"


def at(leg: str, when: str, seconds: int = ARRIVAL_MEAN_S) -> dict:
    """Means over ``seconds`` from ``when`` (directions as circular means);
    missing values are None, never guessed. Sea state is 4σ of heave over
    SEA_STATE_WINDOW_S centred on ``when``."""
    t0 = _epoch(when)
    got = _rows(leg, t0, t0 + seconds)
    if got is None:
        return {"n": 0} if db_path(leg).is_file() else {}
    _, arr, wanted = got
    out: dict = {"n": len(arr)}
    for i, v in enumerate(wanted):
        col = arr[:, i]
        if v.circular:
            out[v.key] = _circ_mean(col)
        else:
            col = col[np.isfinite(col)]
            out[v.key] = float(col.mean()) if len(col) else None
    out.pop("heave_m", None)
    sog = out.get("sog_kn")
    out["true_wind_kn"] = out.get("rel_wind_kn") if sog is not None and sog < STATION_SOG_KN else None
    out["tsg_pump_off"] = pump_off(when)
    half = SEA_STATE_WINDOW_S // 2
    heave = _rows(leg, t0 - half, t0 + half, ["heave_m"])
    h = heave[1][:, 0] if heave else np.array([])
    h = h[np.isfinite(h)]
    out["sea_state_m"] = float(4 * h.std(ddof=1)) if len(h) >= 12 else None
    return out


def camera_ice(when: str, seconds: int = ARRIVAL_MEAN_S, reach_s: int = 600) -> dict | None:
    """The ice camera's products at ``when``: its photos within ``seconds``
    from then, else the nearest within ``reach_s``. {pct, types: [(type, %)],
    n, offset_s} or None when the camera saw nothing near then."""
    from . import underway_panels as UP

    t = pd.Timestamp(when)
    t = t.tz_convert("UTC").tz_localize(None) if t.tzinfo else t
    df = UP._camera(t - pd.Timedelta(seconds=reach_s), t + pd.Timedelta(seconds=reach_s))
    if df.empty:
        return None
    win = df[(df.index >= t) & (df.index <= t + pd.Timedelta(seconds=seconds))]
    offset = 0
    if win.empty:
        near = abs((df.index - t).total_seconds())
        win = df.iloc[[int(np.argmin(near))]]
        offset = int(round((win.index[0] - t).total_seconds()))
    means = win.mean()
    types = sorted(((k, float(means[k])) for k in UP.ICE_TYPES if np.isfinite(means[k]) and means[k] > 0),
                   key=lambda x: -x[1])
    return {"pct": float(means["ice"]), "types": types, "n": len(win), "offset_s": offset}


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
