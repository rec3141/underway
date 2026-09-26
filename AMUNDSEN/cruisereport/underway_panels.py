"""The underway dashboard's panels, read for a report period.

The panel list and each panel's source column come from the dashboard's own
``dashboard.config``, so a panel added there shows up here, and
they are grouped by the rules the Underway tab uses (``panelGroup`` in its
``app.js``): Surprise, Lab, Met Station, Bridge, Winches, plus the camera's
Ice camera group.

Each panel is read from one of three places:

* ``record``  a measured variable, from the leg's ACSD store at 10-minute
  medians (vector means for directions); TSG variables are blanked while the
  intake pump was off, as on the dashboard
* ``hourly``  a variable the dashboard derives (surprise scores, excess heat,
  line warming, TSG flow, sea state, roll and pitch, rosette rate), from the
  hourly means it publishes in ``www/data/agg-1h.json``, so the numbers are
  the dashboard's own
* ``camera``  the ice camera's released products (``ice/ice.sqlite``): total
  concentration as a 30-minute centred mean, and the ice-type composition

"Time elapsed" and "Distance travelled" are left out: they are the
dashboard's axes, not measurements.
"""

from __future__ import annotations

import json
import re
import sqlite3
from functools import lru_cache

import numpy as np
import pandas as pd

from . import underway
from .config import UNDERWAY_DB_DIR


AGG = UNDERWAY_DB_DIR.parent / "www" / "data" / "agg-1h.json"
ICE_DB = UNDERWAY_DB_DIR.parent / "ice" / "ice.sqlite"

GROUPS = ["Surprise", "Lab", "Met Station", "Bridge", "Winches", "Ice camera", "Other"]
SKIP = {"Time elapsed (h)", "Distance travelled (km)"}
ICE_TYPES = ["grease ice", "nilas", "thin ice floe", "icy bits", "brash ice", "thick ice floe"]
CAMERA = [("Camera · concentration", "Ice concentration (camera)", "%"),
          ("Camera · ice composition", "Ice composition (camera)", "%")]
DEFAULT = ["SST (°C)", "Salinity (PSU)", "Fluorescence (µg/L)", "Air temperature (°C)",
           "Relative wind speed (kn)", "Bottom depth (m)"]


def _config():
    from dashboard import config
    return config


def group_of(v) -> str:
    """The Underway tab's grouping (app.js panelGroup), for the dashboard's variables."""
    name = v.name
    if name.startswith("Surprise"):
        return "Surprise"
    if re.match(r"^(Bottom depth|Rosette |Cable )", name):
        return "Winches"
    if re.match(r"^(Air temperature|Relative humidity|Atmospheric pressure|True wind direction|"
                r"Relative wind speed|Short-wave radiation)", name):
        return "Met Station"
    if v.tsg:
        return "Lab"
    if re.match(r"^(Sea state|Roll & pitch|Heading|Ship speed)", name):
        return "Bridge"
    return "Other"


def catalog() -> list[dict]:
    """Every panel, in the dashboard's group order."""
    out = []
    for v in _config().VARIABLES:
        if v.name in SKIP:
            continue
        out.append({"id": v.name, "label": v.name, "unit": v.unit, "group": group_of(v),
                    "source": "hourly" if v.derived else "record",
                    "circular": v.circular, "tsg": v.tsg, "reverse": v.reverse})
    out += [{"id": i, "label": label, "unit": unit, "group": "Ice camera", "source": "camera",
             "circular": False, "tsg": False, "reverse": False} for i, label, unit in CAMERA]
    order = {g: n for n, g in enumerate(GROUPS)}
    return sorted(out, key=lambda p: order.get(p["group"], 99))


def catalog_group(panel_id: str) -> str | None:
    return next((p["group"] for p in catalog() if p["id"] == panel_id), None)


def _vector_mean(deg: pd.Series) -> float:
    r = np.radians(deg.dropna())
    if not len(r):
        return np.nan
    return float(np.degrees(np.arctan2(np.sin(r).mean(), np.cos(r).mean())) % 360)


def _record(leg: str, t0: pd.Timestamp, t1: pd.Timestamp, panels: list[dict]) -> pd.DataFrame:
    p = underway.db_path(leg)
    if not p.is_file() or not panels:
        return pd.DataFrame()
    con = sqlite3.connect(f"file:{p}?mode=ro", uri=True)
    try:
        cols = dict(con.execute("SELECT key, col FROM columns").fetchall())
        by_name = {v.name: v for v in _config().VARIABLES}
        chosen = []
        for pn in panels:
            key = by_name[pn["id"]].resolve(list(cols))
            if key:
                chosen.append((pn, cols[key]))
        if not chosen:
            return pd.DataFrame()
        q = (f"SELECT t, {', '.join(c for _, c in chosen)} FROM obs WHERE t BETWEEN ? AND ? ORDER BY t")
        rows = con.execute(q, (int(t0.tz_localize("UTC").timestamp()),
                               int(t1.tz_localize("UTC").timestamp()))).fetchall()
    finally:
        con.close()
    if not rows:
        return pd.DataFrame()
    df = pd.DataFrame(rows, columns=["t"] + [pn["id"] for pn, _ in chosen])
    df["t"] = pd.to_datetime(df["t"], unit="s")
    df = df.set_index("t")
    out = {}
    for pn, _ in chosen:
        s = pd.to_numeric(df[pn["id"]], errors="coerce")
        r = s.resample("10min")
        out[pn["id"]] = r.apply(_vector_mean) if pn["circular"] else r.median()
    frame = pd.DataFrame(out)
    pump = underway.pump_mask(frame.index)
    for pn, _ in chosen:
        if pn["tsg"]:
            frame.loc[pump, pn["id"]] = np.nan
    return frame


@lru_cache(maxsize=2)
def _agg(_mtime: float) -> tuple[list[str], list[dict]]:
    d = json.loads(AGG.read_text())
    return d["variables"], d["rows"]


def _hourly(t0: pd.Timestamp, t1: pd.Timestamp, panels: list[dict]) -> pd.DataFrame:
    if not panels or not AGG.is_file():
        return pd.DataFrame()
    names, rows = _agg(AGG.stat().st_mtime)
    a, b = t0.tz_localize("UTC").timestamp() * 1000, t1.tz_localize("UTC").timestamp() * 1000
    keep = [r for r in rows if a <= r["t"] <= b]
    if not keep:
        return pd.DataFrame()
    idx = pd.to_datetime([r["t"] for r in keep], unit="ms")
    data = {}
    for pn in panels:
        if pn["id"] in names:
            data[pn["id"]] = [(r.get(pn["id"]) or [None])[0] for r in keep]
    return pd.DataFrame(data, index=idx).astype(float)


def _camera(t0: pd.Timestamp, t1: pd.Timestamp) -> pd.DataFrame:
    """Per photo: total ice % and the six type fractions; filtered water counts as 0%."""
    if not ICE_DB.is_file():
        return pd.DataFrame()
    con = sqlite3.connect(f"file:{ICE_DB}?mode=ro", uri=True)
    try:
        rows = con.execute(
            "SELECT t, status, ice, types FROM photos WHERE t BETWEEN ? AND ? AND ice IS NOT NULL "
            "ORDER BY t", (t0.tz_localize("UTC").timestamp(), t1.tz_localize("UTC").timestamp())).fetchall()
    finally:
        con.close()
    if not rows:
        return pd.DataFrame()
    recs = []
    for t, status, ice_pct, types in rows:
        try:
            tv = json.loads(types) if types else [0] * 6
        except ValueError:
            tv = [np.nan] * 6
        recs.append([t, ice_pct] + list(tv[:6]))
    df = pd.DataFrame(recs, columns=["t", "ice"] + ICE_TYPES)
    df["t"] = pd.to_datetime(df["t"], unit="s")
    return df.set_index("t").astype(float)


def read(leg: str, t0: pd.Timestamp, t1: pd.Timestamp, ids: list[str]) -> dict:
    """{'panels': [panel...], 'record', 'hourly', 'camera': frames} for the chosen panel ids."""
    wanted = set(ids)
    panels = [p for p in catalog() if p["id"] in wanted]       # the dashboard's order, not the click order
    return {
        "panels": panels,
        "record": _record(leg, t0, t1, [p for p in panels if p["source"] == "record"]),
        "hourly": _hourly(t0, t1, [p for p in panels if p["source"] == "hourly"]),
        "camera": _camera(t0, t1) if any(p["source"] == "camera" for p in panels) else pd.DataFrame(),
    }
