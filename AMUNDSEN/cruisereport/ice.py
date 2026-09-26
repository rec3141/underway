"""Sea-ice concentration at a position from the Canadian Ice Service charts.

The underway dashboard imports CIS regional charts as GeoJSON under
``<underway db>/ice-charts/<region>-<date>.geojson`` (read only). A position
takes the most recent chart valid on or before the time, within
``MAX_AGE_DAYS``; the answer carries that chart's date and region so the
report can say how old it is.
"""

from __future__ import annotations

import json
import logging
from functools import lru_cache
from pathlib import Path

import numpy as np
import pandas as pd
from matplotlib.path import Path as MplPath

from .config import UNDERWAY_DB_DIR

log = logging.getLogger(__name__)
MAX_AGE_DAYS = 8


def _dir() -> Path:
    return UNDERWAY_DB_DIR / "ice-charts"


@lru_cache(maxsize=32)
def _chart(path: str, _mtime: float) -> dict:
    data = json.loads(Path(path).read_text())
    polys = []
    for f in data.get("features", []):
        g = f.get("geometry") or {}
        rings = ([g["coordinates"]] if g.get("type") == "Polygon"
                 else g.get("coordinates", []) if g.get("type") == "MultiPolygon" else [])
        for poly in rings:
            if not poly:
                continue
            outer = MplPath(np.asarray(poly[0])[:, :2])
            holes = [MplPath(np.asarray(h)[:, :2]) for h in poly[1:]]
            polys.append((outer, holes, f.get("properties") or {}))
    return {"meta": data.get("chart", {}), "polys": polys}


def charts() -> list[tuple[pd.Timestamp, Path]]:
    out = []
    for p in _dir().glob("*.geojson") if _dir().is_dir() else []:
        try:
            out.append((pd.Timestamp(p.stem[-10:]), p))
        except ValueError:
            continue
    return sorted(out)


def at(lat: float | None, lon: float | None, when: str) -> dict | None:
    if lat is None or lon is None:
        return None
    t = pd.Timestamp(when).tz_localize(None).normalize()
    candidates = [(d, p) for d, p in charts() if d <= t and (t - d).days <= MAX_AGE_DAYS]
    for d, p in sorted(candidates, reverse=True):
        c = _chart(str(p), p.stat().st_mtime)
        for outer, holes, props in c["polys"]:
            if outer.contains_point((lon, lat)) and not any(h.contains_point((lon, lat)) for h in holes):
                return {
                    "chart_date": d.date().isoformat(),
                    "region": c["meta"].get("region"),
                    "age_days": (t - d).days,
                    "polygon_type": props.get("polygon_type"),
                    "concentration": props.get("concentration_label"),
                    "stage": props.get("stage_a"),
                    "attribution": c["meta"].get("attribution", "Canadian Ice Service"),
                }
    return None
