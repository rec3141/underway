"""CTD casts: the leg's CTD logbook and the underway dashboard's parsed casts.

The logbook (``Data/Rosette/<leg>/Logs/<yyyy>_<nn>_CTD_logbook.csv``) maps
the rosette console's cast number to the event label, station and cast type.
The dashboard's cast cache (``<underway db>/casts/<leg>/CTD_###.json``)
holds each binned downcast profile and the bottle-file values at each trip;
it is read, never written.
"""

from __future__ import annotations

import json
import logging
import math
from functools import lru_cache
from pathlib import Path

import pandas as pd

from .config import DATA_ROOT, UNDERWAY_DB_DIR

log = logging.getLogger(__name__)


def _logbook_path(leg: str) -> Path | None:
    d = DATA_ROOT / "Rosette" / leg / "Logs"
    hits = sorted(d.glob("*_CTD_logbook.csv")) if d.is_dir() else []
    return hits[0] if hits else None


def logbook(leg: str) -> list[dict]:
    p = _logbook_path(leg)
    return list(_logbook(str(p), p.stat().st_mtime)) if p else []


@lru_cache(maxsize=8)
def _logbook(path: str, _mtime: float) -> tuple[dict, ...]:
    df = pd.read_csv(path, encoding="latin1", sep=None, engine="python")
    df.columns = [str(c).strip() for c in df.columns]
    out = []
    for r in df.to_dict("records"):
        r = {k: (None if isinstance(v, float) and math.isnan(v) else v) for k, v in r.items()}
        try:
            r["cast"] = int(r["cast"])
        except (TypeError, ValueError, KeyError):
            continue
        out.append(r)
    return tuple(out)


def label_for_cast(leg: str) -> dict[int, str]:
    return {r["cast"]: r["label"] for r in logbook(leg) if r.get("label")}


def cast_cache(leg: str) -> dict[str, dict[str, dict]]:
    """Parsed casts: event label -> kind (CTD, TM, LADCP, MVP) -> cast.

    A rosette cast and its lowered ADCP share the event label.
    """
    d = UNDERWAY_DB_DIR / "casts" / leg
    files = sorted(d.glob("*.json")) if d.is_dir() else []
    stamp = tuple((p.name, p.stat().st_mtime) for p in files)
    return _cast_cache(str(d), stamp)


@lru_cache(maxsize=4)
def _cast_cache(d: str, stamp: tuple) -> dict[str, dict[str, dict]]:
    out = {}
    for name, _ in stamp:
        try:
            c = json.loads((Path(d) / name).read_text())["cast"]
        except Exception as e:
            log.warning("%s/%s: %s", d, name, e)
            continue
        if c.get("label"):
            out.setdefault(c["label"], {})[c.get("kind") or "CTD"] = c
    return out
