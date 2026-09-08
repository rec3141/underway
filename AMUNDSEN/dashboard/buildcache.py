"""What a build keeps for the next one, under ``cache/build/``.

The build runs every minute over a record of which only the live leg
changes, so three things are kept between runs:

* each leg's frame, pickled beside a key made of its store's size, mtime
  and the wanted columns — read back instead of the store while the key holds;
* the surprise scores for the minutes before the live leg, with the scaling
  statistics of the full record they came from — a run then scores only
  the live leg, from a month's warm-up, scaled exactly as the whole was;
* the long windows' manifest entries — a window with bins of 15 minutes or
  more is re-sliced every ten minutes and its file kept in between.

Every read falls back to the full work when a key does not hold or a file
is unreadable; a failed write only costs the next run its shortcut.
"""

from __future__ import annotations

import hashlib
import json
import logging
import os
import pickle
from datetime import datetime
from pathlib import Path
from typing import Callable

import pandas as pd

log = logging.getLogger(__name__)

CACHE_DIR = Path(os.environ.get("UNDERWAY_CACHE_DIR", str(Path(__file__).resolve().parents[1] / "cache"))) / "build"
SURPRISE_WARMUP = pd.Timedelta(days=30)     # the longest half-life is two days: a month leaves nothing of what came before
LONG_STEP_S = 900                           # a window binned this coarsely is re-sliced every LONG_EVERY_MIN minutes
LONG_EVERY_MIN = 10


def file_signature(path: Path) -> str:
    st = path.stat()
    return f"{st.st_size}:{st.st_mtime_ns}"


def _pickle_atomic(path: Path, obj) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("wb") as f:
        pickle.dump(obj, f, protocol=pickle.HIGHEST_PROTOCOL)
    os.replace(tmp, path)


def cached_frame(leg_id: str, db_path: Path, want: list[str], read: Callable[[], pd.DataFrame]) -> pd.DataFrame:
    """The leg's frame of the wanted columns: the pickle kept from the last
    build while the store and the columns are the ones it was read from,
    otherwise ``read()``, kept for the next build."""
    key = f"{file_signature(db_path)}|{hashlib.sha1(chr(10).join(want).encode()).hexdigest()[:12]}"
    p = CACHE_DIR / "frames" / f"{leg_id}.pkl"
    k = p.with_suffix(".key")
    try:
        if k.read_text() == key:
            return pd.read_pickle(p)
    except (OSError, ValueError, EOFError, pickle.UnpicklingError, AttributeError):
        pass
    df = read()
    try:
        _pickle_atomic(p, df)
        k.write_text(key)
    except OSError as e:
        log.warning("frame cache for %s not written: %s", leg_id, e)
    return df


def surprise_cached(minute: pd.DataFrame, cfg, cutoff: pd.Timestamp, key: str, score) -> pd.DataFrame | None:
    """Surprise scores for the whole record. While the record before
    ``cutoff`` (the live leg's first minute) is the one the kept scoring
    saw (``key``), the minutes before it come from that scoring and the
    rest are scored afresh from SURPRISE_WARMUP before it, with the kept
    scaling statistics; otherwise the record is scored whole, and kept.
    ``score`` is ``surprise.score_minutes``."""
    p = CACHE_DIR / "surprise.pkl"
    try:
        with p.open("rb") as f:
            c = pickle.load(f)
        if c["key"] == key and c["cutoff"] == cutoff and c["cfg"] == repr(cfg):
            new, _ = score(minute[minute.index >= cutoff - SURPRISE_WARMUP], cfg, stats=c["stats"])
            if new is not None:
                old = c["scores"]
                return pd.concat([old[old.index < cutoff], new[new.index >= cutoff]])
    except (OSError, KeyError, ValueError, EOFError, pickle.UnpicklingError, AttributeError):
        pass
    full, stats = score(minute, cfg)
    if full is not None and stats is not None:
        try:
            _pickle_atomic(p, {"key": key, "cutoff": cutoff, "cfg": repr(cfg), "stats": stats, "scores": full})
        except OSError as e:
            log.warning("surprise cache not written: %s", e)
    return full


def kept_window(label: str, step_s: int, path: Path, now: datetime) -> dict | None:
    """The manifest entry a long window keeps from the last build, when it
    is not yet due for re-slicing and its file is still there."""
    if step_s < LONG_STEP_S or now.minute % LONG_EVERY_MIN == 0 or not path.is_file():
        return None
    try:
        metas = json.loads((CACHE_DIR / "windows.json").read_text())
    except (OSError, ValueError):
        return None
    m = metas.get(label)
    return m if m and m.get("file") == f"data/{path.name}" else None


def remember_windows(metas: list[dict]) -> None:
    try:
        CACHE_DIR.mkdir(parents=True, exist_ok=True)
        tmp = CACHE_DIR / "windows.json.tmp"
        tmp.write_text(json.dumps({m["label"]: m for m in metas}))
        os.replace(tmp, CACHE_DIR / "windows.json")
    except OSError as e:
        log.warning("window cache not written: %s", e)
