"""The TSG's own 1 Hz files: ``Data/TSG/<leg>/tsg_convdata_YYYYMMDD.cnv``.

They carry what the ACSD feed leaves out — above all the intake water flow
(a sensor voltage, ``Debit``; about 1.5 V pumping, near 0.06 V stopped) —
plus conductivity and both temperatures. The channel list grew over the
season (12, 13, then 14 columns as analogue channels were added), so the
columns are named from both ends: the fixed head, then whatever analogue
channels fit, then always ``flow`` and the second sound velocity. Each file
is reduced to minute means and cached per leg in ``db/tsg/<leg>.pkl`` keyed
by file size, mtime and parser version, so only the growing current-day
file is re-read.
"""

from __future__ import annotations

import io
import logging
import pickle
from pathlib import Path

import pandas as pd

from .config import DATA_ROOT, DB_DIR

log = logging.getLogger(__name__)
HEAD = ["t", "t_gps", "lat", "lon", "t_sbe45", "cond", "sal", "vel_a", "t_sbe38"]
ANALOGUE = ["fluo", "oxy", "cdom"]      # optional, in the order they were added
TAIL = ["flow", "vel_b"]
KEEP = ["flow", "cond", "t_sbe45", "t_sbe38", "sal"]
FLOW_RANGE = (0.0, 10.0)                # the flow sensor is a voltage; anything else is a misread column
CACHE_VERSION = 2


def columns_for(n: int) -> list[str]:
    """Column names for an ``n``-field row: head, then the analogue channels
    that fit, then the two tail columns; too short a row keeps only the head."""
    if n < len(HEAD) + len(TAIL):
        return HEAD[:n]
    k = n - len(HEAD) - len(TAIL)
    mid = ANALOGUE[:k] + [f"x{i}" for i in range(len(ANALOGUE), k)]
    return HEAD + mid + TAIL


def parse_conv(path: Path) -> pd.DataFrame:
    text = path.read_bytes().decode("latin-1")            # one read: the share is CIFS
    df = pd.read_csv(io.StringIO(text), sep=";", header=None, engine="c", on_bad_lines="skip", dtype=str)
    df.columns = columns_for(df.shape[1])
    t = pd.to_datetime(df["t"].str.strip(), format="%Y/%m/%d %H:%M:%S", errors="coerce", utc=True)
    out = df[[c for c in KEEP if c in df.columns]].apply(pd.to_numeric, errors="coerce")
    if "flow" in out.columns:
        # a day whose column count changed mid-file pads the short rows on the
        # right, which puts a sound velocity where the flow belongs
        out.loc[~out["flow"].between(*FLOW_RANGE), "flow"] = float("nan")
    out.index = t
    out = out[out.index.notna()]
    return out.resample("1min").mean()


def minute_frame(legs) -> pd.DataFrame | None:
    """Minute means of the TSG files of every leg, UTC index; None when there are none."""
    frames = []
    for leg in legs:
        d = DATA_ROOT / "TSG" / leg.id
        if not d.is_dir():
            continue
        cache_p = DB_DIR / "tsg" / f"{leg.id}.pkl"
        cache = pickle.loads(cache_p.read_bytes()) if cache_p.is_file() else {}
        seen, changed = set(), 0
        for p in sorted(d.glob("tsg_convdata_*.cnv")):
            st = p.stat()
            key = p.name; seen.add(key)
            ent = cache.get(key)
            if ent and ent.get("v") == CACHE_VERSION and ent["size"] == st.st_size and abs(ent["mtime"] - st.st_mtime) < 2:
                continue
            try:
                cache[key] = {"v": CACHE_VERSION, "size": st.st_size, "mtime": st.st_mtime, "frame": parse_conv(p)}
                changed += 1
            except Exception as e:                        # noqa: BLE001 — a bad day file must not stop the build
                log.warning("%s: cannot read %s (%s)", leg.id, p.name, e)
        if changed:
            cache_p.parent.mkdir(parents=True, exist_ok=True)
            cache_p.write_bytes(pickle.dumps(cache))
            log.info("%s: TSG minute means refreshed from %d file(s)", leg.id, changed)
        frames += [v["frame"] for k, v in cache.items() if k in seen]
    if not frames:
        return None
    f = pd.concat(frames).sort_index()
    return f[~f.index.duplicated(keep="last")]
