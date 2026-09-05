"""The TSG's own 1 Hz files: ``Data/TSG/<leg>/tsg_convdata_YYYYMMDD.cnv``.

They carry what the ACSD feed leaves out — above all the intake water flow
(a sensor voltage, ``Debit``; about 1.5 V pumping, near 0.06 V stopped) —
plus conductivity and both temperatures. The files have no header and the
channel list grew over the season: fluorescence only until 2026-08-21, then
oxygen and CDOM (the ACSD feed took those up on 2026-08-25). In the files
from 2026-08-21 to 2026-08-24 the two new channels share one field with no
``;`` between them, so every line is split on whitespace rather than on the
delimiter and the columns are named from both ends: the fixed head, then the
analogue channels that fit, then always ``flow`` and the second sound
velocity. A flow value is kept only if it is a plausible voltage and the
sound velocity beside it is a plausible sound velocity, which is what pins
the tail down. Each file is reduced to minute means and cached per leg in
``db/tsg/<leg>.pkl`` keyed by file size, mtime and parser version, so only
the growing current-day file is re-read.
"""

from __future__ import annotations

import io
import logging
import pickle
from pathlib import Path

import numpy as np
import pandas as pd

from .config import DATA_ROOT, DB_DIR

log = logging.getLogger(__name__)
HEAD = ["date", "time", "t_gps", "lat", "lon", "t_sbe45", "cond", "sal", "vel_a", "t_sbe38"]
ANALOGUE = ["fluo", "oxy", "cdom"]      # optional, in the order they were added
TAIL = ["flow", "vel_b"]
KEEP = ["flow", "cond", "t_sbe45", "t_sbe38", "sal"]
FLOW_RANGE = (0.0, 10.0)                # the flow sensor is a voltage
VEL_RANGE = (1350.0, 1650.0)            # sound speed in seawater, m/s
CACHE_VERSION = 3


def columns_for(n: int) -> list[str]:
    """Column names for a line of ``n`` whitespace-separated tokens: head, then
    the analogue channels that fit, then the two tail columns; too short a
    line keeps only the head (and so no flow)."""
    if n < len(HEAD) + len(TAIL):
        return HEAD[:n]
    k = n - len(HEAD) - len(TAIL)
    mid = ANALOGUE[:k] + [f"x{i}" for i in range(len(ANALOGUE), k)]
    return HEAD + mid + TAIL


def parse_conv(path: Path) -> pd.DataFrame:
    text = path.read_bytes().decode("latin-1")            # one read: the share is CIFS
    lines = pd.Series(text.replace(";", " ").splitlines())
    lines = lines[lines.str.strip() != ""]
    ntok = lines.str.count(r"\S+")
    parts = []
    for n, chunk in lines.groupby(ntok):                  # a file whose channel count changed mid-day
        cols = columns_for(int(n))
        df = pd.read_csv(io.StringIO("\n".join(chunk)), sep=r"\s+", header=None, names=cols, dtype=str,
                         engine="c", on_bad_lines="skip")
        t = pd.to_datetime(df["date"].str.strip() + " " + df["time"].str.strip(), format="%Y/%m/%d %H:%M:%S",
                           errors="coerce", utc=True)
        want = [c for c in KEEP + ["vel_b"] if c in df.columns]
        out = df[want].apply(pd.to_numeric, errors="coerce")
        if "flow" in out.columns:
            bad = ~out["flow"].between(*FLOW_RANGE) | ~out["vel_b"].between(*VEL_RANGE)
            out.loc[bad, "flow"] = np.nan
        out = out.drop(columns=[c for c in ("vel_b",) if c in out.columns])
        out.index = t
        parts.append(out[out.index.notna()])
    if not parts:
        return pd.DataFrame(columns=KEEP)
    return pd.concat(parts).sort_index().resample("1min").mean()


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
