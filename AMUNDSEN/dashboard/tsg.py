"""The TSG's own 1 Hz files: ``Data/TSG/<leg>/tsg_convdata_YYYYMMDD.cnv``.

They carry what the ACSD feed leaves out — above all the intake water flow
(a sensor voltage, ``Debit``; about 1.5 V pumping, near 0.06 V stopped) —
plus conductivity and both temperatures. Each file is reduced to minute
means and cached per leg in ``db/tsg/<leg>.pkl`` keyed by file size and
mtime, so only the growing current-day file is re-read.
"""

from __future__ import annotations

import io
import logging
import pickle
from pathlib import Path

import pandas as pd

from .config import DATA_ROOT, DB_DIR

log = logging.getLogger(__name__)
COLS = ["t", "t_gps", "lat", "lon", "t_sbe45", "cond", "sal", "vel_a", "t_sbe38", "fluo", "oxy", "cdom", "flow", "vel_b"]
KEEP = ["flow", "cond", "t_sbe45", "t_sbe38", "sal"]


def parse_conv(path: Path) -> pd.DataFrame:
    text = path.read_bytes().decode("latin-1")            # one read: the share is CIFS
    df = pd.read_csv(io.StringIO(text), sep=";", header=None, engine="c", on_bad_lines="skip", dtype=str)
    df = df.iloc[:, :len(COLS)]
    df.columns = COLS[:df.shape[1]]
    t = pd.to_datetime(df["t"].str.strip(), format="%Y/%m/%d %H:%M:%S", errors="coerce", utc=True)
    out = df[[c for c in KEEP if c in df.columns]].apply(pd.to_numeric, errors="coerce")
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
            if ent and ent["size"] == st.st_size and abs(ent["mtime"] - st.st_mtime) < 2:
                continue
            try:
                cache[key] = {"size": st.st_size, "mtime": st.st_mtime, "frame": parse_conv(p)}
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
