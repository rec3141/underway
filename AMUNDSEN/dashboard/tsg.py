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

The file also runs two minutes behind real time where the ACSD feed is
flushed every ten, so its minutes past the end of the ACSD record stand in
as a provisional tail (``provisional_tail``): the TSG channels and its GPS
position, mapped onto the ACSD columns they duplicate (the values agree to
the fourth decimal; the position to about 15 m). The tail is recomputed at
every build from whatever the ACSD does not yet cover, however long that
is, and every provisional row is also appended to ``db/provisional_tsg.csv``
so nothing seen only through the TSG file is ever lost.
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
KEEP = ["flow", "cond", "t_sbe45", "t_sbe38", "sal", "fluo", "oxy", "cdom", "vel_b", "lat", "lon"]
# the ACSD column each TSG channel duplicates, as a pattern on the key
ACSD_OF = {"t_sbe38": r"^tsg — hull temperature", "t_sbe45": r"^tsg — salinometer temperature", "sal": r"^tsg — salinity",
           "oxy": r"^tsg — oxygen", "fluo": r"^tsg — fluorescence", "cdom": r"^tsg — ecocdom", "vel_b": r"^tsg — sound velocity",
           "lat": r"^posmv — latitude", "lon": r"^posmv — longitude"}
FLOW_RANGE = (0.0, 10.0)                # the flow sensor is a voltage
VEL_RANGE = (1350.0, 1650.0)            # sound speed in seawater, m/s
CACHE_VERSION = 4


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
        want = [c for c in KEEP if c in df.columns]
        out = df[want].apply(pd.to_numeric, errors="coerce")
        if "flow" in out.columns and "vel_b" in out.columns:
            bad = ~out["flow"].between(*FLOW_RANGE) | ~out["vel_b"].between(*VEL_RANGE)
            out.loc[bad, "flow"] = np.nan
            out.loc[~out["vel_b"].between(*VEL_RANGE), "vel_b"] = np.nan
        if "lat" in out.columns:
            out.loc[~out["lat"].between(-90, 90) | ~out["lon"].between(-180, 180), ["lat", "lon"]] = np.nan
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


def provisional_tail(tsg: pd.DataFrame, after: pd.Timestamp, columns: list[str]) -> pd.DataFrame:
    """The TSG minutes after ``after`` (the end of the ACSD record), as rows
    in the ACSD columns they duplicate, for as long as the ACSD stays behind."""
    import re
    if tsg is None or after is None:
        return pd.DataFrame(columns=columns)
    naive = after.tzinfo is None
    cut = after.tz_localize("UTC") if naive else after.tz_convert("UTC")
    tail = tsg[tsg.index > cut]
    out = pd.DataFrame(index=tail.index.tz_convert(None) if naive else tail.index, columns=columns, dtype=float)
    for ch, pat in ACSD_OF.items():
        key = next((c for c in columns if re.match(pat, c)), None)
        if key and ch in tail.columns:
            out[key] = tail[ch].to_numpy()
    return out.dropna(how="all")


def archive_tail(tail: pd.DataFrame) -> int:
    """Keep every provisional row in ``db/provisional_tsg.csv`` (one row per
    minute, the newest version of a minute winning); returns the rows added."""
    if tail is None or not len(tail):
        return 0
    p = DB_DIR / "provisional_tsg.csv"
    idx = tail.index.tz_convert("UTC") if tail.index.tz is not None else tail.index.tz_localize("UTC")
    new = tail.copy(); new.index = idx.strftime("%Y-%m-%dT%H:%M:%SZ"); new.index.name = "time_utc"
    if p.is_file():
        old = pd.read_csv(p, index_col="time_utc")
        added = len(new.index.difference(old.index))
        merged = pd.concat([old, new]); merged = merged[~merged.index.duplicated(keep="last")].sort_index()
    else:
        added, merged = len(new), new
    DB_DIR.mkdir(parents=True, exist_ok=True)
    tmp = p.with_suffix(".tmp"); merged.to_csv(tmp); tmp.replace(p)
    return added
