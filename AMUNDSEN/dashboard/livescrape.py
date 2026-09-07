"""The ship intranet's live page, recorded and replayed.

``http://10.0.0.2/live.html`` shows the acquisition host's readings a few
seconds old — position, heading, speed and bottom depth, the met tower, the
thermosalinograph intake, the rosette and the 500HP winch — but keeps no
history. The server polls it every few seconds (``serve.IntranetLive``) and
``record`` appends every snapshot whose clock moved to
``db/live_scrape/YYYYMMDD.jsonl``, one JSON line each, flattened as
``section/label`` keys. Nothing is ever deleted.

``provisional_tail`` turns those snapshots into minute means on the ACSD
columns they duplicate, for the minutes past the end of the ACSD record:
the map's track, the water and met panels keep going while the acquisition
host's file logging is behind or stopped. Position and heading are POSMV,
depth the multibeam, the water values the TSG; the page's met values are
the met tower's, written to the AVOS columns the panels read so the
series stays continuous (the two sensors agree to a few tenths). The page's
wind direction is heading plus relative direction, not true wind, so it is
left out.
"""

from __future__ import annotations

import json
import logging
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from .config import DB_DIR

log = logging.getLogger(__name__)
SECTION_KEYS = (("nav", r"navigation"), ("atm", r"atmospheric"), ("sea", r"sea water"), ("ros", r"rosette"), ("500", r"500hp"))
_last_t: str | None = None


def _dir() -> Path:
    return DB_DIR / "live_scrape"


def ddm(text) -> float | None:
    """76° 24.9565' N -> 76.4159; 89° 12.6412' W -> -89.2107."""
    m = re.search(r"(\d+)\D+([\d.]+)'?\s*([NSEW])", str(text or ""))
    if not m:
        return None
    v = int(m.group(1)) + float(m.group(2)) / 60
    return -v if m.group(3) in "SW" else v


def flatten(sections: list[dict]) -> dict:
    out = {}
    for s in sections:
        key = next((k for k, pat in SECTION_KEYS if re.search(pat, s.get("title", ""), re.I)), None)
        if not key:
            continue
        for label, value in s.get("rows", []):
            out[f"{key}/{label}"] = value
    return out


def record(sections: list[dict]) -> bool:
    """Append the snapshot when its clock moved on; returns whether it did."""
    global _last_t
    flat = flatten(sections)
    t = flat.get("nav/Time (UTC)")
    if not t or t == _last_t:
        return False
    try:
        stamp = datetime.strptime(t, "%Y/%m/%d %H:%M:%S").replace(tzinfo=timezone.utc)
    except ValueError:
        return False
    _last_t = t
    d = _dir(); d.mkdir(parents=True, exist_ok=True)
    with open(d / f"{stamp:%Y%m%d}.jsonl", "a") as fh:
        fh.write(json.dumps({"t": stamp.isoformat(timespec="seconds"), "rows": flat}) + "\n")
    return True


def load(after: pd.Timestamp) -> pd.DataFrame:
    """Every snapshot after ``after`` (UTC), one row each, raw strings."""
    d = _dir()
    if not d.is_dir():
        return pd.DataFrame()
    rows = []
    for p in sorted(d.glob("*.jsonl")):
        if p.stem < f"{after:%Y%m%d}":
            continue
        for line in p.read_text().splitlines():
            try:
                j = json.loads(line)
            except ValueError:
                continue
            t = pd.Timestamp(j["t"])
            if t > after:
                rows.append(dict(j["rows"], _t=t))
    if not rows:
        return pd.DataFrame()
    df = pd.DataFrame(rows).set_index("_t").sort_index()
    return df[~df.index.duplicated(keep="last")]


# ACSD column (a pattern on the key) <- page field, and how to read it
NUM = lambda v: pd.to_numeric(str(v).replace(",", "").strip(), errors="coerce")
ACSD_OF = {
    r"^posmv — latitude": ("nav/Latitude", ddm), r"^posmv — longitude": ("nav/Longitude", ddm),
    r"^posmv — speed": ("nav/Speed (knt)", NUM), r"^posmv — heading": ("nav/Heading (deg)", NUM), r"^posmv — track": ("nav/Track (deg)", NUM),
    r"^multibeam — bottom depth": ("nav/Depth (m)", NUM),
    r"^avos — relative wind speed": ("atm/Wind speed (knt)", NUM), r"^avos — atmospheric pressure": ("atm/Pressure (hPa)", NUM),
    r"^avos — air temperature": ("atm/Temperature (deg C)", NUM), r"^avos — air humidity": ("atm/Humidity (%)", NUM),
    r"^tsg — hull temperature": ("sea/Temperature (deg C)", NUM), r"^tsg — salinity": ("sea/Salinity (psu)", NUM),
    r"^tsg — fluorescence": ("sea/Fluorescence (ug/L)", NUM), r"^tsg — oxygen": ("sea/Oxygene (ml/L)", NUM),
    r"^tsg — ecocdom": ("sea/EcoCdom (mg/m³)", NUM), r"^tsg — sound velocity": ("sea/Sound velocity (m/s)", NUM),
    r"^ctd-rosette — rosette depth": ("ros/Rosette Depth (m)", NUM), r"^500hp — winch cable length": ("500/500HP cable length (m)", NUM),
}


def provisional_tail(after: pd.Timestamp, columns: list[str]) -> pd.DataFrame:
    """Minute means of the recorded page after ``after`` (the end of the
    ACSD record), on the ACSD columns they duplicate."""
    if after is None:
        return pd.DataFrame(columns=columns)
    naive = after.tzinfo is None
    cut = after.tz_localize("UTC") if naive else after.tz_convert("UTC")
    raw = load(cut)
    if raw.empty:
        return pd.DataFrame(columns=columns)
    vals = pd.DataFrame(index=raw.index)
    for pat, (field, conv) in ACSD_OF.items():
        key = next((c for c in columns if re.match(pat, c)), None)
        # the page's labels are matched up to their unit, whose characters
        # have arrived mangled before ("mg/mÂ³")
        stem = field.split(" (")[0]
        src = next((c for c in raw.columns if str(c).split(" (")[0] == stem), None)
        if key is None or src is None:
            continue
        col = raw[src]
        vals[key] = col.map(conv) if conv is ddm else pd.to_numeric(col.astype(str).str.replace(",", "").str.strip(), errors="coerce")
    if vals.empty or not len(vals.columns):
        return pd.DataFrame(columns=columns)
    minute = vals.resample("1min").mean()
    out = minute.reindex(columns=columns)
    if naive:
        out.index = out.index.tz_convert(None)
    return out.dropna(how="all")
