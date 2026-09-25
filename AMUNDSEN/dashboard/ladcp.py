"""Processed lowered ADCP profiles on the ship's native depth grid.

EWCT and NSCT already reference true east and north; no heading rotation or
depth interpolation is applied. ERRV is retained without an automatic QC cut.
"""

from __future__ import annotations

import json
import logging
import math
import re
from datetime import datetime
from pathlib import Path

from .casts import Cast, _meta_key, read_logbook
from .legs import Leg

log = logging.getLogger(__name__)
CACHE_VERSION = 1
QC_NOTE = ("Processed ship LADCP export; scientific QC has not been verified. "
           "Error velocity is retained; no automatic error threshold is applied. "
           "Components reference true east/north; direction is toward the flow.")


def parse_ladcp(path: Path, leg: Leg, logbook: dict | None = None) -> Cast:
    """Read a .lad profile, rejecting ambiguous identity and malformed grids."""
    header, rows = {}, []
    columns = None
    for line in path.read_bytes().decode("utf-8-sig").splitlines():
        line = line.strip()
        if not line:
            continue
        if line.startswith("%"):
            key, sep, value = line[1:].partition(":")
            if sep:
                header[key.strip()] = value.strip()
            continue
        tokens = line.split()
        if columns is None:
            if tokens != ["DEPH", "EWCT", "NSCT", "ERRV"]:
                raise ValueError("expected DEPH EWCT NSCT ERRV columns")
            columns = tokens
        elif all(set(t) == {"-"} for t in tokens):
            continue
        else:
            if len(tokens) != 4:
                raise ValueError("malformed profile row")
            rows.append([float(t) for t in tokens])
    identity = re.fullmatch(r"CTD_(\d{4})_(\d{2})_(\d{3})\.cnv", header.get("Cast_Number", ""), re.I)
    filename = re.fullmatch(r"stn(\d{3})\.lad", path.name, re.I)
    if not identity or not filename:
        raise ValueError("missing or invalid cast identity")
    year, number, cast = identity.groups()
    if (int(year), int(number)) != (leg.year, leg.number) or cast != filename[1]:
        raise ValueError("cast header disagrees with leg or filename")
    if header.get("Cruise_Number") != f"{leg.year}_{leg.number:02d}":
        raise ValueError("cruise header disagrees with leg")
    if not rows:
        raise ValueError("empty profile")
    depth = [r[0] for r in rows]
    if any(not math.isfinite(d) or d < 0 for d in depth) or any(b <= a for a, b in zip(depth, depth[1:])):
        raise ValueError("depth must be finite, nonnegative and strictly increasing")
    for key, actual in (("Min_Depth [m]", depth[0]), ("Max_Depth [m]", depth[-1])):
        declared = float(header[key])
        if not math.isfinite(declared) or abs(declared - actual) > 0.11:
            raise ValueError(f"{key} disagrees with profile")
    lat, lon = float(header["Initial_Latitude [deg]"]), float(header["Initial_Longitude [deg]"])
    if not (-90 <= lat <= 90 and -180 <= lon <= 180):
        raise ValueError("invalid coordinates")
    when = datetime.strptime(header["Start_Date_Time [UTC]"], "%d-%b-%Y %H:%M:%S")
    if when.year != leg.year:
        raise ValueError("timestamp disagrees with cruise year")
    bottom = float(header.get("Sounding [m]", "nan"))
    bottom = bottom if math.isfinite(bottom) and bottom > 0 else None
    def finite(v):
        return v if math.isfinite(v) else None
    east, north, error = ([finite(row[i]) for row in rows] for i in (1, 2, 3))
    speed = [finite(math.hypot(u, v)) if u is not None and v is not None else None for u, v in zip(east, north)]
    variables = {"Eastward current": east, "Northward current": north,
                 "Current speed": speed, "Current error": error}
    lb = (logbook or {}).get(cast, {})
    stat = path.stat()
    return Cast(id=f"{leg.id}:LADCP{cast}", leg=leg.id, kind="LADCP", cast=cast,
                time=when.isoformat() + "Z", lat=lat, lon=lon,
                station=lb.get("station", ""), label=lb.get("label", ""), bottom_m=bottom,
                depth=depth, vars=variables, units={v: "m/s" for v in variables},
                parent_cast_id=f"{leg.id}:CTD_{cast}", qc_note=QC_NOTE,
                source={"path": f"Data/Rosette/{leg.id}/Ladcp/{path.name}",
                        "size": stat.st_size, "mtime_ns": stat.st_mtime_ns,
                        "header": header})


def ladcp_casts(leg: Leg, data_root: Path, db_dir: Path) -> list[Cast]:
    """Revalidate every source by size and nanosecond mtime, including old casts."""
    try:
        logbook = read_logbook(leg.stations)
    except (OSError, UnicodeError) as error:
        log.warning("%s: LADCP logbook unavailable (%s)", leg.id, error)
        logbook = {}
    try:
        sources = sorted((data_root / "Rosette" / leg.id / "Ladcp").glob("*.lad"))
    except OSError as error:
        log.warning("%s: LADCP directory unavailable (%s)", leg.id, error)
        return []
    result = []
    for path in sources:
        cache = db_dir / "casts" / leg.id / ("LADCP_" + path.stem + ".json")
        try:
            stat = path.stat()
            stamp = [path.name, stat.st_size, stat.st_mtime_ns]
            metadata = _meta_key(logbook)
            try:
                saved = json.loads(cache.read_text())
                if saved.get("version") == CACHE_VERSION and saved.get("stamp") == stamp and saved.get("meta") == metadata:
                    result.append(Cast(**saved["cast"]))
                    continue
            except (OSError, ValueError, KeyError, TypeError):
                pass
            cast = parse_ladcp(path, leg, logbook)
            # A producer may be replacing a file while the share is read.
            after = path.stat()
            if [path.name, after.st_size, after.st_mtime_ns] != stamp:
                log.warning("%s: LADCP source changed during read; defer", path)
                continue
            cache.parent.mkdir(parents=True, exist_ok=True)
            temp = cache.with_suffix(".tmp")
            temp.write_text(json.dumps({"version": CACHE_VERSION, "stamp": stamp,
                                       "meta": metadata, "cast": cast.__dict__}, allow_nan=False))
            temp.replace(cache)
            result.append(cast)
        except (OSError, ValueError, KeyError, TypeError) as error:
            log.warning("%s: cannot import LADCP (%s)", path, error)
    return result
