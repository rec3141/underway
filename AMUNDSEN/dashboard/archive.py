"""Amundsen Science's published archive as legs of the dashboard.

``tools/fetch-amundsen-archive.py`` gathers the years before the ship's own
record from https://erddap.amundsenscience.com/erddap and lays them out by
leg (CC-BY 4.0; cite Amundsen Science / ArcticNet):

    <ARCHIVE_ROOT>/by-leg/<year>/<YYYY_LEG_NN>/tsg.csv        TSG, per-minute means
                                              avos.csv       AVOS weather (2004-2022)
                                              ats.csv        ATS weather (2023-2024)
                                              ctd_1dbar.csv  rosette CTD, 1 dbar bins

``python -m dashboard archive-import`` writes each leg's underway files as the
ship's own daily ``ACSD_YYYYMMDD.csv`` files under ``<ARCHIVE_ROOT>/acsd/<year>/<leg>/``,
with the ship's instrument and column names, so discovery, ingest, the charts
and the track take them like any other leg (``legs.discover`` marks them
``archive``). The casts are read from ``ctd_1dbar.csv`` at build time.

The columns carried over, and why some are not:

* TSG: hull temperature, salinity, fluorescence, sound velocity; its vessel
  speed is the ship's speed. The ship's position comes from whichever
  archive row has it.
* AVOS: true wind direction, air temperature, pressure, and the humidity
  that follows from the air and dew-point temperatures. Its wind speed is
  the true wind speed, which the dashboard has no variable for (its wind
  speed is the relative one), so it is left out rather than mislabelled.
* ATS: air temperature, humidity, pressure, true wind direction, short-wave
  radiation; its wind speed is left out for the same reason.
* Navigation (1 Hz, amundsen12447) could not be fetched: the server resets
  every transfer of it.
"""

from __future__ import annotations

import csv
import logging
import math
import os
import re
from collections import defaultdict
from pathlib import Path

from .config import DB_DIR

log = logging.getLogger(__name__)

ARCHIVE_ROOT = Path(os.environ.get("UNDERWAY_ARCHIVE_DIR", DB_DIR.parent / "archive"))
LEG_RE = re.compile(r"^(\d{4})_LEG_(\d{2})$")

# (instrument, column) the ship's ACSD files use, from each archive file's column
TSG = {"latitude": ("POSMV", "Latitude (deg N)"), "longitude": ("POSMV", "Longitude (deg E)"),
       "SST_TSG": ("TSG", "Hull temperature (deg C)"), "P_sal_TSG": ("TSG", "Salinity (psu)"),
       "Fluo": ("TSG", "Fluorescence (ug/L)"), "SVel": ("TSG", "Sound velocity (m/s)"),
       "Speed": ("POSMV", "Speed (knt)")}
AVOS = {"latitude": ("POSMV", "Latitude (deg N)"), "longitude": ("POSMV", "Longitude (deg E)"),
        "Wind_dir": ("AVOS", "True wind direction (deg)"), "Air_temp": ("AVOS", "Air temperature (deg C)"),
        "Pressure": ("AVOS", "Atmospheric pressure (HPa)")}
ATS = {"latitude": ("POSMV", "Latitude (deg N)"), "longitude": ("POSMV", "Longitude (deg E)"),
       "wind_direction": ("ATS_MetTower", "True wind direction (deg)"),
       "air_temperature": ("ATS_MetTower", "Air temperature (deg C)"),
       "air_humidity": ("ATS_MetTower", "Air humidity (%)"),
       "air_pressure": ("ATS_MetTower", "Atmospheric pressure (HPa)"),
       "shortwave_radiation": ("ATS_Starboard", "Short wave radiation (W/m²)")}
HUMIDITY = ("AVOS", "Air humidity (%)")
SOURCES = (("tsg.csv", TSG), ("avos.csv", AVOS), ("ats.csv", ATS))

# the casts: archive column -> (the Casts tab's variable, unit); the older
# one-leg datasets name their columns differently and publish no units for
# some of them, which are then shown without one
CTD_VARS = {"TE90": ("Temperature", "°C"), "Temp": ("Temperature", "°C"),
            "PSAL": ("Salinity", "PSU"), "Sal": ("Salinity", "PSU"),
            "OXYM": ("Oxygen", "µM"), "O2": ("Oxygen", ""),
            "FLOR": ("Fluorescence", "µg/L"), "Fluo": ("Fluorescence", ""),
            "CDOM": ("CDOM", "mg/m³"), "TRAN": ("Transmission", "%"), "Trans": ("Transmission", ""),
            "PSAR": ("PAR", "µE/s/m²"), "Par": ("PAR", ""),
            "NTRA": ("Nitrates", "mmol/m³"), "NO3": ("Nitrates", ""),
            "SIGT": ("Sigma-t", "kg/m³"), "sigt": ("Sigma-t", "kg/m³"),
            "pH": ("pH", ""), "TURB": ("Turbidity", "FTU")}
PRESSURE = ("PRES", "Pres")


def acsd_root() -> Path:
    return ARCHIVE_ROOT / "acsd"


def leg_dirs() -> list[Path]:
    """The archive's leg folders (by-leg/<year>/<YYYY_LEG_NN>)."""
    root = ARCHIVE_ROOT / "by-leg"
    if not root.is_dir():
        return []
    return sorted(d for y in root.iterdir() if y.is_dir() and re.fullmatch(r"\d{4}", y.name)
                  for d in y.iterdir() if d.is_dir() and LEG_RE.match(d.name))


def _num(v) -> float | None:
    try:
        f = float(v)
    except (TypeError, ValueError):
        return None
    return f if math.isfinite(f) else None


def _humidity(t: float | None, td: float | None) -> float | None:
    """Relative humidity (%) from air and dew-point temperatures (°C), Magnus."""
    if t is None or td is None:
        return None
    rh = 100 * math.exp(17.625 * td / (243.04 + td)) / math.exp(17.625 * t / (243.04 + t))
    return round(rh, 1) if 0 < rh <= 105 else None


def _minute(iso: str) -> str | None:
    """'2016-06-05T18:37:47Z' -> '2016/06/05 18:37:00' (the ACSD time format)."""
    m = re.match(r"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})", iso or "")
    return f"{m[1]}/{m[2]}/{m[3]} {m[4]}:{m[5]}:00" if m else None


def leg_rows(leg_dir: Path) -> tuple[list[tuple[str, str]], dict[str, dict]]:
    """A leg's archive files merged per minute: the (instrument, column)
    list and {time: {(instrument, column): value}}. Where two files give the
    ship's position in one minute, the first one read (the TSG's) stands."""
    columns: dict[tuple[str, str], None] = {}
    rows: dict[str, dict] = defaultdict(dict)
    for name, mapping in SOURCES:
        path = leg_dir / name
        if not path.is_file():
            continue
        with open(path, newline="") as f:
            for r in csv.DictReader(f):
                t = _minute(r.get("time", ""))
                if not t:
                    continue
                row = rows[t]
                for src, key in mapping.items():
                    v = _num(r.get(src))
                    if v is not None and key not in row:
                        row[key] = v
                        columns[key] = None
                if name == "avos.csv":
                    rh = _humidity(_num(r.get("Air_temp")), _num(r.get("Dew_point")))
                    if rh is not None and HUMIDITY not in row:
                        row[HUMIDITY] = rh
                        columns[HUMIDITY] = None
    return list(columns), rows


def write_acsd(leg_dir: Path, out: Path) -> int:
    """Write a leg's archive as daily ACSD files in ``out``; return the days
    written. Each file is rewritten whole, beside itself then renamed."""
    columns, rows = leg_rows(leg_dir)
    if not rows or not columns:
        return 0
    days: dict[str, list[str]] = defaultdict(list)
    for t in sorted(rows):
        days[t[:10].replace("/", "")].append(t)
    out.mkdir(parents=True, exist_ok=True)
    head1 = "Time (yyyy/mm/dd HH:MM:SS);" + ";".join(f" {c} " for _, c in columns)
    head2 = "Time;" + ";".join(f" {i} " for i, _ in columns)
    for day, times in days.items():
        path = out / f"ACSD_{day}.csv"
        tmp = path.with_suffix(".tmp")
        with open(tmp, "w", newline="") as f:
            f.write(head1 + "\n" + head2 + "\n")
            for t in times:
                row = rows[t]
                f.write(t + ";" + ";".join(" NaN " if row.get(k) is None else f" {row[k]:g} " for k in columns) + "\n")
        tmp.replace(path)
    return len(days)


def import_all() -> dict:
    """Write every archive leg's ACSD files; a leg's folder is replaced whole
    so a day no longer in the archive leaves no file behind."""
    import shutil
    done = {}
    for d in leg_dirs():
        target = acsd_root() / d.parent.name / d.name
        stage = target.with_name(target.name + ".new")
        shutil.rmtree(stage, ignore_errors=True)
        n = write_acsd(d, stage)
        if n:
            shutil.rmtree(target, ignore_errors=True)
            stage.rename(target)
            done[d.name] = n
        else:
            shutil.rmtree(stage, ignore_errors=True)
    log.info("archive: %d legs written under %s", len(done), acsd_root())
    return done


def casts(leg_id: str) -> list:
    """The leg's rosette CTD casts from the archive's ``ctd_1dbar.csv``."""
    from .casts import Cast
    m = LEG_RE.match(leg_id)
    if not m:
        return []
    path = ARCHIVE_ROOT / "by-leg" / m[1] / leg_id / "ctd_1dbar.csv"
    if not path.is_file():
        return []
    groups: dict[tuple, list[dict]] = defaultdict(list)
    with open(path, newline="") as f:
        for r in csv.DictReader(f):
            if _num(r.get("cast_number")) is None:
                continue
            groups[(r.get("cruise_number", ""), r["cast_number"], r.get("time", ""))].append(r)
    out, seen = [], set()
    for (_, number, when), rs in sorted(groups.items(), key=lambda kv: kv[0][2]):
        n = int(float(number))
        ident = f"CTD_{n:03d}"
        k = 2
        while ident in seen:                          # a cast number used twice in a leg
            ident = f"CTD_{n:03d}_{k}"
            k += 1
        seen.add(ident)
        pkey = next((c for c in PRESSURE if c in rs[0]), None)
        if not pkey:
            continue
        rs = sorted((r for r in rs if _num(r.get(pkey)) is not None), key=lambda r: _num(r[pkey]))
        if not rs:
            continue
        variables, units = {}, {}
        for col, (name, unit) in CTD_VARS.items():
            if col in rs[0] and name not in variables:
                vals = [_num(r.get(col)) for r in rs]
                if any(v is not None for v in vals):
                    variables[name] = vals
                    units[name] = unit
        first = rs[0]
        out.append(Cast(id=f"{leg_id}:{ident}", leg=leg_id, kind="CTD", cast=f"{n:03d}",
                        time=(when or "").rstrip("Z") or None, lat=_num(first.get("latitude")),
                        lon=_num(first.get("longitude")), station=(first.get("station") or "").strip(),
                        p=[_num(r[pkey]) for r in rs], vars=variables, units=units,
                        source={"archive": "Amundsen Science ERDDAP", "file": str(path.name)}))
    return out
