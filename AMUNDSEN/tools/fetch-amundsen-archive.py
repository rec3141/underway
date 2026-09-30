#!/usr/bin/env python3
"""Fetch Amundsen Science's published archive from its ERDDAP, for ingestion
by year and leg.

Runs on grid (the ship reaches the internet over Starlink; grid is on campus).
Everything here is CC-BY 4.0: cite Amundsen Science / ArcticNet and the
dataset's DOI from https://catalogue.amundsenscience.com/.

What it fetches, and how:

* TSG (amundsen12715, 1 min): the server's own per-minute means
  (``orderByMean("cruise_number,time/1minute")``), one calendar month per
  request.
* navigation (amundsen12447, 1 Hz, 18 GB): the server cannot average it (a
  single day's mean runs past five minutes and slows every other dataset), so
  its daily source files come whole from ``/files/`` into ``raw/nav-nc/`` and
  are averaged per minute here. Only position and speed are kept: heading and
  track are circular, a mean of them is wrong at north, and the course
  follows from the positions.
* weather, AVOS (amundsen12518) and ATS (amundsen13391): whole, AVOS one
  month per request and ATS one voyage per request (the server finds none of
  its rows by time). AVOS is hourly or sparser, so a per-minute mean would change
  nothing; ATS's wind direction is circular.
* rosette CTD: amundsen12713 (2014 on) one voyage per request, and the older
  one-dataset-per-leg sets whole; then binned here to 1 dbar. The Bioness and
  Hydrobios net CTDs (amundsen12716, 12717) are not rosette casts.

The server is slow and stops answering under parallel load, so requests go
one at a time with a pause between them. A request's file is written beside
its target and renamed when complete, so a rerun skips what is done and
fetches the rest; an empty month (HTTP 404) leaves an empty ``.none`` marker.

    fetch-amundsen-archive.py fetch [--until 2025-01-01] [--only tsg,avos,ats,ctd,nav]
    fetch-amundsen-archive.py split       raw/ -> by-leg/<year>/<cruise>/<kind>.csv
    fetch-amundsen-archive.py status

Layout under ``--root`` (default /data/amundsen-archive):

    raw/<kind>/<YYYY-MM>.csv        a month of a time series (ATS: <voyage>.csv)
    raw/nav-nc/<file>.csv.nc        a day of navigation as published
    raw/nav/<file>.csv              that day, averaged per minute
    raw/ctd/<dataset>[_<cruise>].csv
    by-leg/<year>/<cruise>/{nav,tsg,avos,ats}.csv, ctd_1dbar.csv
    fetch.log
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import math
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import defaultdict
from pathlib import Path

ERDDAP = "https://erddap.amundsenscience.com/erddap/tabledap"
FILES = "https://erddap.amundsenscience.com/erddap/files"
NAV = "amundsen12447"
NAV_VARS = ("latitude", "longitude", "Speed")
PAUSE_S = 5
TIMEOUT_S = 1800
RETRIES = 3

# kind: (dataset, variables, per-minute mean, first month)
SERIES = {
    "tsg": ("amundsen12715", "cruise_number,time,latitude,longitude,SST_TSG,P_sal_TSG,Fluo,SVel", True, "2005-08"),
    "avos": ("amundsen12518", "cruise_number,time,latitude,longitude,Wind_dir,Wind_speed,Air_temp,Dew_point,Pressure", False, "2004-06"),
    "ats": ("amundsen13391", "cruise_number,time,latitude,longitude,wind_speed,wind_direction,air_temperature,"
            "air_humidity,air_pressure,surface_temperature,photosynthetically_active_radiation,"
            "shortwave_radiation,longwave_radiation", False, "2023-01"),
}
# no time index on the server: a month's query finds nothing, a voyage's works
PER_CRUISE = {"ats"}
CTD_VARS = ("cruise_name,cruise_number,cast_number,station,time,latitude,longitude,PRES,depth,"
            "TE90,PSAL,OXYM,pH,NTRA,FLOR,CDOM,TRAN,TURB,PSAR,SPAR,SIGT")
CTD_MODERN = "amundsen12713"
CTD_LEGACY = (
    "amundsen513 amundsen514 amundsen515 amundsen516 amundsen518 amundsen519 amundsen520 amundsen521 "
    "amundsen522 amundsen524 amundsen525 amundsen526 amundsen527 amundsen80 amundsen438 amundsen449 "
    "amundsen452 amundsen456 amundsen468 amundsen482 amundsen491 amundsen502 amundsen496 amundsen509 "
    "amundsen510 amundsen796 amundsen797 amundsen798 amundsen799 amundsen800 amundsen936 amundsen1496 "
    "amundsen1497 amundsen1498 amundsen1499 amundsen1500 amundsen1501 amundsen1502 amundsen10987 "
    "amundsen10988 amundsen10989 amundsen10990 amundsen10991 amundsen10992 amundsen11150 amundsen11151 "
    "amundsen11153 amundsen11154 amundsen11155 amundsen11156 amundsen11943 amundsen11919 amundsen11920 "
    "amundsen11921 amundsen11922 amundsen11923 amundsen11924 amundsen11926 amundsen11927"
).split()
CTD_TEXT = {"platform_name", "platform_id", "filename", "cruise_name", "cruise_number", "cast_number", "station", "time"}
PRESSURE = ("PRES", "Pres")


def log(root: Path, msg: str) -> None:
    line = f"{dt.datetime.now(dt.timezone.utc):%Y-%m-%dT%H:%M:%SZ} {msg}"
    print(line, flush=True)
    with open(root / "fetch.log", "a") as f:
        f.write(line + "\n")


def get(root: Path, url: str, out: Path) -> str:
    """Fetch ``url`` to ``out``: "done", "none" (the server has no rows) or "failed"."""
    if out.exists() or out.with_suffix(".none").exists():
        return "skip"
    out.parent.mkdir(parents=True, exist_ok=True)
    part = out.with_suffix(".part")
    for attempt in range(1, RETRIES + 1):
        t0 = time.monotonic()
        try:
            with urllib.request.urlopen(url, timeout=TIMEOUT_S) as r, open(part, "wb") as f:
                while chunk := r.read(1 << 20):
                    f.write(chunk)
            part.replace(out)
            log(root, f"ok {out.relative_to(root)} {out.stat().st_size} B {time.monotonic() - t0:.0f}s")
            return "done"
        except urllib.error.HTTPError as e:
            body = e.read(400).decode(errors="replace")
            if e.code == 404 and ("no matching results" in body.lower() or "no data matches" in body.lower()):
                out.with_suffix(".none").touch()
                return "none"
            log(root, f"http {e.code} {out.relative_to(root)} (try {attempt}): {body[:160]!r}")
        except Exception as e:  # noqa: BLE001 - timeouts, resets: retry
            log(root, f"error {out.relative_to(root)} (try {attempt}): {e}")
        part.unlink(missing_ok=True)
        time.sleep(PAUSE_S * 6 * attempt)
    return "failed"


def query(dataset: str, variables: str, *constraints: str) -> str:
    q = variables + "".join("&" + c for c in constraints)
    return f"{ERDDAP}/{dataset}.csv?" + urllib.parse.quote(q, safe=",&=<>()/:\"")


def months(first: str, until: dt.date):
    y, m = map(int, first.split("-"))
    while dt.date(y, m, 1) < until:
        nxt = (y + m // 12, m % 12 + 1)
        yield f"{y:04d}-{m:02d}", dt.date(y, m, 1), dt.date(*nxt, 1)
        y, m = nxt


def distinct(dataset: str, var: str) -> list[str]:
    with urllib.request.urlopen(query(dataset, var, "distinct()"), timeout=300) as r:
        rows = list(csv.reader(r.read().decode().splitlines()))
    return [row[0] for row in rows[2:] if row and row[0]]


def fetch(root: Path, until: dt.date, only: set[str]) -> int:
    failed = 0
    for kind, (ds, variables, mean, first) in SERIES.items():
        if only and kind not in only:
            continue
        if kind in PER_CRUISE:
            for cruise in distinct(ds, "cruise_number"):
                if (y := year_of(cruise)) and y >= until.year:
                    continue
                status = get(root, query(ds, variables, f'cruise_number="{cruise}"'),
                             root / "raw" / kind / f"{re.sub(r'[^\w.-]', '_', cruise)}.csv")
                failed += status == "failed"
                if status != "skip":
                    time.sleep(PAUSE_S)
            continue
        for label, a, b in months(first, until):
            cons = [f"time>={a}T00:00:00Z", f"time<{b}T00:00:00Z"]
            if mean:
                cons.append('orderByMean("cruise_number,time/1minute")')
            status = get(root, query(ds, variables, *cons), root / "raw" / kind / f"{label}.csv")
            failed += status == "failed"
            if status != "skip":
                time.sleep(PAUSE_S)
    if not only or "ctd" in only:
        for cruise in distinct(CTD_MODERN, "cruise_number"):
            if (y := year_of(cruise)) and y >= until.year:
                continue
            safe = re.sub(r"[^\w.-]", "_", cruise)
            status = get(root, query(CTD_MODERN, CTD_VARS, f'cruise_number="{cruise}"'),
                         root / "raw/ctd" / f"{CTD_MODERN}_{safe}.csv")
            failed += status == "failed"
            if status != "skip":
                time.sleep(PAUSE_S)
        for ds in CTD_LEGACY:
            # the older sets name their columns in more than one way (PRES or
            # Pres, TE90 or Temp), so each comes with all of its own
            status = get(root, f"{ERDDAP}/{ds}.csv", root / "raw/ctd" / f"{ds}.csv")
            failed += status == "failed"
            if status != "skip":
                time.sleep(PAUSE_S)
    if not only or "nav" in only:
        failed += fetch_nav(root, until)
    log(root, f"fetch finished, {failed} failed (rerun to retry them)")
    return 1 if failed else 0


def fetch_nav(root: Path, until: dt.date) -> int:
    """The navigation's daily files, oldest first, each averaged per minute
    as soon as it arrives."""
    listing = root / "raw/nav-files.csv"
    with urllib.request.urlopen(f"{FILES}/{NAV}/.csv", timeout=300) as r:
        listing.write_bytes(r.read())
    names = sorted(row["Name"] for row in csv.DictReader(open(listing)) if row["Name"].endswith(".nc"))
    failed = 0
    for name in names:
        day = re.search(r"_(\d{8})\.", name)
        if day and dt.datetime.strptime(day.group(1), "%Y%m%d").date() >= until:
            continue
        nc = root / "raw/nav-nc" / name
        status = get(root, f"{FILES}/{NAV}/{urllib.parse.quote(name)}", nc)
        failed += status == "failed"
        out = root / "raw/nav" / (name[:-len(".csv.nc")] + ".csv")
        if nc.exists() and not out.exists():
            try:
                write(out, nav_minutes(nc))
            except Exception as e:  # noqa: BLE001 - one bad file must not stop the rest
                log(root, f"could not average {nc.name}: {e}")
                failed += 1
        if status != "skip":
            time.sleep(PAUSE_S)
    return failed


def nav_minutes(nc: Path) -> list[dict]:
    """A day of 1 Hz navigation as per-minute means of position and speed."""
    import netCDF4
    import numpy as np
    with netCDF4.Dataset(nc) as d:
        t = d["time"]
        secs = np.asarray(netCDF4.date2num(netCDF4.num2date(t[:], t.units, only_use_cftime_datetimes=False),
                                           "seconds since 1970-01-01T00:00:00Z"), dtype=float)
        cols = {v: np.ma.filled(np.ma.asarray(d[v][:], dtype=float), np.nan) for v in NAV_VARS if v in d.variables}
        cruise = ""
        if "cruise_number" in d.variables:
            raw = d["cruise_number"][:]
            cruise = str(netCDF4.chartostring(raw[0]) if getattr(raw, "ndim", 0) > 1 else raw[0]).strip()
        cruise = cruise or str(getattr(d, "cruise_number", "")).strip()
    minute = np.floor(secs / 60).astype(np.int64)
    keep = np.isfinite(secs)
    out = []
    for m in np.unique(minute[keep]):
        sel = keep & (minute == m)
        row = {"cruise_number": cruise,
               "time": dt.datetime.fromtimestamp(int(m) * 60, dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
               "n": int(sel.sum())}
        for v, a in cols.items():
            vals = a[sel]
            vals = vals[np.isfinite(vals)]
            row[v] = f"{vals.mean():.6f}" if vals.size else ""
        out.append(row)
    return out


def year_of(cruise: str) -> int | None:
    m = re.search(r"(?<!\d)((?:19|20)\d\d)", cruise)
    return int(m.group(1)) if m else None


def rows(path: Path):
    """An ERDDAP CSV's rows as dicts (its second line is the units)."""
    with open(path, newline="") as f:
        reader = csv.reader(f)
        header = next(reader, None)
        next(reader, None)
        for row in reader:
            yield dict(zip(header, row))


def leg_dir(root: Path, cruise: str, when: str) -> Path:
    """by-leg/<year>/<cruise>: the year from the cruise's name, else from the
    row's time; a row with neither goes under ``unknown``."""
    year = year_of(cruise) or (int(when[:4]) if (when or "")[:4].isdigit() else "unknown")
    return root / "by-leg" / str(year) / re.sub(r"[^\w.-]", "_", cruise or "unknown")


def split(root: Path) -> None:
    out = root / "by-leg"
    for kind in (*SERIES, "nav"):
        per: dict[Path, list[dict]] = defaultdict(list)
        for path in sorted((root / "raw" / kind).glob("*.csv")):
            for r in (csv.DictReader(open(path, newline="")) if kind == "nav" else rows(path)):
                per[leg_dir(root, r.get("cruise_number", ""), r["time"])].append(r)
        for d, rs in per.items():
            write(d / f"{kind}.csv", rs)
    per = defaultdict(list)
    for path in sorted((root / "raw/ctd").glob("*.csv")):
        for r in rows(path):
            per[leg_dir(root, r.get("cruise_number", ""), r["time"])].append(r)
    for d, rs in per.items():
        write(d / "ctd_1dbar.csv", bin_dbar(rs))
    log(root, f"split into {sum(1 for _ in out.glob('*/*'))} legs under {out}")


def bin_dbar(rs: list[dict]) -> list[dict]:
    """Each cast's rows averaged into 1 dbar bins centred on whole decibars;
    text columns and the cast's time and position keep their first value."""
    bins: dict[tuple, list[dict]] = defaultdict(list)
    for r in rs:
        key = next((k for k in PRESSURE if r.get(k) not in (None, "")), None)
        try:
            p = float(r[key])
        except (KeyError, TypeError, ValueError):
            continue
        bins[(r.get("cruise_number"), r.get("cast_number"), r.get("time"), round(p))].append(r)
    out = []
    for (_, _, _, p), group in sorted(bins.items(), key=lambda kv: (kv[0][0] or "", kv[0][2] or "", kv[0][1] or "", kv[0][3])):
        row = dict(group[0])
        for k in row:
            if k in CTD_TEXT or k in ("latitude", "longitude"):
                continue
            vals = [float(g[k]) for g in group if _num(g.get(k))]
            row[k] = f"{sum(vals) / len(vals):.4f}" if vals else ""
        for k in PRESSURE:
            if k in row:
                row[k] = str(p)
        out.append(row)
    return out


def _num(v) -> bool:
    try:
        return v not in (None, "") and not math.isnan(float(v))
    except ValueError:
        return False


def write(path: Path, rs: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fields = list(dict.fromkeys(k for r in rs for k in r))
    tmp = path.with_suffix(".tmp")
    with open(tmp, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(rs if set(PRESSURE) & set(fields) else sorted(rs, key=lambda r: r.get("time", "")))
    tmp.replace(path)


def status(root: Path) -> None:
    for d in sorted((root / "raw").glob("*")):
        done = list(d.glob("*.csv"))
        none = list(d.glob("*.none"))
        size = sum(f.stat().st_size for f in done)
        print(f"{d.name:5} {len(done):4} files {size / 1e6:9.1f} MB, {len(none)} empty")
    log_path = root / "fetch.log"
    if log_path.exists():
        print("".join(log_path.read_text().splitlines(True)[-5:]), end="")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("action", choices=("fetch", "split", "status"))
    ap.add_argument("--root", type=Path, default=Path("/data/amundsen-archive"))
    ap.add_argument("--until", type=dt.date.fromisoformat, default=dt.date(2025, 1, 1),
                    help="fetch before this date (default: the years before our own record)")
    ap.add_argument("--only", default="", help="comma-separated kinds: nav,tsg,avos,ats,ctd")
    a = ap.parse_args()
    a.root.mkdir(parents=True, exist_ok=True)
    if a.action == "fetch":
        return fetch(a.root, a.until, set(filter(None, a.only.split(","))))
    if a.action == "split":
        split(a.root)
    else:
        status(a.root)
    return 0


if __name__ == "__main__":
    sys.exit(main())
