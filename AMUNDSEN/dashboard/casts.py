"""CTD profiles for the cast viewer.

Two sources:

* Rosette casts. The profile is the SeaBird ``.cnv`` in
  ``Data/external_proprietary/CTD/`` when one exists (downcast, 1-dbar bins).
  Amundsen Science also publishes each cast as Plotly HTML files under
  ``Data/Rosette/<leg>/plots/`` with the binned profile embedded as base64
  float arrays; those supply the variables the ``.cnv`` lacks and are the only
  source for legs without ``.cnv`` files. The filename names the rosette
  (Classic or TM). Station, label and time come from the leg's CTD logbook.
* MVP (Moving Vessel Profiler) tows: ``Data/MVP/<leg>/<tow>/*.m1``, an AML
  CTD text format with the fix in the header and one row per sample. A tow
  directory becomes one dataset holding every dip.

Parsed casts are cached as JSON under ``DB_DIR/casts`` so a rebuild only
touches new ones. Every file on the share is read in a single call: CIFS
charges a network round trip per read, and the SBE 9 files are 21 MB.
"""

from __future__ import annotations

import base64
import csv
import json
import logging
import re
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np

from .config import DATA_ROOT, DB_DIR
from .legs import Leg

log = logging.getLogger(__name__)

# group 4 is the rosette: Classic (the main SBE 9 rosette) or TM (trace-metal)
PLOT_RE = re.compile(r"^CTD_(\d{4})_(\d{2})_(\d{3})_([A-Za-z]+)_(.+)_raw_data\.html$")
ROSETTE_KIND = {"classic": "CTD", "tm": "TM"}
SKIP_PAIRS = {"TS_diagram"}
# display name, unit, and how the plots label the trace
ROSETTE_VARS = {
    "Temperature": ("Temperature", "°C"), "Salinity": ("Salinity", "PSU"),
    "Oxygen": ("Oxygen", "µM"), "OxySat": ("Oxygen saturation", "mL/L"),
    "Fluorescence": ("Fluorescence", "µg/L"), "Transmission": ("Transmission", "%"),
    "Cdom": ("CDOM", "mg/m³"), "Par": ("PAR", "µE/s/m²"),
    "Buoyancy": ("Buoyancy frequency", "rad²/s²"), "Sigma-t": ("Sigma-t", "kg/m³"),
}
MVP_VARS = {"Temp": ("Temperature", "°C"), "Sal": ("Salinity", "PSU"), "Density": ("Sigma-t", "kg/m³"),
            "SV": ("Sound velocity", "m/s"),
            # the AML CTD-SV's three analogue inputs, in raw counts (millivolts):
            # a C-Star transmissometer, an oxygen optode and an ECO fluorometer,
            # identified from the ship's own MVP plots (Data/MVP/<leg>/<tow>/plot).
            # No calibration file travels with the data, so the scalings below
            # are nominal: 0–5 V C-Star with a 4.70 V clean-water reference and
            # 0.06 V dark, oxygen left in volts, fluorometer ≈ 6.3 µg/L per volt.
            "ANLG1": ("Transmission", "% (nominal)"), "ANLG2": ("Dissolved oxygen", "V (raw)"),
            "ANLG3": ("Fluorescence", "µg/L (nominal)")}
MVP_SCALE = {"ANLG1": lambda v: 100.0 * (v / 1000.0 - 0.06) / (4.70 - 0.06),
             "ANLG2": lambda v: v / 1000.0,
             "ANLG3": lambda v: 6.3 * v / 1000.0}
BIN_DBAR = 1.0
MAX_NEW_PER_BUILD = 80        # new Rosette casts parsed per build; the rest wait for the next run
MAX_NEW_MVP_PER_BUILD = 240   # MVP dips are ~1 MB text files, so many more fit in a build
PARALLEL_READS = 8            # concurrent CIFS reads


@dataclass
class Cast:
    id: str
    leg: str
    kind: str                     # CTD | MVP
    cast: str
    time: str | None
    lat: float | None
    lon: float | None
    station: str = ""
    label: str = ""
    bottom_m: float | None = None
    p: list = field(default_factory=list)
    vars: dict = field(default_factory=dict)
    units: dict = field(default_factory=dict)
    # an MVP tow is one dataset made of many dips: each profile has its own
    # time, position, pressure grid and variables, in tow order
    profiles: list = field(default_factory=list)
    time_end: str | None = None
    lat_end: float | None = None
    lon_end: float | None = None

    def meta(self) -> dict:
        ps = self.profiles
        return {"id": self.id, "leg": self.leg, "kind": self.kind, "cast": self.cast, "time": self.time,
                "time_end": self.time_end, "lat": self.lat, "lon": self.lon, "lat_end": self.lat_end, "lon_end": self.lon_end,
                "station": self.station, "label": self.label, "bottom_m": self.bottom_m,
                "max_p": (max(self.p) if self.p else None) if not ps else max((pr["p"][-1] for pr in ps if pr["p"]), default=None),
                "n_profiles": len(ps) or None,
                "track": [[pr["lat"], pr["lon"]] for pr in ps if pr.get("lat") is not None] if ps else None,
                "vars": list(self.vars) if self.vars else sorted({v for pr in ps for v in pr["vars"]}),
                "file": f"data/casts/{self.leg}/{self.id.split(':')[-1]}.json"}

    def payload(self) -> dict:
        return {**self.meta(), "p": self.p, "vars": self.vars, "units": self.units, "profiles": self.profiles}


# ---------------------------------------------------------------- cache

def _cache_path(leg: str, key: str) -> Path:
    return DB_DIR / "casts" / leg / (key + ".json")


RECENT_DAYS = 3
# bump when the parsed representation changes so old cache entries are redone
CACHE_VERSION = 3
MVP_CACHE_VERSION = 5    # bumped when the MVP column set or scaling changes


def _cached(leg: str, key: str, sources: list[Path]):
    """A cached cast is trusted by name unless it is recent: every stat() of a
    source file costs CIFS latency, and a cast's files stop changing once its
    processing is done. Recent casts are re-validated against size and mtime
    in case they were reprocessed."""
    p = _cache_path(leg, key)
    if not p.is_file():
        return None
    try:
        c = json.loads(p.read_text())
        cast = c["cast"]
    except (OSError, json.JSONDecodeError, KeyError):
        return None
    if c.get("version") != (MVP_CACHE_VERSION if key.startswith("MVP") else CACHE_VERSION):
        return None
    t = cast.get("time") or ""
    recent = True
    if t:
        try:
            from datetime import datetime, timedelta, timezone
            when = datetime.fromisoformat(t[:19]).replace(tzinfo=timezone.utc)
            recent = datetime.now(timezone.utc) - when < timedelta(days=RECENT_DAYS)
        except ValueError:
            recent = True
    if not recent:
        return cast
    try:
        stamp = [[s.name, s.stat().st_size, int(s.stat().st_mtime)] for s in sources]
    except OSError:
        return cast
    return cast if c.get("stamp") == stamp else None


def _store(leg: str, key: str, sources: list[Path], cast: dict) -> None:
    p = _cache_path(leg, key)
    p.parent.mkdir(parents=True, exist_ok=True)
    stamp = [[s.name, s.stat().st_size, int(s.stat().st_mtime)] for s in sources]
    p.write_text(json.dumps({"version": MVP_CACHE_VERSION if key.startswith("MVP") else CACHE_VERSION, "stamp": stamp, "cast": cast}, separators=(",", ":")))


# ---------------------------------------------------------------- rosette

def _decode(a) -> np.ndarray:
    if isinstance(a, dict) and "bdata" in a:
        return np.frombuffer(base64.b64decode(a["bdata"]), dtype=np.dtype(a["dtype"])).astype(float)
    return np.asarray(a, dtype=float)


NEWPLOT_RE = re.compile(r'Plotly\.newPlot\(\s*"[^"]+"\s*,\s*(\[.*?\])\s*,\s*(\{.*?\})\s*,\s*\{', re.S)
TAIL_BYTES = 512 * 1024


def _plot_traces(path: Path) -> list[tuple[str, np.ndarray, np.ndarray]]:
    # The inlined plotly.js library fills the first ~4.8 MB; the data call is
    # in the last few tens of KB, so read the tail and fall back to the whole
    # file only if the call is not there.
    with path.open("rb") as fh:
        fh.seek(0, 2)
        size = fh.tell()
        fh.seek(max(0, size - TAIL_BYTES))
        s = fh.read().decode("utf-8", errors="replace")
    m = NEWPLOT_RE.search(s)
    if not m:
        s = path.read_text(encoding="utf-8", errors="replace")
        m = NEWPLOT_RE.search(s)
    if not m:
        return []
    out = []
    for t in json.loads(m.group(1)):
        if "x" in t and "y" in t and t.get("name"):
            out.append((t["name"], _decode(t["x"]), _decode(t["y"])))
    return out


def read_logbook(path: Path | None) -> dict[str, dict]:
    if not path or not path.is_file():
        return {}
    out = {}
    with path.open(encoding="utf-8-sig", newline="") as fh:
        for row in csv.DictReader(fh):
            row = {k.strip().lower(): (v or "").strip() for k, v in row.items() if k}
            cast = row.get("cast", "").zfill(3)
            if cast:
                out[cast] = row
    return out


# ---------------------------------------------------------------- seabird cnv

CNV_DIR = DATA_ROOT / "external_proprietary" / "CTD"
CNV_RE = re.compile(r"^CTD_(\d{4})_(\d{2})_(\d{3})\.cnv$", re.I)
# SeaBird short names -> (display, unit), in order of preference where two
# names map to one variable (the SBE 9 rosette CTD writes prDM/t090C, the
# SBE 19plus writes prdM/tv290C). Anything else in the file is ignored.
CNV_PRESSURE = ("prDM", "prdM", "prSM", "prM")
CNV_VARS = {
    "t090C": ("Temperature", "°C"), "tv290C": ("Temperature", "°C"),
    "sal00": ("Salinity", "PSU"), "sigma-t00": ("Sigma-t", "kg/m³"),
    "sbeox0Mm/L": ("Oxygen", "µM"), "oxsatML/L": ("Oxygen saturation", "mL/L"), "sbeox0ML/L": ("Oxygen (mL/L)", "mL/L"),
    "CStarTr0": ("Transmission", "%"), "flSP": ("Fluorescence", "µg/L"), "flECO-AFL": ("Fluorescence", "µg/L"),
    "wetCDOM": ("CDOM", "mg/m³"), "par": ("PAR", "µE/s/m²"), "upoly0": ("SUNA nitrate (raw)", "µM"),
    "svCM": ("Sound velocity", "m/s"),
}
CNV_BAD = -9.99e-29


def parse_cnv(path: Path) -> dict | None:
    """Downcast of a SeaBird .cnv, binned to 1 dbar. Returns the fields of a
    Cast minus id/leg/kind, or None if the file is unusable."""
    names: dict[int, str] = {}
    header: dict[str, str] = {}
    data_lines: list[str] = []
    # one read: iterating a file object fetches 8 KB per network round trip,
    # which turns a 21 MB SBE 9 file into two minutes of CIFS latency
    text = path.read_bytes().decode("latin-1", errors="replace")
    if True:
        for line in text.splitlines(keepends=True):
            if line.startswith("#"):
                m = re.match(r"#\s*name\s+(\d+)\s*=\s*([^:]+):", line)
                if m:
                    names[int(m.group(1))] = m.group(2).strip()
                elif "=" in line:
                    k, _, v = line[1:].partition("=")
                    header[k.strip()] = v.strip()
            elif line.startswith("*"):
                if "=" in line:
                    k, _, v = line[1:].partition("=")
                    header[k.strip()] = v.strip()
            else:
                data_lines.append(line)
    col = {n: i for i, n in names.items()}
    pcol = next((n for n in CNV_PRESSURE if n in col), None)
    if not data_lines or pcol is None:
        log.debug("%s: no pressure column among %s", path.name, list(col)[:8])
        return None
    # an SBE 9 file has ~65k rows of 30 columns; the C parser does this in
    # well under a second where a Python float() loop takes minutes
    import io
    import pandas as pd
    arr = pd.read_csv(io.StringIO("".join(data_lines)), sep=r"\s+", header=None, engine="c",
                      dtype=float, na_values=[], on_bad_lines="skip").to_numpy(dtype=float)
    if arr.shape[1] < len(names):
        return None
    arr = arr[:, :len(names)]
    # SeaBird's bad-value flag is the specific number -9.990e-29 (a "<=" test
    # would wipe every negative longitude)
    arr[np.isclose(arr, CNV_BAD, rtol=1e-3, atol=0)] = np.nan
    pres = arr[:, col[pcol]]
    ok = np.isfinite(pres)
    arr, pres = arr[ok], pres[ok]
    if len(pres) < 10:
        return None
    # downcast: everything up to the deepest sample
    bottom_i = int(np.nanargmax(pres))
    down = arr[: bottom_i + 1]
    dp = down[:, col[pcol]]
    bins = np.floor(dp / BIN_DBAR).astype(int)
    levels = sorted(set(bins[dp >= 0.5]))
    vars_: dict[str, list] = {}
    units: dict[str, str] = {}
    for short, (disp, unit) in CNV_VARS.items():
        if short not in col or disp in vars_:
            continue
        v = down[:, col[short]]
        vals = []
        for b in levels:
            sel = v[(bins == b) & np.isfinite(v)]
            vals.append(round(float(sel.mean()), 4) if sel.size else None)
        vars_[disp] = vals
        units[disp] = unit
    lat = float(np.nanmedian(arr[:, col["latitude"]])) if "latitude" in col else None
    lon = float(np.nanmedian(arr[:, col["longitude"]])) if "longitude" in col else None
    bottom = float(np.nanmax(arr[:, col["sfdSM"]])) if "sfdSM" in col and np.isfinite(arr[:, col["sfdSM"]]).any() else None
    t = header.get("NMEA UTC (Time)") or header.get("start_time", "").split("[")[0].strip()
    time = None
    try:
        from datetime import datetime
        time = datetime.strptime(t, "%b %d %Y %H:%M:%S").isoformat()
    except ValueError:
        pass
    return {"time": time, "lat": lat, "lon": lon, "bottom_m": bottom,
            "p": [round((b + 0.5) * BIN_DBAR, 1) for b in levels], "vars": vars_, "units": units}


def rosette_casts(leg: Leg) -> list[Cast]:
    plots = DATA_ROOT / "Rosette" / leg.id / "plots"
    files: dict[str, list[Path]] = {}
    rosette: dict[str, str] = {}
    if plots.is_dir():
        for p in plots.iterdir():
            m = PLOT_RE.match(p.name)
            if m and m.group(5) not in SKIP_PAIRS:
                files.setdefault(m.group(3), []).append(p)
                rosette[m.group(3)] = m.group(4)
    cnvs: dict[str, Path] = {}
    if CNV_DIR.is_dir():
        for p in CNV_DIR.iterdir():
            m = CNV_RE.match(p.name)
            if m and int(m.group(1)) == leg.year and int(m.group(2)) == leg.number:
                cnvs[m.group(3)] = p
    if not files and not cnvs:
        return []
    logbook = read_logbook(leg.stations)
    casts, todo = [], []
    for cast_no in sorted(set(files) | set(cnvs)):
        paths = sorted(files.get(cast_no, []))
        cnv = cnvs.get(cast_no)
        sources = ([cnv] if cnv else []) + paths
        cached = _cached(leg.id, f"CTD_{cast_no}", sources)
        if cached:
            casts.append(Cast(**cached))
        else:
            todo.append((cast_no, paths, cnv, rosette.get(cast_no, "")))
    # Each file costs seconds of CIFS latency, so new casts are parsed in
    # parallel and in batches: a build takes the first MAX_NEW_PER_BUILD and the
    # rest arrive on later runs, which keeps the underway page on its cadence.
    batch, deferred = todo[:MAX_NEW_PER_BUILD], todo[MAX_NEW_PER_BUILD:]
    if deferred:
        log.info("%s: %d casts deferred to later builds", leg.id, len(deferred))
    with ThreadPoolExecutor(max_workers=PARALLEL_READS) as ex:
        for c in ex.map(lambda item: _parse_rosette_cast(leg, item[0], item[1], item[2], logbook.get(item[0], {}), item[3]), batch):
            if c:
                casts.append(c)
    casts.sort(key=lambda c: c.cast)
    return casts


def _parse_rosette_cast(leg: Leg, cast_no: str, paths: list[Path], cnv: Path | None, lb: dict, rosette: str = "") -> Cast | None:
    """The .cnv is the profile when there is one; the plot files add the
    variables the .cnv does not carry (CDOM, PAR, buoyancy, nitrates)."""
    # which rosette: from the plot filenames, else the logbook's type_cast
    kind = ROSETTE_KIND.get(rosette.lower()) or ROSETTE_KIND.get(lb.get("type_cast", "").lower(), "CTD")
    base = None
    if cnv is not None:
        try:
            base = parse_cnv(cnv)
        except (OSError, ValueError) as e:
            log.warning("%s: cannot parse %s (%s)", leg.id, cnv.name, e)
    grid: dict[float, dict[str, float]] = {}
    units: dict[str, str] = dict(base["units"]) if base else {}
    have = set(units)
    for p in paths:
        if base and PLOT_RE.match(p.name).group(5) in ("Temperature_Salinity", "Oxygen_OxySat"):
            continue                    # already covered by the .cnv
        try:
            traces = _plot_traces(p)
        except OSError as e:
            log.warning("%s: cannot read %s (%s)", leg.id, p.name, e)
            continue
        for name, x, y in traces:
            disp, unit = ROSETTE_VARS.get(name, (name, ""))
            if disp in have:
                continue
            units[disp] = unit
            for xi, yi in zip(x, y):
                if np.isfinite(xi) and np.isfinite(yi):
                    grid.setdefault(round(float(yi), 1), {})[disp] = float(xi)
    if base:
        pres = base["p"]
        vars_ = dict(base["vars"])
        # plot variables are binned to the same 1 dbar grid; align by nearest level
        extra = [v for v in units if v not in vars_]
        for v in extra:
            vars_[v] = [None] * len(pres)
        for pp, vals in grid.items():
            i = int(round(pp - 0.5))          # plot levels are the bin centres (3.0, 4.0, ...)
            j = min(range(len(pres)), key=lambda k: abs(pres[k] - pp)) if pres else None
            if j is not None and abs(pres[j] - pp) <= BIN_DBAR:
                for v in extra:
                    if v in vals:
                        vars_[v][j] = round(vals[v], 4)
    else:
        if not grid:
            return None
        pres = sorted(grid)
        vars_ = {v: [None if grid[pp].get(v) is None else round(grid[pp][v], 4) for pp in pres] for v in units}

    def num(k):
        try:
            return float(lb[k])
        except (KeyError, ValueError):
            return None
    c = Cast(id=f"{leg.id}:CTD_{cast_no}", leg=leg.id, kind=kind, cast=cast_no,
             time=lb.get("date_utc") or (base or {}).get("time"),
             lat=num("latitude") if num("latitude") is not None else (base or {}).get("lat"),
             lon=num("longitude") if num("longitude") is not None else (base or {}).get("lon"),
             station=lb.get("station", ""), label=lb.get("label", ""),
             bottom_m=num("bottom_m") if num("bottom_m") is not None else (base or {}).get("bottom_m"),
             p=[round(x, 1) for x in pres], vars=vars_, units=units)
    _store(leg.id, f"CTD_{cast_no}", ([cnv] if cnv else []) + paths, c.__dict__)
    log.info("%s: parsed rosette cast %s (%d levels, %d vars, %s)", leg.id, cast_no, len(pres), len(units),
             "cnv+plots" if base and paths else "cnv" if base else "plots")
    return c


# ---------------------------------------------------------------- mvp

def _no_fix(lat, lon) -> bool:
    """The MVP deck unit writes 45 00.000 N, 000 00.000 E while it has no GPS fix."""
    return lat is not None and lon is not None and abs(lat - 45.0) < 1e-6 and abs(lon) < 1e-6


def _fill_positions(ds: list) -> None:
    """Give dips without a fix a position interpolated (by time) between the
    nearest dips of the tow that have one; ends take the nearest fix."""
    for d in ds:
        if _no_fix(d.lat, d.lon):
            d.lat = d.lon = None
    idx = [i for i, d in enumerate(ds) if d.lat is not None and d.lon is not None]
    if not idx or len(idx) == len(ds):
        return
    t = [datetime.fromisoformat(d.time).timestamp() if d.time else i for i, d in enumerate(ds)]
    for i, d in enumerate(ds):
        if d.lat is not None:
            continue
        before = [j for j in idx if j < i]; after = [j for j in idx if j > i]
        if before and after:
            a, b = before[-1], after[0]
            f = (t[i] - t[a]) / (t[b] - t[a]) if t[b] != t[a] else 0.5
            d.lat = round(ds[a].lat + (ds[b].lat - ds[a].lat) * f, 6)
            d.lon = round(ds[a].lon + (ds[b].lon - ds[a].lon) * f, 6)
        else:
            n = ds[before[-1] if before else after[0]]
            d.lat, d.lon = n.lat, n.lon


def _ddmm(s: str) -> float | None:
    m = re.match(r"\s*(\d+)(\d{2}\.\d+)\s*,\s*([NSEW])", s)
    if not m:
        return None
    v = int(m.group(1)) + float(m.group(2)) / 60.0
    return -v if m.group(3) in "SW" else v


def mvp_casts(leg: Leg) -> list[Cast]:
    """One Cast per tow directory, holding every dip of the tow as a profile."""
    root = DATA_ROOT / "MVP" / leg.id
    if not root.is_dir():
        return []
    dips: list[Cast] = []
    todo: list[Path] = []
    for p in sorted(root.glob("*/*.m1")):
        cached = _cached(leg.id, "MVP_" + p.stem, [p])
        if cached:
            dips.append(Cast(**cached))
        else:
            todo.append(p)
    batch, deferred = todo[:MAX_NEW_MVP_PER_BUILD], todo[MAX_NEW_MVP_PER_BUILD:]
    if deferred:
        log.info("%s: %d MVP dips deferred to later builds", leg.id, len(deferred))
    with ThreadPoolExecutor(max_workers=PARALLEL_READS) as ex:
        for c in ex.map(lambda p: _parse_mvp(leg, p), batch):
            if c:
                dips.append(c)
    tows: dict[str, list[Cast]] = {}
    for d in dips:
        tows.setdefault(d.station, []).append(d)
    out = []
    for tow, ds in sorted(tows.items()):
        ds.sort(key=lambda c: c.time or "")
        _fill_positions(ds)
        units = {}
        for d in ds:
            units.update(d.units)
        first, last = ds[0], ds[-1]
        out.append(Cast(id=f"{leg.id}:MVP_{tow}", leg=leg.id, kind="MVP", cast=tow, time=first.time, time_end=last.time,
                        lat=first.lat, lon=first.lon, lat_end=last.lat, lon_end=last.lon, station=tow, label=f"{len(ds)} dips",
                        bottom_m=max((d.bottom_m for d in ds if d.bottom_m is not None and d.bottom_m > 0), default=None), units=units,
                        profiles=[{"time": d.time, "lat": d.lat, "lon": d.lon, "bottom_m": d.bottom_m if d.bottom_m is not None and d.bottom_m > 0 else None, "p": d.p, "vars": d.vars} for d in ds]))
    if out:
        log.info("%s: %d MVP tows (%d dips)", leg.id, len(out), len(dips))
    return out


def _parse_mvp(leg: Leg, p: Path) -> Cast | None:
    key = "MVP_" + p.stem
    try:
        lines = p.read_bytes().decode("latin-1", errors="replace").splitlines()   # one CIFS read, see parse_cnv
    except OSError:
        return None
    hdr = {}
    data_at = None
    for i, line in enumerate(lines):
        if line.lstrip().startswith("Press,"):
            data_at = i
            break
        if ":" in line:
            k, _, v = line.partition(":")
            hdr[k.strip()] = v.strip()
    if data_at is None:
        return None
    cols = [c.strip() for c in lines[data_at].split(",")]
    rows = []
    for line in lines[data_at + 1:]:
        parts = line.split(",")
        if len(parts) != len(cols):
            continue
        try:
            rows.append([float(x) for x in parts])
        except ValueError:
            continue
    if len(rows) < 10:
        return None
    arr = np.array(rows)
    ci = {c: i for i, c in enumerate(cols)}
    pres = arr[:, ci["Press"]]
    # downcast only: samples up to the deepest point
    down = arr[: int(np.argmax(pres)) + 1]
    pres = down[:, ci["Press"]]
    bins = np.floor(pres / BIN_DBAR).astype(int)
    levels = sorted(set(bins[pres > 0.5]))
    vars_, units = {}, {}
    for col, (disp, unit) in MVP_VARS.items():
        if col not in ci:
            continue
        v = down[:, ci[col]]
        if col in MVP_SCALE:
            v = MVP_SCALE[col](v.astype(float))
        vals = []
        for b in levels:
            sel = v[(bins == b) & np.isfinite(v)]
            vals.append(round(float(sel.mean()), 4) if sel.size else None)
        vars_[disp] = vals
        units[disp] = unit
    lat = next((_ddmm(v) for k, v in hdr.items() if k.startswith("LAT")), None)
    lon = next((_ddmm(v) for k, v in hdr.items() if k.startswith("LON")), None)
    if _no_fix(lat, lon):
        lat = lon = None
    date = hdr.get("Date (dd/mm/yyyy)", ""); tm = hdr.get("Time (hh|mm|ss.s)", "")
    time = None
    m = re.match(r"(\d{2})/(\d{2})/(\d{4})", date)
    if m:
        time = f"{m.group(3)}-{m.group(2)}-{m.group(1)}T{tm[:8] or '00:00:00'}"
    try:
        bottom = float(hdr.get("Bottom Depth (m)", ""))
    except ValueError:
        bottom = None
    if bottom is not None and bottom <= 0:       # -99999.9 when the sounder had no bottom
        bottom = None
    c = Cast(id=f"{leg.id}:{key}", leg=leg.id, kind="MVP", cast=p.parent.name + "/" + p.stem, time=time,
             lat=lat, lon=lon, station=p.parent.name, label="", bottom_m=bottom,
             p=[round((b + 0.5) * BIN_DBAR, 1) for b in levels], vars=vars_, units=units)
    _store(leg.id, key, [p], c.__dict__)
    return c


# ---------------------------------------------------------------- output

def build_casts(legs: list[Leg], root: Path) -> dict:
    """Write per-cast JSON files and an index; return the index."""
    from .build import atomic_write   # local import: build imports this module
    (root / "data" / "casts").mkdir(parents=True, exist_ok=True)
    index = []
    for leg in legs:
        casts = rosette_casts(leg) + mvp_casts(leg)
        if not casts:
            continue
        out = root / "data" / "casts" / leg.id
        out.mkdir(parents=True, exist_ok=True)
        for c in casts:
            atomic_write(out / (c.id.split(":")[-1] + ".json"), json.dumps(c.payload(), separators=(",", ":")))
            index.append(c.meta())
    index.sort(key=lambda m: (m["time"] or "", m["id"]))
    idx = {"casts": index, "variables": sorted({v for m in index for v in m["vars"]})}
    atomic_write(root / "data" / "casts" / "index.json", json.dumps(idx, separators=(",", ":")))
    log.info("casts: %d (%d rosette, %d TM, %d MVP tows)", len(index), sum(m["kind"] == "CTD" for m in index),
             sum(m["kind"] == "TM" for m in index), sum(m["kind"] == "MVP" for m in index))
    return idx
