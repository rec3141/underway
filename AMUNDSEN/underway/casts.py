"""CTD profiles for the cast viewer.

Two sources:

* Rosette casts. Amundsen Science publishes each cast as five Plotly HTML
  files under ``Data/Rosette/<leg>/plots/`` (Temperature/Salinity,
  Oxygen/OxySat, Fluorescence/Transmission, Cdom/Par, Buoyancy/Sigma-t), with
  the 1-dbar binned profile embedded as base64 float arrays. No .cnv is
  shared, so the plots are the profile source. Position, time and station
  come from the leg's CTD logbook.
* MVP (Moving Vessel Profiler) tows: ``Data/MVP/<leg>/<tow>/*.m1``, an AML
  CTD text format with the fix in the header and one row per sample.

Each 5 MB plot file is parsed once and cached as JSON under ``DB_DIR/casts``,
keyed by size and mtime, so a rebuild only touches new casts.
"""

from __future__ import annotations

import base64
import csv
import json
import logging
import re
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np

from .config import DATA_ROOT, DB_DIR
from .legs import Leg

log = logging.getLogger(__name__)

PLOT_RE = re.compile(r"^CTD_(\d{4})_(\d{2})_(\d{3})_(?:[A-Za-z]+)_(.+)_raw_data\.html$")
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
            "SV": ("Sound velocity", "m/s")}
BIN_DBAR = 1.0
MAX_NEW_PER_BUILD = 40        # new casts parsed per build; the rest wait for the next run
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

    def meta(self) -> dict:
        return {"id": self.id, "leg": self.leg, "kind": self.kind, "cast": self.cast, "time": self.time,
                "lat": self.lat, "lon": self.lon, "station": self.station, "label": self.label,
                "bottom_m": self.bottom_m, "max_p": max(self.p) if self.p else None,
                "vars": list(self.vars), "file": f"data/casts/{self.leg}/{self.id.split(':')[-1]}.json"}

    def payload(self) -> dict:
        return {**self.meta(), "p": self.p, "vars": self.vars, "units": self.units}


# ---------------------------------------------------------------- cache

def _cache_path(leg: str, key: str) -> Path:
    return DB_DIR / "casts" / leg / (key + ".json")


RECENT_DAYS = 3


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
    p.write_text(json.dumps({"stamp": stamp, "cast": cast}, separators=(",", ":")))


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


def rosette_casts(leg: Leg) -> list[Cast]:
    plots = DATA_ROOT / "Rosette" / leg.id / "plots"
    if not plots.is_dir():
        return []
    files: dict[str, list[Path]] = {}
    for p in plots.iterdir():
        m = PLOT_RE.match(p.name)
        if m and m.group(4) not in SKIP_PAIRS:
            files.setdefault(m.group(3), []).append(p)
    logbook = read_logbook(leg.stations)
    casts, todo = [], []
    for cast_no, paths in sorted(files.items()):
        paths.sort()
        cached = _cached(leg.id, f"CTD_{cast_no}", paths)
        if cached:
            casts.append(Cast(**cached))
        else:
            todo.append((cast_no, paths))
    # Each file costs seconds of CIFS latency, so new casts are parsed in
    # parallel and in batches: a build takes the first MAX_NEW_PER_BUILD and the
    # rest arrive on later runs, which keeps the underway page on its cadence.
    batch, deferred = todo[:MAX_NEW_PER_BUILD], todo[MAX_NEW_PER_BUILD:]
    if deferred:
        log.info("%s: %d casts deferred to later builds", leg.id, len(deferred))
    with ThreadPoolExecutor(max_workers=PARALLEL_READS) as ex:
        for c in ex.map(lambda item: _parse_rosette_cast(leg, item[0], item[1], logbook.get(item[0], {})), batch):
            if c:
                casts.append(c)
    casts.sort(key=lambda c: c.cast)
    return casts


def _parse_rosette_cast(leg: Leg, cast_no: str, paths: list[Path], lb: dict) -> Cast | None:
    grid: dict[float, dict[str, float]] = {}
    units: dict[str, str] = {}
    for p in paths:
        try:
            traces = _plot_traces(p)
        except OSError as e:
            log.warning("%s: cannot read %s (%s)", leg.id, p.name, e)
            continue
        for name, x, y in traces:
            disp, unit = ROSETTE_VARS.get(name, (name, ""))
            units[disp] = unit
            for xi, yi in zip(x, y):
                if np.isfinite(xi) and np.isfinite(yi):
                    grid.setdefault(round(float(yi), 1), {})[disp] = float(xi)
    if not grid:
        return None
    pres = sorted(grid)
    vars_ = {v: [grid[pp].get(v) for pp in pres] for v in units}

    def num(k):
        try:
            return float(lb[k])
        except (KeyError, ValueError):
            return None
    c = Cast(id=f"{leg.id}:CTD_{cast_no}", leg=leg.id, kind="CTD", cast=cast_no,
             time=lb.get("date_utc") or None, lat=num("latitude"), lon=num("longitude"),
             station=lb.get("station", ""), label=lb.get("label", ""), bottom_m=num("bottom_m"),
             p=[round(x, 1) for x in pres], vars={k: [None if v is None else round(v, 4) for v in vv] for k, vv in vars_.items()},
             units=units)
    _store(leg.id, f"CTD_{cast_no}", paths, c.__dict__)
    log.info("%s: parsed rosette cast %s (%d levels, %d vars)", leg.id, cast_no, len(pres), len(units))
    return c


# ---------------------------------------------------------------- mvp

def _ddmm(s: str) -> float | None:
    m = re.match(r"\s*(\d+)(\d{2}\.\d+)\s*,\s*([NSEW])", s)
    if not m:
        return None
    v = int(m.group(1)) + float(m.group(2)) / 60.0
    return -v if m.group(3) in "SW" else v


def mvp_casts(leg: Leg) -> list[Cast]:
    root = DATA_ROOT / "MVP" / leg.id
    if not root.is_dir():
        return []
    casts, todo = [], []
    for p in sorted(root.glob("*/*.m1")):
        key = "MVP_" + p.stem
        cached = _cached(leg.id, key, [p])
        if cached:
            casts.append(Cast(**cached))
        else:
            todo.append(p)
    batch, deferred = todo[:MAX_NEW_PER_BUILD], todo[MAX_NEW_PER_BUILD:]
    if deferred:
        log.info("%s: %d MVP profiles deferred to later builds", leg.id, len(deferred))
    with ThreadPoolExecutor(max_workers=PARALLEL_READS) as ex:
        for c in ex.map(lambda p: _parse_mvp(leg, p), batch):
            if c:
                casts.append(c)
    casts.sort(key=lambda c: c.time or "")
    if casts:
        log.info("%s: %d MVP profiles", leg.id, len(casts))
    return casts


def _parse_mvp(leg: Leg, p: Path) -> Cast | None:
    key = "MVP_" + p.stem
    try:
        lines = p.read_text(encoding="latin-1", errors="replace").splitlines()
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
        vals = []
        for b in levels:
            sel = v[(bins == b) & np.isfinite(v)]
            vals.append(round(float(sel.mean()), 4) if sel.size else None)
        vars_[disp] = vals
        units[disp] = unit
    lat = next((_ddmm(v) for k, v in hdr.items() if k.startswith("LAT")), None)
    lon = next((_ddmm(v) for k, v in hdr.items() if k.startswith("LON")), None)
    date = hdr.get("Date (dd/mm/yyyy)", ""); tm = hdr.get("Time (hh|mm|ss.s)", "")
    time = None
    m = re.match(r"(\d{2})/(\d{2})/(\d{4})", date)
    if m:
        time = f"{m.group(3)}-{m.group(2)}-{m.group(1)}T{tm[:8] or '00:00:00'}"
    try:
        bottom = float(hdr.get("Bottom Depth (m)", ""))
    except ValueError:
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
    log.info("casts: %d (%d CTD, %d MVP)", len(index), sum(m["kind"] == "CTD" for m in index),
             sum(m["kind"] == "MVP" for m in index))
    return idx
