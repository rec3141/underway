"""Conditions at each operation and each station visit, as a table and as prose.

Each value names its source, because the sources disagree and a reader needs
to know which one they are looking at:

* bridge — the event log row for the Deployment (met tower, TSG and echo
  sounder as the bridge logged them)
* underway — the median of the ACSD record around the Deployment
* observer — the rosette console's sheet for the same station within
  ``OBSERVER_WINDOW_H`` (cloud cover, sea state, ice as seen from deck)
* CIS — the Canadian Ice Service chart for that date (with its age)
* computed — solar elevation from time and position

Bottom depth prefers the multibeam: the EK60 and the event log (which copies
it) pick a second-bottom return in some places, e.g. about 300 m for 180 m
of water at CardS-3 on 2026 Leg 3. A disagreement of more than
``DEPTH_DISAGREE`` is written into the notes rather than resolved silently.

The narrative states only what was recorded. A missing value is listed as
not recorded; it is never filled from a neighbouring source without saying so.
"""

from __future__ import annotations

import math
import threading
import time
from dataclasses import dataclass

import pandas as pd

from . import activities, eventlog, ice, rosette, underway

OBSERVER_WINDOW_H = 6
DEPTH_DISAGREE = 0.10
VISIT_GAP_H = 12

COMPASS = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
           "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]


@dataclass(frozen=True)
class Column:
    key: str
    label: str
    unit: str = ""
    digits: int | None = None
    default: bool = False


# Columns of the conditions table, in display order.
COLUMNS = [
    Column("station", "Station", default=True),
    Column("label", "Event label", default=True),
    Column("activity", "Operation", default=True),
    Column("start_utc", "Start (UTC)", default=True),
    Column("end_utc", "End (UTC)"),
    Column("duration_min", "Duration", "min", 0),
    Column("lat", "Latitude", "°N", 4, default=True),
    Column("lon", "Longitude", "°E", 4, default=True),
    Column("depth_m", "Bottom depth", "m", 0, default=True),
    Column("depth_source", "Depth source"),
    Column("air_c", "Air temperature", "°C", 1, default=True),
    Column("wind_dir_deg", "Wind from", "°", 0, default=True),
    Column("wind_kn", "Wind speed", "kn", 1, default=True),
    Column("pressure_hpa", "Pressure", "hPa", 1),
    Column("humidity_pct", "Humidity", "%", 0),
    Column("visibility_km", "Visibility", "km", 1),
    Column("sst_c", "SST", "°C", 2, default=True),
    Column("sss", "SSS", "PSU", 2, default=True),
    Column("fluo_ugl", "Surface fluorescence", "µg/L", 2),
    Column("o2_mll", "Surface O₂", "mL/L", 2),
    Column("ice", "Ice", default=True),
    Column("ice_source", "Ice source"),
    Column("cloud_cover", "Cloud cover (sheet code)"),
    Column("sea_state", "Sea state (sheet code)"),
    Column("sun_elev_deg", "Sun elevation", "°", 0),
    Column("notes", "Notes"),
]
BY_KEY = {c.key: c for c in COLUMNS}


def compass(deg: float | None) -> str | None:
    if deg is None:
        return None
    return COMPASS[int((deg % 360) / 22.5 + 0.5) % 16]


def sun_elevation(when: str, lat: float | None, lon: float | None) -> float | None:
    """Solar elevation in degrees (NOAA low-precision formulae, ±0.5°)."""
    if lat is None or lon is None:
        return None
    t = pd.Timestamp(when)
    t = t.tz_localize("UTC") if t.tzinfo is None else t.tz_convert("UTC")
    doy = t.dayofyear
    hour = t.hour + t.minute / 60 + t.second / 3600
    g = 2 * math.pi / 365 * (doy - 1 + (hour - 12) / 24)
    eqtime = 229.18 * (0.000075 + 0.001868 * math.cos(g) - 0.032077 * math.sin(g)
                       - 0.014615 * math.cos(2 * g) - 0.040849 * math.sin(2 * g))
    decl = (0.006918 - 0.399912 * math.cos(g) + 0.070257 * math.sin(g)
            - 0.006758 * math.cos(2 * g) + 0.000907 * math.sin(2 * g)
            - 0.002697 * math.cos(3 * g) + 0.00148 * math.sin(3 * g))
    tst = hour * 60 + eqtime + 4 * lon
    ha = math.radians(tst / 4 - 180)
    phi = math.radians(lat)
    cz = math.sin(phi) * math.sin(decl) + math.cos(phi) * math.cos(decl) * math.cos(ha)
    return math.degrees(math.asin(max(-1.0, min(1.0, cz))))


def _first(*pairs):
    """The first (value, source) whose value is present."""
    for v, src in pairs:
        if v is not None and v != "":
            return v, src
    return None, None


def _observer_sheet(op_summary: dict, sheets: list[dict]) -> dict | None:
    """The rosette sheet for this operation, or for the same station within the window."""
    by_label = next((s for s in sheets if s.get("label") and s["label"] == op_summary["label"]), None)
    if by_label:
        return by_label
    st = (op_summary.get("station") or "").casefold()
    if not st:
        return None
    t0 = pd.Timestamp(op_summary["start_utc"])
    best = None
    for s in sheets:
        if (s.get("station") or "").casefold() != st or not s.get("_time"):
            continue
        dt = abs((pd.Timestamp(s["_time"]) - t0).total_seconds()) / 3600
        if dt <= OBSERVER_WINDOW_H and (best is None or dt < best[0]):
            best = (dt, s)
    return best[1] if best else None


def _sheet_times(leg: str, ops: dict[str, eventlog.Operation]) -> list[dict]:
    """Rosette sheets with a time, taken from the event log via the label."""
    out = []
    for s in rosette.sheets(leg):
        op = ops.get(s.get("label") or "")
        out.append({**s, "_time": op.start.time if op else None})
    return out


def _code(v):
    return None if v in (None, "-99") else v


def for_operation(op: eventlog.Operation, sheets: list[dict]) -> dict:
    s = op.summary()
    br = op.start.readings
    uw = underway.around(op.leg, s["start_utc"]) or {}
    obs_sheet = _observer_sheet(s, sheets)
    obs = (obs_sheet or {}).get("observed", {})
    notes = []

    mb, ek, bridge_depth = uw.get("mb_depth_m"), uw.get("ek60_depth_m"), br.get("depth_m")
    if bridge_depth is None:
        bridge_depth = next((e.readings.get("depth_m") for e in op.events
                             if e.readings.get("depth_m") is not None), None)
    depth, depth_src = _first((mb, "multibeam"), (ek, "EK60"), (bridge_depth, "bridge"),
                              ((obs_sheet or {}).get("bottom_depth_m"), "rosette sheet"))
    others = [(n, v) for n, v in (("EK60", ek), ("bridge log", bridge_depth)) if v is not None]
    if depth_src == "multibeam" and any(abs(v - mb) > DEPTH_DISAGREE * mb for _, v in others):
        notes.append(f"depth sources disagree (multibeam {mb:.0f} m, "
                     + ", ".join(f"{n} {v:.0f} m" for n, v in others) + ")")

    air, air_src = _first((br.get("air_c"), "bridge"), (uw.get("air_c"), "underway"),
                          (obs.get("air_c"), "observer"))
    wdir, _ = _first((br.get("wind_dir_deg"), "bridge"), (uw.get("true_wind_dir_deg"), "underway"),
                     (obs.get("wind_dir_deg"), "observer"))
    wspd, _ = _first((br.get("wind_kn"), "bridge"), (uw.get("true_wind_kn"), "underway"),
                     (obs.get("wind_kn"), "observer"))
    pres, _ = _first((br.get("pressure_hpa"), "bridge"), (uw.get("pressure_hpa"), "underway"),
                     (obs.get("pressure_hpa"), "observer"))
    hum, _ = _first((br.get("humidity_pct"), "bridge"), (uw.get("humidity_pct"), "underway"),
                    (obs.get("humidity_pct"), "observer"))
    sst, _ = _first((br.get("water_c"), "bridge"), (uw.get("sst_c"), "underway"))
    sss, _ = _first((br.get("water_sal"), "bridge"), (uw.get("sss"), "underway"))
    if uw.get("tsg_pump_off"):
        notes.append("TSG intake pump off or restricted: surface-water values are unreliable")

    chart = ice.at(s["lat"], s["lon"], s["start_utc"])
    bridge_ice = br.get("ice_tenths")
    if bridge_ice is not None:
        ice_txt, ice_src = f"{bridge_ice:g}/10", "bridge"
    elif rosette.ice_english(obs.get("ice")):
        ice_txt, ice_src = rosette.ice_english(obs.get("ice")), "observer"
    elif _code(obs.get("ice")):
        ice_txt, ice_src = f"“{obs['ice']}” (as entered)", "observer, as entered"
    elif chart:
        ice_txt = chart["concentration"] + (f" ({chart['stage']})" if chart.get("stage")
                                            and chart["stage"] != "Not reported" else "")
        ice_src = f"CIS chart {chart['chart_date']}"
    else:
        ice_txt, ice_src = None, None

    vis = uw.get("visibility_m")
    return {
        **s,
        "depth_m": depth, "depth_source": depth_src,
        "air_c": air, "air_source": air_src,
        "wind_dir_deg": wdir, "wind_kn": wspd,
        "pressure_hpa": pres, "humidity_pct": hum,
        "visibility_km": vis / 1000 if vis is not None else None,
        "precip_mm_h": uw.get("precip_mm_h"),
        "sst_c": sst, "sss": sss,
        "fluo_ugl": uw.get("fluo_ugl"), "o2_mll": uw.get("o2_mll"),
        "tsg_pump_off": bool(uw.get("tsg_pump_off")),
        "ice": ice_txt, "ice_source": ice_src, "ice_chart": chart,
        "cloud_cover": _code(obs.get("cloud_cover")), "sea_state": _code(obs.get("sea_state")),
        "observer_sheet": (obs_sheet or {}).get("file"),
        "sun_elev_deg": sun_elevation(s["start_utc"], s["lat"], s["lon"]),
        "notes": "; ".join(notes) or None,
        "note_list": notes,
    }


# Figures, tables and the narrative all ask for the same operations within a
# few seconds; one answer serves them for CACHE_S (the underway record grows,
# so it is not kept longer).
CACHE_S = 300
_cache: dict[tuple, tuple[float, list[dict]]] = {}
_cache_lock = threading.Lock()


def table(leg: str, keys: list[str]) -> list[dict]:
    ops = eventlog.by_key(leg)
    ident = (leg, tuple(sorted(k for k in keys if k in ops)), eventlog.path_for(leg).stat().st_mtime)
    now = time.monotonic()
    with _cache_lock:
        hit = _cache.get(ident)
        if hit and now - hit[0] < CACHE_S:
            return [dict(r) for r in hit[1]]
    sheets = _sheet_times(leg, ops)
    rows = sorted((for_operation(ops[k], sheets) for k in ident[1]), key=lambda r: r["start_utc"])
    with _cache_lock:
        for k in [k for k, (t, _) in _cache.items() if now - t >= CACHE_S]:
            del _cache[k]
        _cache[ident] = (now, rows)
    return [dict(r) for r in rows]


def visits(rows: list[dict]) -> list[list[dict]]:
    """Operations grouped into station visits: same station, no gap over VISIT_GAP_H."""
    out: list[list[dict]] = []
    last: dict[str, list[dict]] = {}
    for r in sorted(rows, key=lambda r: r["start_utc"]):
        st = r.get("station") or r["key"]
        cur = last.get(st)
        if cur and (pd.Timestamp(r["start_utc"]) - pd.Timestamp(cur[-1]["end_utc"])) \
                < pd.Timedelta(hours=VISIT_GAP_H):
            cur.append(r)
        else:
            cur = [r]
            out.append(cur)
            last[st] = cur
    return out


# --- prose --------------------------------------------------------------------

def _fmt(v, digits=1):
    if v is None:
        return None
    s = f"{v:.{digits}f}"
    if float(s) == 0:
        s = s.lstrip("-")
    return s.replace("-", "−")


def _range(vals, digits, unit):
    vals = [v for v in vals if v is not None]
    if not vals:
        return None
    sep = "" if unit == "°" else " "
    lo, hi = min(vals), max(vals)
    if _fmt(lo, digits) == _fmt(hi, digits):
        return f"{_fmt(lo, digits)}{sep}{unit}".strip()
    return f"{_fmt(lo, digits)} to {_fmt(hi, digits)}{sep}{unit}".strip()


def _mean_dir(degs: list[float]) -> tuple[float, float] | None:
    """Circular mean and the largest departure from it, in degrees."""
    if not degs:
        return None
    r = [math.radians(d) for d in degs]
    m = math.degrees(math.atan2(sum(map(math.sin, r)), sum(map(math.cos, r)))) % 360
    spread = max(abs((d - m + 180) % 360 - 180) for d in degs)
    return m, spread


def _latlon(lat, lon):
    if lat is None or lon is None:
        return None
    return (f"{abs(lat):.3f}°{'N' if lat >= 0 else 'S'}, "
            f"{abs(lon):.3f}°{'E' if lon >= 0 else 'W'}")


def _day(ts: str) -> str:
    t = pd.Timestamp(ts)
    return f"{t.day} {t.strftime('%B %Y')}"


def _ops_phrase(visit: list[dict]) -> str:
    counts: dict[str, int] = {}
    for r in visit:
        counts[r["group"]] = counts.get(r["group"], 0) + 1
    parts = [activities.noun(g, n) for g, n in counts.items()]
    return parts[0] if len(parts) == 1 else ", ".join(parts[:-1]) + " and " + parts[-1]


def narrative(visit: list[dict]) -> str:
    first, last = visit[0], visit[-1]
    station = first.get("station") or "An unnamed position"
    start, end = pd.Timestamp(first["start_utc"]), pd.Timestamp(last["end_utc"])
    when = (f"on {_day(first['start_utc'])} between {start:%H:%M} and {end:%H:%M} UTC"
            if start.date() == end.date()
            else f"from {start:%H:%M} UTC on {_day(first['start_utc'])} "
                 f"to {end:%H:%M} UTC on {_day(last['end_utc'])}")
    where = _latlon(first["lat"], first["lon"])
    out = [f"{station}{f' ({where})' if where else ''} was sampled {when}, "
           f"with {_ops_phrase(visit)}."]

    depths = [(r["depth_m"], r["depth_source"]) for r in visit if r["depth_m"] is not None]
    if depths:
        d = [v for v, _ in depths]
        src = depths[0][1]
        out.append(f"Water depth was {_range(d, 0, 'm')} ({src}).")

    met = []
    air = _range([r["air_c"] for r in visit], 1, "°C")
    if air:
        met.append(f"air temperature was {air}")
    winds = [(r["wind_dir_deg"], r["wind_kn"]) for r in visit if r["wind_kn"] is not None]
    if winds:
        spd = _range([w[1] for w in winds], 0, "kn")
        md = _mean_dir([w[0] for w in winds if w[0] is not None])
        frm = ("" if md is None else f"from the {compass(md[0])} " if md[1] <= 45
               else "from variable directions ")
        met.append(f"winds were {frm}at {spd}")
    pres = _range([r["pressure_hpa"] for r in visit], 0, "hPa")
    if pres:
        met.append(f"sea-level pressure was {pres}")
    vis = [r["visibility_km"] for r in visit if r["visibility_km"] is not None]
    if vis and min(vis) < 2:
        met.append(f"visibility dropped to {_fmt(min(vis), 1)} km")
    if met:
        s = "; ".join(met)
        out.append(s[0].upper() + s[1:] + ".")

    sea = []
    pump = any(r.get("tsg_pump_off") for r in visit)
    sst = _range([r["sst_c"] for r in visit], 2, "°C")
    sss = _range([r["sss"] for r in visit], 2, "")
    if sst:
        sea.append(f"sea-surface temperature {sst}")
    if sss:
        sea.append(f"salinity {sss}")
    if sea:
        out.append("At the ship's intake, " + " and ".join(sea) + "."
                   + (" The intake pump was off or restricted during part of the visit, so "
                      "these surface values are unreliable." if pump else ""))

    ices = [(r["ice"], r["ice_source"]) for r in visit if r["ice"]]
    if ices:
        txt, src = ices[0]
        if src == "bridge":
            out.append(f"The bridge logged ice at {txt}.")
        elif src == "observer":
            out.append(f"The rosette-sheet observer recorded {txt}.")
        elif src == "observer, as entered":
            out.append(f"The rosette sheet's ice field reads {txt.replace(' (as entered)', '')}, "
                       "with no scale stated.")
        else:
            chart = first.get("ice_chart") or {}
            age = chart.get("age_days")
            out.append(f"The Canadian Ice Service chart of {chart.get('chart_date')} "
                       f"({'same day' if age == 0 else f'{age} days earlier'}) shows "
                       f"{txt} ice concentration at the station.")
    sun = [r["sun_elev_deg"] for r in visit if r["sun_elev_deg"] is not None]
    if sun:
        lo, hi = min(sun), max(sun)
        if hi < -6:
            out.append("Operations took place in darkness (sun more than 6° below the horizon).")
        elif lo > 0:
            out.append(f"The sun was {_range([lo, hi], 0, '°')} above the horizon.")
        else:
            out.append(f"Solar elevation was {_range([lo, hi], 0, '°')}"
                       f"{' (twilight)' if hi < 0 else ''}.")

    missing = [name for name, present in (
        ("water depth", depths), ("air temperature", air), ("wind", winds),
        ("sea-surface temperature", sst), ("ice", ices)) if not present]
    if missing:
        out.append("Not recorded: " + ", ".join(missing) + ".")
    notes = sorted({n for r in visit for n in r.get("note_list", []) if "TSG intake" not in n})
    if notes:
        out.append("Note: " + "; ".join(notes) + ".")
    return " ".join(out)


def _span(vals, digits, unit):
    vals = [v for v in vals if v is not None]
    return _range(vals, digits, unit) if vals else None


def summary(rows: list[dict]) -> str:
    """One paragraph over every selected operation: the ranges, not the stations.

    The template caps a report at 3000 words, which per-station prose for a
    leg of rosette casts alone can exceed; the per-station values belong in
    the conditions table.
    """
    if not rows:
        return ""
    vs = visits(rows)
    stations = {r.get("station") for r in rows if r.get("station")}
    t0 = pd.Timestamp(min(r["start_utc"] for r in rows))
    t1 = pd.Timestamp(max(r["end_utc"] for r in rows))
    counts: dict[str, int] = {}
    for r in rows:
        counts[r["group"]] = counts.get(r["group"], 0) + 1
    ops = [activities.noun(g, n) for g, n in counts.items()]
    ops_txt = ops[0] if len(ops) == 1 else ", ".join(ops[:-1]) + " and " + ops[-1]
    when = (f"on {_day(str(t0))}" if t0.date() == t1.date()
            else f"between {t0.day} {t0:%B} and {_day(str(t1))}" if t0.year == t1.year
            else f"between {_day(str(t0))} and {_day(str(t1))}")
    lats = [r["lat"] for r in rows if r.get("lat") is not None]
    lons = [r["lon"] for r in rows if r.get("lon") is not None]
    out = [f"The team's {ops_txt} took place {when}, over {len(vs)} station visits "
           f"({len(stations)} stations)"
           + (f" between {_fmt(min(lats), 1)}° and {_fmt(max(lats), 1)}°N, "
              f"{_fmt(abs(max(lons)), 1)}° and {_fmt(abs(min(lons)), 1)}°"
              f"{'W' if max(lons) < 0 else 'E'}" if lats and lons else "") + "."]
    d = _span([r["depth_m"] for r in rows], 0, "m")
    if d:
        out.append(f"Water depth ranged from {d.replace(' to ', ' to ')}.")
    met = []
    air = _span([r["air_c"] for r in rows], 1, "°C")
    if air:
        met.append(f"air temperature ranged from {air}")
    w = [r["wind_kn"] for r in rows if r["wind_kn"] is not None]
    if w:
        met.append(f"wind speed from {_range(w, 0, 'kn')} "
                   f"(median {_fmt(float(pd.Series(w).median()), 0)} kn)")
    if met:
        out.append("At the start of operations, " + " and ".join(met) + ".")
    sea = []
    sst = _span([r["sst_c"] for r in rows if not r.get("tsg_pump_off")], 2, "°C")
    sss = _span([r["sss"] for r in rows if not r.get("tsg_pump_off")], 2, "")
    if sst:
        sea.append(f"temperature {sst}")
    if sss:
        sea.append(f"salinity {sss}")
    if sea:
        n_pump = sum(1 for r in rows if r.get("tsg_pump_off"))
        out.append("Surface water at the ship's intake had " + " and ".join(sea)
                   + (f" ({n_pump} operations during TSG pump stops are left out)" if n_pump else "")
                   + ".")
    ice_counts: dict[str, int] = {}
    unread = 0
    for v in vs:
        txt = next((r["ice"] for r in v if r["ice"] and r["ice_source"] in ("bridge", "observer")), None)
        if txt:
            ice_counts[txt] = ice_counts.get(txt, 0) + 1
        elif any(r["ice_source"] == "observer, as entered" for r in v):
            unread += 1
    if ice_counts:
        parts = [f"{txt} at {n} visit{'s' if n > 1 else ''}"
                 for txt, n in sorted(ice_counts.items(), key=lambda kv: -kv[1])]
        noted = sum(ice_counts.values())
        rest = len(vs) - noted - unread
        out.append("Ice was recorded on board as " + "; ".join(parts)
                   + (f"; at {unread} visit{'s' if unread > 1 else ''} the rosette sheet holds a "
                      "bare number with no scale stated" if unread else "")
                   + (f"; it was not recorded at the other {rest} visits" if rest else "") + ".")
    else:
        out.append("Ice conditions were not recorded on board at these stations.")
    dark = sum(1 for r in rows if r["sun_elev_deg"] is not None and r["sun_elev_deg"] < -6)
    if dark:
        out.append(f"{dark} of {len(rows)} operations began in darkness (sun more than 6° below "
                   "the horizon).")
    disagree = sum(1 for r in rows if any("depth sources disagree" in n for n in r.get("note_list", [])))
    if disagree:
        out.append(f"At {disagree} operations the EK60 and bridge-log depths disagree with the "
                   "multibeam by more than 10%; multibeam depths are used.")
    return " ".join(out)
