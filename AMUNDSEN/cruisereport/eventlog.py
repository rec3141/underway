"""Operations from the bridge's event log.

``Data/EventLog/<leg>/Eventlog_<leg>.xls`` has one row per event: a
Deployment (which carries the operation's label, e.g. ``AMD2603-010``), then
usually a Bottom and a Recovery. Each row also records the ship's position,
bottom depth and the met/TSG readings at that moment.

Rows are paired into operations in time order: a Bottom or Recovery joins the
latest open operation of the same station and instrument group. The label is
the key that the CTD logbook, the rosette sheets and most team logsheets
share. An event with no open Deployment still becomes an operation, with a
synthetic key, so nothing the bridge logged is dropped.

Comments in the log are often carried from the station plan ("Timing TBD");
they are kept as written and not interpreted.
"""

from __future__ import annotations

import logging
import math
from dataclasses import asdict, dataclass, field
from functools import lru_cache
from pathlib import Path

import pandas as pd

from . import activities
from .config import DATA_ROOT

log = logging.getLogger(__name__)

# Event-log column -> our key. The log's headers carry stray spaces.
READINGS = {
    "Depth (m)": "depth_m",
    "Wind Dir": "wind_dir_deg",
    "Wind Speed": "wind_kn",
    "Air Temp": "air_c",
    "Water Temp": "water_c",
    "Water Salinity": "water_sal",
    "Pr Baro": "pressure_hpa",
    "Hum (%)": "humidity_pct",
    "Ice (0-10)": "ice_tenths",
}


@dataclass
class Event:
    kind: str                   # Deployment, Bottom, Recovery, Other
    time: str                   # ISO UTC
    lat: float | None
    lon: float | None
    readings: dict = field(default_factory=dict)


@dataclass
class Operation:
    key: str                    # the label, or a synthetic key when there is none
    label: str | None
    leg: str
    station: str | None
    station_type: str | None
    activity: str
    group: str
    comment: str | None
    events: list[Event] = field(default_factory=list)

    def event(self, kind: str) -> Event | None:
        return next((e for e in self.events if e.kind == kind), None)

    @property
    def start(self) -> Event:
        return self.event("Deployment") or self.events[0]

    @property
    def end(self) -> Event:
        return self.event("Recovery") or self.events[-1]

    def summary(self) -> dict:
        s, b, e = self.start, self.event("Bottom"), self.end
        dur = (pd.Timestamp(e.time) - pd.Timestamp(s.time)).total_seconds() / 60
        return {
            "key": self.key, "label": self.label, "leg": self.leg,
            "station": self.station, "station_type": self.station_type,
            "activity": self.activity, "group": self.group,
            "group_label": activities.LABELS.get(self.group, self.group),
            "start_utc": s.time, "bottom_utc": b.time if b else None, "end_utc": e.time,
            "duration_min": round(dur, 1) if len(self.events) > 1 else None,
            "lat": s.lat, "lon": s.lon, "end_lat": e.lat, "end_lon": e.lon,
            "depth_m": s.readings.get("depth_m"),
            "comment": self.comment,
        }

    def to_dict(self) -> dict:
        return asdict(self)


def _num(v) -> float | None:
    try:
        v = float(v)
    except (TypeError, ValueError):
        return None
    return v if math.isfinite(v) else None


def _text(v) -> str | None:
    if v is None or (isinstance(v, float) and math.isnan(v)):
        return None
    v = " ".join(str(v).split())
    return v or None


def path_for(leg: str) -> Path:
    return DATA_ROOT / "EventLog" / leg / f"Eventlog_{leg}.xls"


def legs() -> list[str]:
    root = DATA_ROOT / "EventLog"
    if not root.is_dir():
        raise FileNotFoundError(f"{root} is not mounted")
    return sorted(p.name for p in root.iterdir() if path_for(p.name).is_file())


def operations(leg: str) -> list[Operation]:
    p = path_for(leg)
    return list(_operations(leg, p.stat().st_mtime))


@lru_cache(maxsize=16)
def _operations(leg: str, _mtime: float) -> tuple[Operation, ...]:
    df = pd.read_excel(path_for(leg))
    df.columns = [" ".join(str(c).split()) for c in df.columns]
    df["_t"] = pd.to_datetime(df["Time (UTC)"], format="%Y/%m/%d %H:%M:%S", errors="coerce")
    df = df.dropna(subset=["_t"]).sort_values("_t", kind="stable")

    ops: list[Operation] = []
    open_ops: dict[tuple, Operation] = {}
    n_synth = 0
    for row in df.to_dict("records"):
        activity = _text(row.get("Activity")) or "Unknown"
        group = activities.group_of(activity)
        station = _text(row.get("Station ID"))
        kind = _text(row.get("Event")) or "Other"
        ev = Event(kind=kind, time=row["_t"].isoformat(),
                   lat=_num(row.get("Latitude")), lon=_num(row.get("Longitude")),
                   readings={k: _num(row.get(c)) for c, k in READINGS.items()})
        slot = (station, group)
        op = open_ops.get(slot)
        if kind == "Deployment" or op is None:
            label = _text(row.get("Label"))
            if not label:
                n_synth += 1
                label_key = f"{leg}:event-{n_synth:03d}"
            op = Operation(key=label or label_key, label=label, leg=leg, station=station,
                           station_type=_text(row.get("Station Type")), activity=activity,
                           group=group, comment=_text(row.get("Comment")))
            ops.append(op)
            open_ops[slot] = op
        op.events.append(ev)
        if kind == "Recovery":
            open_ops.pop(slot, None)
    log.info("%s: %d operations from %d events", leg, len(ops), len(df))
    return tuple(ops)


def by_key(leg: str) -> dict[str, Operation]:
    return {op.key: op for op in operations(leg)}
