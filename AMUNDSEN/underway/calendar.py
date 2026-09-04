"""Event log and operations schedule for the calendar tab.

* The event log is the ship's ``Data/EventLog/<leg>/Eventlog_<leg>.xls``
  (one sheet; one row per logged event with position and met data).
* The schedule is the ship intranet page ``http://10.0.0.2/Schedule.html`` —
  a table of planned operations and a whiteboard note. It is fetched at build
  time and cached so the page keeps its last copy when the intranet is down.
"""

from __future__ import annotations

import html
import json
import logging
import os
import re
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from .config import DATA_ROOT, DB_DIR
from .legs import Leg

log = logging.getLogger(__name__)
SCHEDULE_URL = os.environ.get("UNDERWAY_SCHEDULE_URL", "http://10.0.0.2/Schedule.html")


def read_eventlog(leg: Leg) -> list[dict]:
    p = DATA_ROOT / "EventLog" / leg.id / f"Eventlog_{leg.id}.xls"
    if not p.is_file():
        return []
    try:
        df = pd.read_excel(p)
    except Exception as e:                      # noqa: BLE001 — a bad workbook must not stop the build
        log.warning("%s: cannot read event log (%s)", leg.id, e)
        return []
    df.columns = [re.sub(r"\s+", " ", str(c)).strip() for c in df.columns]
    want = {"Time (UTC)": "time_utc", "Time (Local)": "time_local", "Station ID": "station",
            "Station Type": "station_type", "Latitude": "lat", "Longitude": "lon", "Activity": "activity",
            "Event": "event", "Label": "label", "Depth (m)": "depth_m", "Wind Speed": "wind_kn",
            "Air Temp": "air_c", "Water Temp": "water_c", "Ice (0-10)": "ice", "Comment": "comment"}
    out = []
    for _, r in df.iterrows():
        e = {"leg": leg.id}
        for src, dst in want.items():
            v = r.get(src)
            if v is None or (isinstance(v, float) and pd.isna(v)):
                continue
            e[dst] = v.isoformat() if hasattr(v, "isoformat") else (str(v).strip() if isinstance(v, str) else float(v) if isinstance(v, (int, float)) else str(v))
        if e.get("time_utc"):
            out.append(e)
    return out


def fetch_schedule() -> dict:
    cache = DB_DIR / "schedule.json"
    try:
        with urllib.request.urlopen(SCHEDULE_URL, timeout=15) as r:
            s = r.read().decode("utf-8", errors="replace")
        sched = parse_schedule(s)
        sched["fetched_utc"] = datetime.now(timezone.utc).isoformat(timespec="seconds")
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(json.dumps(sched))
        return sched
    except Exception as e:                      # noqa: BLE001 — intranet down: serve the cached copy
        log.info("schedule not fetched (%s); using cache", e)
        if cache.is_file():
            sched = json.loads(cache.read_text())
            sched["stale"] = True
            return sched
        return {"rows": [], "whiteboard": "", "title": "", "updated": None, "stale": True}


def parse_schedule(s: str) -> dict:
    txt = re.sub(r"<(script|style).*?</\1>", "", s, flags=re.S | re.I)
    title = re.search(r"Schedule\s+(\d{4}\s+Leg\s+\d+)", txt)
    updated = re.search(r"Last Update:\s*([^<\n]+)", txt)
    # the operations table: header row then rows of 8 cells
    rows = []
    for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", txt, flags=re.S | re.I):
        cells = [html.unescape(re.sub(r"<[^>]+>", " ", c)).strip() for c in re.findall(r"<t[dh][^>]*>(.*?)</t[dh]>", tr, flags=re.S | re.I)]
        if len(cells) >= 7 and cells[0].lower() != "station":
            rows.append({"station": cells[0], "operation": cells[1], "status": cells[2], "date": cells[3],
                         "start": cells[4], "end": cells[5], "duration_h": _num(cells[6]),
                         "comment": cells[7] if len(cells) > 7 else ""})
    wb = re.search(r"Whiteboard\s*(?:</[^>]+>\s*)*(?:<[^>]+>\s*)*([^<]{3,})", txt)
    return {"title": title.group(1) if title else "", "updated": updated.group(1).strip() if updated else None,
            "rows": rows, "whiteboard": html.unescape(wb.group(1)).strip() if wb else ""}


def _num(s):
    try:
        return float(s)
    except (TypeError, ValueError):
        return None


def build_calendar(legs: list[Leg], root: Path) -> dict:
    from .build import atomic_write
    events = [e for leg in legs for e in read_eventlog(leg)]
    events.sort(key=lambda e: e.get("time_utc", ""))
    payload = {"events": events, "schedule": fetch_schedule(),
               "generated_utc": datetime.now(timezone.utc).isoformat(timespec="seconds")}
    atomic_write(root / "data" / "calendar.json", json.dumps(payload, separators=(",", ":")))
    log.info("calendar: %d events, %d scheduled operations", len(events), len(payload["schedule"].get("rows", [])))
    return {"events": len(events), "schedule_rows": len(payload["schedule"].get("rows", []))}
