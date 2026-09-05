"""Event log and operations schedule for the calendar tab.

* The event log is the ship's ``Data/EventLog/<leg>/Eventlog_<leg>.xls``
  (one sheet; one row per logged event with position and met data).
* The schedule is the ship intranet page ``http://10.0.0.2/Schedule.html`` —
  a table of planned operations and a whiteboard note. It is fetched at build
  time and cached so the page keeps its last copy when the intranet is down.
  The page only lists current and upcoming operations, so every row seen is
  also kept in ``db/schedule_history.json``; rows no longer on the page are
  served as ``former`` operations. Schedule times are ship wall-clock
  (``LOCAL_TZ``) and are given to the page as UTC instants.
"""

from __future__ import annotations

import html
import json
import logging
import os
import re
import urllib.request
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo
from pathlib import Path

import pandas as pd

from .config import DATA_ROOT, DB_DIR, LOCAL_TZ, SHARE_ROOT
from .legs import Leg

log = logging.getLogger(__name__)
SCHEDULE_URL = os.environ.get("UNDERWAY_SCHEDULE_URL", "http://10.0.0.2/Schedule.html")


def eventlog_path(leg: Leg) -> Path | None:
    """The leg's event log: ``Data/EventLog/<leg>/`` for the current season;
    earlier seasons only survive as copies scattered through people's folders
    on the Share (``Share/<year>/<any leg>/**/Eventlog_<leg>.xls[x]``), so the
    largest of those stands in."""
    p = DATA_ROOT / "EventLog" / leg.id / f"Eventlog_{leg.id}.xls"
    if p.is_file():
        return p
    year = leg.id[:4]
    found = [q for pat in (f"*/**/Eventlog_{leg.id}.xls", f"*/**/Eventlog_{leg.id}.xlsx") for q in (SHARE_ROOT / year).glob(pat)] if (SHARE_ROOT / year).is_dir() else []
    found = [q for q in found if "copy" not in q.name.lower()]
    return max(found, key=lambda q: q.stat().st_size) if found else None


def read_eventlog(leg: Leg) -> list[dict]:
    p = eventlog_path(leg)
    if p is None:
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
        # rows without a real time, or with nothing said, are not events
        if e.get("time_utc") and str(e["time_utc"])[:4].isdigit() and int(str(e["time_utc"])[:4]) >= 2000 \
                and (e.get("station") or e.get("activity") or e.get("event") or e.get("comment")):
            out.append(e)
    return out


def fetch_schedule() -> dict:
    cache = DB_DIR / "schedule.json"
    prev = json.loads(cache.read_text()) if cache.is_file() else None
    try:
        with urllib.request.urlopen(SCHEDULE_URL, timeout=15) as r:
            s = r.read().decode("utf-8", errors="replace")
        sched = parse_schedule(s)
        sched["fetched_utc"] = datetime.now(timezone.utc).isoformat(timespec="seconds")
        # the latest change to the page, for the alert bar; carried forward
        # until the next one
        sched["update"] = _what_changed(prev, sched) or (prev or {}).get("update")
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(json.dumps(sched))
    except Exception as e:                      # noqa: BLE001 — intranet down: serve the cached copy
        log.info("schedule not fetched (%s); using cache", e)
        if prev:
            sched = dict(prev, stale=True)
        else:
            sched = {"rows": [], "whiteboard": "", "title": "", "updated": None, "stale": True}
    for r in sched["rows"]:
        r.update(_instants(r))
    sched["former"] = _remember(sched["rows"], sched.get("title", ""))
    return sched


def _what_changed(prev: dict | None, new: dict) -> dict | None:
    """A one-line description of how the page differs from the copy before."""
    if prev is None:
        return None
    parts = []
    if (new.get("whiteboard") or "") != (prev.get("whiteboard") or ""):
        parts.append(f"Whiteboard: {new.get('whiteboard') or '(cleared)'}")
    key = lambda r: (r.get("date"), r.get("start"), r.get("station"), r.get("operation"))
    old_rows = {key(r): r for r in prev.get("rows", [])}
    new_rows = {key(r): r for r in new.get("rows", [])}
    changed = []
    for k, r in new_rows.items():
        o = old_rows.get(k)
        if o is None:
            changed.append(f"new: {r.get('station')} — {r.get('operation')} {r.get('date')} {r.get('start')}–{r.get('end')}")
        elif (o.get("status"), o.get("end"), o.get("comment")) != (r.get("status"), r.get("end"), r.get("comment")):
            changed.append(f"{r.get('station')} — {r.get('operation')}: {r.get('status')}")
    for k, o in old_rows.items():
        if k not in new_rows:
            changed.append(f"removed: {o.get('station')} — {o.get('operation')} {o.get('date')}")
    if changed:
        parts.append("Schedule: " + "; ".join(changed[:6]) + (f" (+{len(changed) - 6} more)" if len(changed) > 6 else ""))
    if not parts:
        return None
    return {"changed_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"), "text": " · ".join(parts),
            "kind": "whiteboard" if parts[0].startswith("Whiteboard") else "schedule"}


def _instants(r: dict) -> dict:
    """UTC start/end for a schedule row whose date and times are ship wall-clock."""
    m = re.match(r"(\d{2})/(\d{2})/(\d{2,4})$", r.get("date") or "")
    if not m:
        return {}
    y = int(m.group(3)); y += 2000 if y < 100 else 0
    tz = ZoneInfo(LOCAL_TZ)
    def at(hm, default):
        hh, mm = (hm or default).split(":")[:2]
        return datetime(y, int(m.group(2)), int(m.group(1)), int(hh), int(mm), tzinfo=tz)
    try:
        t0 = at(r.get("start"), "00:00"); t1 = at(r.get("end"), "23:59")
    except ValueError:
        return {}
    # the duration column is authoritative: an operation running past midnight
    # (04:30 to 05:00 the next day, 24.5 h) reads as half an hour from the clock
    # times alone
    dur = r.get("duration_h")
    if isinstance(dur, (int, float)) and dur > 0:
        t1 = t0 + timedelta(hours=float(dur))
    elif t1 < t0:
        t1 += timedelta(days=1)
    return {"start_utc": t0.astimezone(timezone.utc).isoformat(timespec="minutes"),
            "end_utc": t1.astimezone(timezone.utc).isoformat(timespec="minutes")}


def _remember(rows: list[dict], title: str) -> list[dict]:
    """Fold the rows seen now into the history; return the former rows (seen
    before, no longer on the page), oldest first."""
    hist_p = DB_DIR / "schedule_history.json"
    hist = json.loads(hist_p.read_text()) if hist_p.is_file() else {}
    now = datetime.now(timezone.utc).isoformat(timespec="seconds")
    key = lambda r: "|".join(str(r.get(k) or "") for k in ("date", "start", "station", "operation"))
    current = set()
    for r in rows:
        if not r.get("start_utc"):
            continue
        k = key(r); current.add(k)
        h = hist.get(k, {"first_seen": now})
        h.update(r); h["last_seen"] = now; h["leg"] = title or h.get("leg", "")
        hist[k] = h
    hist_p.parent.mkdir(parents=True, exist_ok=True)
    hist_p.write_text(json.dumps(hist))
    former = [dict(h, former=True) for k, h in hist.items() if k not in current]
    former.sort(key=lambda h: h.get("start_utc", ""))
    return former


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
    # the whiteboard is the <p> that follows the "Whiteboard" heading, one
    # line per <br>
    wb = re.search(r"Whiteboard\s*</p>.*?<p[^>]*>(.*?)</p>", txt, flags=re.S | re.I)
    board = ""
    if wb:
        raw = re.sub(r"<br\s*/?>", "\n", wb.group(1), flags=re.I)
        lines = [" ".join(html.unescape(re.sub(r"<[^>]+>", " ", l)).split()) for l in raw.split("\n")]
        board = "\n".join(l for l in lines if l)
    return {"title": title.group(1) if title else "", "updated": updated.group(1).strip() if updated else None,
            "rows": rows, "whiteboard": board}


def _num(s):
    try:
        return float(s)
    except (TypeError, ValueError):
        return None


def build_calendar(legs: list[Leg], root: Path, frame=None) -> dict:
    from .build import atomic_write
    from . import gcal
    events = [e for leg in legs for e in read_eventlog(leg)]
    events.sort(key=lambda e: e.get("time_utc", ""))
    payload = {"events": events, "schedule": fetch_schedule(),
               "generated_utc": datetime.now(timezone.utc).isoformat(timespec="seconds")}
    try:
        payload["gcal"] = gcal.import_calendars()
    except Exception:                       # noqa: BLE001 — the tab works without the feeds
        log.exception("google calendar import failed")
        payload["gcal"] = []
    try:
        payload["gcal_sync"] = gcal.queue(events, payload["schedule"], frame)
    except Exception:                       # noqa: BLE001
        log.exception("google calendar queue failed")
    atomic_write(root / "data" / "calendar.json", json.dumps(payload, separators=(",", ":")))
    sched = payload["schedule"]
    log.info("calendar: %d events, %d scheduled operations (%d former)", len(events), len(sched.get("rows", [])), len(sched.get("former", [])))
    return {"events": len(events), "schedule_rows": len(sched.get("rows", [])), "former": len(sched.get("former", [])),
            "update": sched.get("update")}
