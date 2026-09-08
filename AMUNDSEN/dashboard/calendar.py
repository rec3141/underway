"""Event log and operations schedule for the calendar tab.

* The event log is the ship's ``Data/EventLog/<leg>/Eventlog_<leg>.xls``
  (one sheet; one row per logged event with position and met data).
* The schedule is the ship intranet page ``http://10.0.0.2/Schedule.html`` —
  a table of planned operations and a whiteboard note. It is fetched at build
  time and cached so the page keeps its last copy when the intranet is down.
  A row is identified by its station and operation (``row_key``) while its
  times, status and comment are edited, so an edit updates the row in the
  history and on the Google calendar instead of adding a copy. The page only
  lists current and upcoming operations, so every row seen is also kept in
  ``db/schedule_history.json``; rows no longer on the page are served as
  ``former`` operations. Schedule times are ship wall-clock (``LOCAL_TZ``)
  and are given to the page as UTC instants.
* ``data/calendar.json`` carries the current legs (the live ones, or the
  newest); the legs before, and the calendar feeds' items from before them,
  go to ``data/calendar-archive.json``, which the browser fetches only when
  it looks that far back and keeps across builds by its content stamp.
"""

from __future__ import annotations

import hashlib
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
    old_rows = {row_key(r): r for r in prev.get("rows", [])}
    new_rows = {row_key(r): r for r in new.get("rows", [])}
    changed = []
    for k, r in new_rows.items():
        o = old_rows.get(k)
        name = f"{r.get('station')} — {r.get('operation')}"
        if o is None:
            changed.append(f"new: {name} {r.get('date')} {r.get('start')}–{r.get('end')}")
        elif o.get("status") != r.get("status"):
            changed.append(f"{name}: {r.get('status')}")
        elif (o.get("date"), o.get("start"), o.get("end")) != (r.get("date"), r.get("start"), r.get("end")):
            changed.append(f"{name} moved to {r.get('date')} {r.get('start')}–{r.get('end')}")
        elif o.get("comment") != r.get("comment"):
            changed.append(f"{name}: {r.get('comment')}")
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


def row_key(r: dict) -> str:
    """What identifies a schedule row through its edits: ``station|operation``,
    with ``|n`` for the n-th further row of the same station and operation on
    the page (``parse_schedule`` stores it as ``key``)."""
    return r.get("key") or f"{r.get('station') or ''}|{r.get('operation') or ''}"


def _remember(rows: list[dict], title: str) -> list[dict]:
    """Fold the rows seen now into the history; return the former rows (seen
    before, no longer on the page), oldest first."""
    hist_p = DB_DIR / "schedule_history.json"
    hist = json.loads(hist_p.read_text()) if hist_p.is_file() else {}
    now = datetime.now(timezone.utc).isoformat(timespec="seconds")
    current = set()
    for r in rows:
        k = row_key(r)
        if not r.get("start_utc"):
            # a canceled row loses its times on the page: the history keeps the
            # ones it had and takes the new status and comment
            if k in hist:
                current.add(k)
                hist[k].update({x: r[x] for x in ("status", "comment") if x in r}); hist[k]["last_seen"] = now
            continue
        current.add(k)
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
    seen: dict[str, int] = {}
    for r in rows:
        k = row_key(r)
        seen[k] = seen.get(k, -1) + 1
        r["key"] = f"{k}|{seen[k]}" if seen[k] else k
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


def around_now(rows: list[dict]) -> dict:
    """The operations the header bar shows: the last completed row, the rows
    in progress, and the next one to start (the first row after the last
    completed or started one, in page order, since the schedule slips)."""
    rows = [r for r in rows if r.get("start_utc")]
    status = lambda r: (r.get("status") or "").lower()
    brief = lambda r: dict({k: r.get(k) for k in ("station", "operation", "status", "start_utc", "end_utc", "comment")}, key=row_key(r))
    done = [r for r in rows if status(r) == "completed"]
    live = [r for r in rows if status(r) == "in progress"]
    last = max((i for i, r in enumerate(rows) if status(r) in ("completed", "in progress")), default=-1)
    now = datetime.now(timezone.utc).isoformat(timespec="minutes")
    upcoming = [r for r in rows[last + 1:] if status(r) not in ("canceled", "cancelled", "completed", "in progress")]
    nxt = next((r for r in upcoming if r["start_utc"] >= now), None) if last < 0 else (upcoming[0] if upcoming else None)
    return {"completed": brief(max(done, key=lambda r: r["end_utc"])) if done else None,
            "in_progress": [brief(r) for r in live], "next": brief(nxt) if nxt else None}


ARCHIVE_FILE = "calendar-archive.json"


def current_legs(legs: list[Leg]) -> list[Leg]:
    """The legs whose events the browser gets with every build: the live
    ones, or the newest when none is live (between legs, in port)."""
    live = [l for l in legs if l.live]
    if live:
        return live
    return [max(legs, key=lambda l: (l.year, l.number))] if legs else []


def partition(events: list[dict], feeds: list[dict], legs: list[Leg]) -> tuple[list[dict], list[dict], dict | None]:
    """Split the calendar between the file fetched every build and the
    archive fetched only when a leg (or a month) before the current legs
    is looked at: the current legs' events, and the calendar feeds' items
    from their first day on, stay; the rest goes to the archive.

    Returns (current events, current feeds, archive payload or None when
    nothing is old enough)."""
    cur = current_legs(legs)
    ids = {l.id for l in cur}
    firsts = [l.first_date for l in cur if l.first_date]
    before = min(firsts) if firsts else None
    before_iso = f"{before[:4]}-{before[4:6]}-{before[6:8]}" if before else None
    # an event without a leg (none are logged so today) stays where it is seen
    old = [e for e in events if e.get("leg") is not None and e.get("leg") not in ids]
    new = [e for e in events if e.get("leg") is None or e.get("leg") in ids]
    cur_feeds, old_feeds = [], []
    for f in feeds:
        items = f.get("events") or []
        past = [e for e in items if before_iso and str(e.get("start") or "")[:10] < before_iso]
        if past:
            old_feeds.append({"key": f.get("key"), "label": f.get("label"), "events": past})
            f = dict(f, events=[e for e in items if e not in past])
        cur_feeds.append(f)
    if not old and not old_feeds:
        return new, cur_feeds, None
    archive = {"events": old, "gcal": old_feeds, "legs": sorted({e["leg"] for e in old}), "before": before_iso}
    return new, cur_feeds, archive


def write_archive(root: Path, archive: dict | None) -> dict | None:
    """Write ``data/calendar-archive.json`` when its content changed (it
    seldom does: the legs before this one are closed) and return what the
    manifest says about it, so the browser fetches it by content stamp and
    keeps it across builds."""
    if archive is None:
        return None
    from .build import atomic_write
    text = json.dumps(archive, separators=(",", ":"))
    stamp = hashlib.sha1(text.encode()).hexdigest()[:12]
    p = root / "data" / ARCHIVE_FILE
    try:
        same = p.is_file() and p.stat().st_size == len(text.encode()) and p.read_text() == text
    except OSError:
        same = False
    if not same:
        atomic_write(p, text)
    return {"file": f"data/{ARCHIVE_FILE}", "stamp": stamp, "legs": archive["legs"], "before": archive["before"],
            "events": len(archive["events"]), "bytes": len(text)}


def build_calendar(legs: list[Leg], root: Path, frame=None, events: list[dict] | None = None) -> dict:
    """Write ``data/calendar.json`` (the current legs) and the archive of
    the legs before; ``events`` are the legs' event-log rows (read here
    when not given)."""
    from .build import atomic_write
    from . import gcal
    from .pump import pump_events
    pump = pump_events(frame, legs)
    events = list(events) if events is not None else [e for leg in legs for e in read_eventlog(leg)]
    events.extend(pump)
    events.sort(key=lambda e: e.get("time_utc", ""))
    payload = {"pump_events": pump, "schedule": fetch_schedule(),
               "generated_utc": datetime.now(timezone.utc).isoformat(timespec="seconds")}
    try:
        feeds = gcal.import_calendars()
    except Exception:                       # noqa: BLE001 — the tab works without the feeds
        log.exception("google calendar import failed")
        feeds = []
    try:
        payload["gcal_sync"] = gcal.queue(events, payload["schedule"], frame)
    except Exception:                       # noqa: BLE001
        log.exception("google calendar queue failed")
    payload["events"], payload["gcal"], archive = partition(events, feeds, legs)
    payload["archive"] = write_archive(root, archive)
    atomic_write(root / "data" / "calendar.json", json.dumps(payload, separators=(",", ":")))
    sched = payload["schedule"]
    log.info("calendar: %d events, %d scheduled operations (%d former)", len(events), len(sched.get("rows", [])), len(sched.get("former", [])))
    from .config import GCAL
    links = [{"key": k, "label": c["label"], "url": f"https://calendar.google.com/calendar/embed?src={c['id'].replace('@', '%40')}&ctz={LOCAL_TZ.replace('/', '%2F')}",
              "ics": f"https://calendar.google.com/calendar/ical/{c['id'].replace('@', '%40')}/public/basic.ics"} for k, c in GCAL.items()]
    logged = [e["time_utc"] for e in events if not str(e.get("id", "")).startswith("pump|") and e.get("time_utc")]
    return {"events": len(events), "schedule_rows": len(sched.get("rows", [])), "former": len(sched.get("former", [])),
            "archive": payload["archive"],
            "update": sched.get("update"), "now": around_now(sched.get("rows", [])), "feeds": links,
            "sources": {"schedule": sched.get("fetched_utc"), "event_log": max(logged) if logged else None,
                        "calendars": max((f.get("fetched_utc") or "" for f in feeds), default=None) or None}}
