"""Google Calendar: the two public calendars are imported into the calendar
tab, and the ship's operations and surprise episodes are pushed to them.

* ``schedule`` ("Amundsen Schedule"): one event per event-log operation
  (station × activity × type × local day) and one per row of the intranet
  operations schedule, updated when its status changes.
* ``surprise`` ("Underway Updates"): one event per surprise episode — a run of
  minutes with the ``SURPRISE_ALERT_SCALE`` score above ``SURPRISE_ALERT``,
  runs less than half an hour apart merged — extended while it continues.

The build itself touches no Google endpoint: it writes the wanted items to
``db/gcal_queue.json`` and reads the feeds from ``db/gcal_cache.json``. The
``gcal-push`` command (underway-gcal.timer, every few minutes) refreshes the
feed cache and works through the queue with the service account in
``GCAL_CREDS`` (a JWT exchanged for a token; the account owns both
calendars), at most ``GCAL_MAX_CALLS`` requests per run. Every pushed item
is remembered in ``db/gcal_state.json`` by fingerprint with its event id and
a hash of its body, so nothing is inserted twice and only changes are
patched; an empty state adopts what is already on the calendars.
"""

from __future__ import annotations

import hashlib
import json
import logging
import os
import re
import time
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo

import pandas as pd

from .config import (DB_DIR, GCAL, GCAL_CREDS, GCAL_MAX_CALLS, GCAL_SINCE, GCAL_SYNC_MINUTES, LOCAL_TZ,
                     SURPRISE_ALERT, SURPRISE_ALERT_SCALE, SURPRISE_NAME, surprise_scale_name)

log = logging.getLogger(__name__)
API = "https://www.googleapis.com/calendar/v3"
TIMEOUT = 12
TZ = ZoneInfo(LOCAL_TZ)


# ---------------------------------------------------------------- import

def _unfold(text: str) -> list[str]:
    out: list[str] = []
    for line in text.splitlines():
        if line[:1] in (" ", "\t") and out:
            out[-1] += line[1:]
        else:
            out.append(line)
    return out


def _ics_time(prop: str, value: str) -> tuple[str | None, bool]:
    """ISO UTC instant for a DTSTART/DTEND line, and whether it is a date."""
    if "VALUE=DATE" in prop or re.fullmatch(r"\d{8}", value):
        return f"{value[:4]}-{value[4:6]}-{value[6:8]}", True
    m = re.match(r"(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})(Z?)", value)
    if not m:
        return None, False
    dt = datetime(*map(int, m.groups()[:6]))
    if m.group(7) == "Z":
        dt = dt.replace(tzinfo=timezone.utc)
    else:
        tzm = re.search(r"TZID=([^;:]+)", prop)
        try:
            dt = dt.replace(tzinfo=ZoneInfo(tzm.group(1)) if tzm else TZ)
        except Exception:                  # noqa: BLE001 — unknown zone name: ship time
            dt = dt.replace(tzinfo=TZ)
    return dt.astimezone(timezone.utc).isoformat(timespec="minutes"), False


def parse_ics(text: str) -> list[dict]:
    events, cur = [], None
    for line in _unfold(text):
        if line == "BEGIN:VEVENT":
            cur = {}
        elif line == "END:VEVENT" and cur is not None:
            if cur.get("start"):
                events.append(cur)
            cur = None
        elif cur is not None and ":" in line:
            prop, _, value = line.partition(":")
            name = prop.split(";")[0]
            value = value.replace("\\n", "\n").replace("\\,", ",").replace("\\;", ";")
            if name in ("DTSTART", "DTEND"):
                t, all_day = _ics_time(prop, value)
                cur["start" if name == "DTSTART" else "end"] = t
                cur["all_day"] = all_day
            elif name in ("SUMMARY", "DESCRIPTION", "LOCATION", "UID", "STATUS"):
                cur[name.lower()] = value
    events.sort(key=lambda e: e["start"])
    return events


def import_calendars(fetch: bool = False) -> list[dict]:
    """The public feeds of both calendars: the cached copy, refreshed when
    ``fetch`` is set and the copy is older than ``GCAL_SYNC_MINUTES``."""
    cache_p = DB_DIR / "gcal_cache.json"
    cache = json.loads(cache_p.read_text()) if cache_p.is_file() else {}
    now = datetime.now(timezone.utc)
    out = []
    for key, c in GCAL.items():
        entry = cache.get(key, {})
        fetched = entry.get("fetched_utc")
        fresh = fetched and (now - datetime.fromisoformat(fetched)) < timedelta(minutes=GCAL_SYNC_MINUTES)
        if not fetch:
            entry = dict(entry, stale=not fetched or (now - datetime.fromisoformat(fetched)) > timedelta(minutes=3 * GCAL_SYNC_MINUTES))
        elif not fresh:
            try:
                import requests
                r = requests.get(f"https://calendar.google.com/calendar/ical/{c['id'].replace('@', '%40')}/public/basic.ics",
                                 timeout=TIMEOUT)
                r.raise_for_status()
                entry = {"events": parse_ics(r.text), "fetched_utc": now.isoformat(timespec="seconds")}
                cache[key] = entry
            except Exception as e:          # noqa: BLE001 — offline: keep the cached copy
                log.info("gcal %s: not fetched (%s); using cache", key, e)
                entry = dict(entry, stale=True)
        out.append({"key": key, "label": c["label"], "id": c["id"], **entry})
    if fetch:
        DB_DIR.mkdir(parents=True, exist_ok=True)
        cache_p.write_text(json.dumps(cache))
    return out


# ---------------------------------------------------------------- push

class _Api:
    def __init__(self):
        import jwt
        import requests
        self.rq = requests
        sa = json.loads(GCAL_CREDS.read_text())
        now = int(time.time())
        assertion = jwt.encode({"iss": sa["client_email"], "scope": "https://www.googleapis.com/auth/calendar",
                                "aud": sa["token_uri"], "iat": now, "exp": now + 3600}, sa["private_key"], algorithm="RS256")
        r = requests.post(sa["token_uri"], data={"grant_type": "urn:ietf:params:oauth:grant-type:jwt-bearer",
                                                  "assertion": assertion}, timeout=TIMEOUT)
        r.raise_for_status()
        self.h = {"Authorization": "Bearer " + r.json()["access_token"]}

    def list_fps(self, cal_id: str, since: str) -> dict[str, str]:
        """fingerprint -> event id for every event this code has put on a calendar."""
        out, token = {}, None
        while True:
            params = {"timeMin": since, "maxResults": 2500, "singleEvents": "true", "fields": "items(id,extendedProperties),nextPageToken"}
            if token:
                params["pageToken"] = token
            r = self.rq.get(f"{API}/calendars/{cal_id}/events", headers=self.h, params=params, timeout=TIMEOUT)
            r.raise_for_status()
            j = r.json()
            for it in j.get("items", []):
                fp = (it.get("extendedProperties") or {}).get("private", {}).get("fp")
                if fp:
                    out[fp] = it["id"]
            token = j.get("nextPageToken")
            if not token:
                return out

    def insert(self, cal_id: str, body: dict) -> str:
        r = self.rq.post(f"{API}/calendars/{cal_id}/events", headers=self.h, json=body, timeout=TIMEOUT)
        r.raise_for_status()
        return r.json()["id"]

    def patch(self, cal_id: str, event_id: str, body: dict) -> None:
        r = self.rq.patch(f"{API}/calendars/{cal_id}/events/{event_id}", headers=self.h, json=body, timeout=TIMEOUT)
        if r.status_code == 404:
            raise KeyError(event_id)
        r.raise_for_status()


def _when(iso: str) -> dict:
    return {"dateTime": datetime.fromisoformat(iso).astimezone(TZ).isoformat(), "timeZone": LOCAL_TZ}


def _utc(s: str) -> datetime | None:
    if not s:
        return None
    s = str(s).strip()
    m = re.match(r"(\d{4})[/-](\d{2})[/-](\d{2})[ T](\d{2}):(\d{2})(?::(\d{2}))?", s)
    dt = None
    try:
        dt = datetime.fromisoformat(s.replace(" ", "T")) if "T" in s or "-" in s[:8] else None
    except ValueError:
        dt = None
    if dt is None and m:
        try:
            dt = datetime(*[int(x) for x in m.groups() if x is not None])
        except ValueError:                  # a placeholder like 0000/00/00 in the workbook
            return None
    if dt is None:
        return None
    return dt.replace(tzinfo=timezone.utc) if dt.tzinfo is None else dt.astimezone(timezone.utc)


def _dms(lat, lon) -> str:
    try:
        lat, lon = float(lat), float(lon)
    except (TypeError, ValueError):
        return ""
    return f"{abs(lat):.4f}°{'N' if lat >= 0 else 'S'}, {abs(lon):.4f}°{'E' if lon >= 0 else 'W'}"


def eventlog_items(events: list[dict]) -> list[tuple[str, str, dict]]:
    """(calendar, fingerprint, body) per operation: station × activity × type × local day."""
    since = _utc(GCAL_SINCE)
    groups: dict[tuple, list[dict]] = {}
    for e in events:
        if str(e.get("id", "")).startswith("pump|"):
            continue  # preserve individual episodes, not one daily operation
        t = _utc(e.get("time_utc", ""))
        if not t or (since and t < since):
            continue
        day = t.astimezone(TZ).date().isoformat()
        groups.setdefault((e.get("station") or "", e.get("activity") or "", e.get("station_type") or "", day), []).append(dict(e, _t=t))
    out = []
    for (station, activity, stype, day), es in groups.items():
        t0 = min(e["_t"] for e in es); t1 = max(e["_t"] for e in es)
        if t1 <= t0:
            t1 = t0 + timedelta(minutes=1)
        first = min(es, key=lambda e: e["_t"])
        comment = next((e["comment"] for e in es if e.get("comment")), "")
        ops = " | ".join(x for x in (activity, stype) if x)
        desc = "\n".join(x for x in (f"Operation: {ops}" if ops else "", f"Station: {station}" if station else "",
                                     "Status: EventLog", f"Position: {_dms(first.get('lat'), first.get('lon'))}" if first.get("lat") is not None else "",
                                     f"Events: {', '.join(sorted({str(e.get('event') or '') for e in es} - {''}))}",
                                     f"Comment: {comment}" if comment else "") if x)
        body = {"summary": f"[EventLog] {station} — {ops}".strip(), "description": desc,
                "start": _when(t0.isoformat()), "end": _when(t1.isoformat())}
        out.append(("schedule", f"ev|{station}|{activity}|{stype}|{day}", body))
    return out


def schedule_items(sched: dict) -> list[tuple[str, str, dict]]:
    out = []
    for r in sched.get("rows", []):
        if not r.get("start_utc") or not r.get("end_utc"):
            continue
        status = r.get("status") or "Scheduled"
        ops = r.get("operation") or ""
        desc = "\n".join(x for x in (f"Operation: {ops}", f"Station: {r.get('station', '')}", f"Status: {status}",
                                     f"Comment: {r['comment']}" if r.get("comment") else "") if x)
        body = {"summary": f"[{status}] {r.get('station', '')} — {ops}".strip(), "description": desc,
                "start": _when(r["start_utc"]), "end": _when(r["end_utc"])}
        out.append(("schedule", f"sch|{r.get('date')}|{r.get('start')}|{r.get('station')}|{ops}", body))
    return out


def surprise_items(frame: pd.DataFrame, hours: float = 72) -> list[tuple[str, str, dict]]:
    """One item per episode of the alert-scale surprise above the alert level
    in the last ``hours``; a continuing episode keeps its fingerprint (the
    start minute) and grows."""
    col = surprise_scale_name(SURPRISE_ALERT_SCALE)
    if frame is None or col not in frame.columns:
        return []
    s = frame[col]
    s = s[s.index >= s.index.max() - pd.Timedelta(hours=hours)]
    hot = s[s > SURPRISE_ALERT]
    if hot.empty:
        return []
    scales = [c for c in frame.columns if c.startswith("Surprise ·")]
    out = []
    gaps = hot.index.to_series().diff() > pd.Timedelta(minutes=30)
    for _, ep in hot.groupby(gaps.cumsum()):
        t0, t1 = ep.index.min(), ep.index.max()
        peak_t = ep.idxmax(); peak = float(ep.max())
        row = frame.loc[peak_t]
        lead = max(scales, key=lambda c: (row[c] if pd.notna(row[c]) else -1)) if scales else None
        t1 = max(t1, t0 + pd.Timedelta(minutes=15))
        local = peak_t.tz_convert(TZ).strftime("%Y-%m-%d %H:%M %Z")
        desc = "\n".join(x for x in (f"Peak {SURPRISE_ALERT_SCALE} surprise {peak:.1f} at {local} ({peak_t.strftime('%H:%M')} UTC)"
                                     + (f"; combined {row[SURPRISE_NAME]:.1f}" if SURPRISE_NAME in row and pd.notna(row[SURPRISE_NAME]) else ""),
                                     f"Position: {_dms(row.get('lat'), row.get('lon'))}" if pd.notna(row.get("lat")) else "",
                                     f"Leading scale: {lead.replace('Surprise · ', '')} ({row[lead]:.1f})" if lead else "",
                                     "Scales: " + ", ".join(f"{c.replace('Surprise · ', '')} {row[c]:.1f}" for c in scales if pd.notna(row[c])),
                                     f"Duration: {(t1 - t0).total_seconds() / 60:.0f} min") if x)
        body = {"summary": f"Surprise {peak:.1f} ({SURPRISE_ALERT_SCALE}) at {local}", "description": desc,
                "start": _when(t0.isoformat()), "end": _when(t1.isoformat())}
        out.append(("surprise", f"sur|{t0.strftime('%Y-%m-%dT%H:%M')}", body))
    return out


def _hash(body: dict) -> str:
    return hashlib.sha1(json.dumps(body, sort_keys=True).encode()).hexdigest()[:16]


def _state() -> dict:
    p = DB_DIR / "gcal_state.json"
    return json.loads(p.read_text()) if p.is_file() else {"last_sync": None, "items": {}}


def _pending(items: list, state: dict) -> list:
    todo = []
    for cal, fp, body in items:
        h = _hash(body)
        known = state["items"].get(fp)
        if known is None:
            todo.append(("insert", cal, fp, body, h))
        elif known.get("hash") != h:
            todo.append(("patch", cal, fp, body, h))
    return todo


def queue(events: list[dict], sched: dict, frame: pd.DataFrame | None) -> dict:
    """Called by the build: write what the calendars should hold; no network."""
    if os.environ.get("UNDERWAY_GCAL", "1") != "1":
        return {"skipped": "disabled"}
    items = eventlog_items(events) + schedule_items(sched) + surprise_items(frame)
    since = _utc(GCAL_SINCE)
    items += [("surprise", e["id"], {"summary": "TSG pump off / low intake flow",
               "description": e["comment"], "start": _when(e["time_utc"]), "end": _when(e["end_utc"]),
               "extendedProperties": {"private": {"underwayPump": e["id"]}}})
              for e in events if str(e.get("id", "")).startswith("pump|")
              and (not since or _utc(e["time_utc"]) >= since)]
    DB_DIR.mkdir(parents=True, exist_ok=True)
    (DB_DIR / "gcal_queue.json").write_text(json.dumps({"queued_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
                                                         "items": [[c, fp, b] for c, fp, b in items]}))
    state = _state()
    return {"items": len(items), "pending": len(_pending(items, state)), "last_push": state.get("last_sync")}


def push() -> dict:
    """The ``gcal-push`` command: refresh the feed cache, then insert or patch
    queued items up to ``GCAL_MAX_CALLS`` requests. Never raises."""
    try:
        feeds = import_calendars(fetch=True)
        log.info("gcal: feeds %s", ", ".join(f"{f['label']} {len(f.get('events', []))}{' (stale)' if f.get('stale') else ''}" for f in feeds))
    except Exception as e:                  # noqa: BLE001
        log.info("gcal: feeds not refreshed (%s)", e)
    if not GCAL_CREDS.is_file():
        return {"skipped": "no credentials"}
    qp = DB_DIR / "gcal_queue.json"
    if not qp.is_file():
        return {"skipped": "nothing queued"}
    items = [tuple(x) for x in json.loads(qp.read_text())["items"]]
    state = _state()
    todo = _pending(items, state)
    inserted = patched = failed = 0
    if todo:
        try:
            api = _Api()
        except Exception as e:              # noqa: BLE001
            log.info("gcal: no token (%s)", e)
            return {"skipped": f"no token: {e}"}
        if not state["items"]:
            # a fresh state (first run, or the file lost): adopt what an earlier
            # run already put on the calendars rather than inserting it again
            try:
                for cal, c in GCAL.items():
                    for fp, eid in api.list_fps(c["id"], _utc(GCAL_SINCE).isoformat()).items():
                        state["items"][fp] = {"cal": cal, "event_id": eid, "hash": None}
                log.info("gcal: adopted %d existing events", len(state["items"]))
                todo = _pending(items, state)
            except Exception as e:          # noqa: BLE001
                log.warning("gcal: could not list existing events (%s)", e)
                return {"skipped": f"no listing: {e}"}
        for op, cal, fp, body, h in todo:
            if inserted + patched + failed >= GCAL_MAX_CALLS:
                break
            cal_id = GCAL[cal]["id"]
            body = dict(body, extendedProperties={"private": {"fp": fp}})
            try:
                if op == "insert":
                    eid = api.insert(cal_id, body); inserted += 1
                else:
                    eid = state["items"][fp]["event_id"]
                    try:
                        api.patch(cal_id, eid, body)
                    except KeyError:        # deleted by hand: recreate
                        eid = api.insert(cal_id, body)
                    patched += 1
                state["items"][fp] = {"cal": cal, "event_id": eid, "hash": h}
            except Exception as e:          # noqa: BLE001
                failed += 1
                log.warning("gcal %s %s failed: %s", op, fp, e)
                if failed >= 5:
                    break
    state["last_sync"] = datetime.now(timezone.utc).isoformat(timespec="seconds")
    DB_DIR.mkdir(parents=True, exist_ok=True)
    (DB_DIR / "gcal_state.json").write_text(json.dumps(state))
    info = {"items": len(items), "inserted": inserted, "patched": patched, "failed": failed,
            "pending": max(0, len(todo) - inserted - patched)}
    log.info("gcal: %d items, %d inserted, %d patched, %d failed, %d pending", len(items), inserted, patched, failed, info["pending"])
    return info
