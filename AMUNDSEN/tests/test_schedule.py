"""An intranet schedule row keeps its identity (station × operation) while it
is edited, in the history and on the Google calendar."""

import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from dashboard import calendar, gcal


def _tr(station, op, status, date, start, end, dur, comment=""):
    cells = (station, f'<a href=".\\html_temp\\Alarm_operation_97778.html">{op}</a>', status, date, start, end, dur, comment)
    return '<tr height="30px">' + "".join(f'<th align="center">{c}</th>' for c in cells) + "</tr>"


PAGE = ('<html><body><p>Schedule 2026 Leg 03</p><table><tr><th>Station</th><th>Operations</th><th>Status</th>'
        '<th>Date</th><th>Start</th><th>End</th><th>Duration (hour)</th><th>Comment</th></tr>'
        + _tr("CardS-3", "CTD-Rosette", "Scheduled", "06/09/26", "09:05", "10:05", "1.00")
        + _tr("", "Transit to CardS-1", "", "06/09/26", "17:20", "17:35", "0.25")
        + _tr("CardS-1", "CTD-Rosette", "", "06/09/26", "18:35", "19:35", "1.00")
        + _tr("CardS-1", "CTD-Rosette", "", "07/09/26", "08:00", "09:00", "1.00", "again")
        + "</table></body></html>")


def _row(station, op, date, start, end, status="Scheduled", comment="", key=None):
    r = {"station": station, "operation": op, "status": status, "date": date, "start": start, "end": end,
         "duration_h": None, "comment": comment}
    if key:
        r["key"] = key
    r.update(calendar._instants(r))
    return r


class ParseTests(unittest.TestCase):
    def test_row_key_is_station_and_operation(self):
        rows = calendar.parse_schedule(PAGE)["rows"]
        self.assertEqual([r["key"] for r in rows], ["CardS-3|CTD-Rosette", "|Transit to CardS-1",
                                                    "CardS-1|CTD-Rosette", "CardS-1|CTD-Rosette|1"])
        self.assertEqual([calendar.row_key(r) for r in rows], [r["key"] for r in rows])
        self.assertEqual(calendar.row_key({"station": "CardS-3", "operation": "CTD-Rosette"}), "CardS-3|CTD-Rosette")

    def test_change_report_follows_the_row(self):
        old = {"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:05", "10:05")]}
        moved = {"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35")]}
        self.assertIn("moved to 06/09/26 09:35", calendar._what_changed(old, moved)["text"])
        done = {"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35", status="Completed")]}
        self.assertIn("CTD-Rosette: Completed", calendar._what_changed(moved, done)["text"])
        self.assertIsNone(calendar._what_changed(done, done))


class HistoryTests(unittest.TestCase):
    def test_edited_row_updates_in_place(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(calendar, "DB_DIR", Path(tmp)):
            calendar._remember([_row("CardS-3", "CTD-Rosette", "06/09/26", "09:05", "10:05")], "Leg")
            former = calendar._remember([_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35", status="In progress")], "Leg")
            self.assertEqual(former, [])
            hist = json.loads((Path(tmp) / "schedule_history.json").read_text())
            self.assertEqual(list(hist), ["CardS-3|CTD-Rosette"])
            self.assertEqual((hist["CardS-3|CTD-Rosette"]["start"], hist["CardS-3|CTD-Rosette"]["status"]), ("09:35", "In progress"))
            former = calendar._remember([_row("CardS-1", "CTD", "06/09/26", "12:00", "13:00")], "Leg")
            self.assertEqual([(f["station"], f["start"]) for f in former], [("CardS-3", "09:35")])


class GcalTests(unittest.TestCase):
    def test_schedule_item_is_the_same_when_moved(self):
        r = _row("CardS-3", "CTD-Rosette", "06/09/26", "09:05", "10:05", comment="deep")
        items = gcal.schedule_items({"rows": [r, dict(r, start_utc=None)]})
        self.assertEqual(len(items), 1)
        cal, fp, body = items[0]
        self.assertEqual((cal, fp), ("schedule", "sch|CardS-3|CTD-Rosette"))
        moved = gcal.schedule_items({"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35", comment="deep")]})
        self.assertEqual(moved[0][1], fp)
        self.assertNotEqual(gcal._hash(moved[0][2]), gcal._hash(body))
        self.assertEqual(gcal._pending(moved, {"items": {fp: {"cal": cal, "event_id": "e", "hash": gcal._hash(body)}}})[0][0], "patch")

    def test_push_patches_the_moved_row(self):
        class Api:
            calls = []
            def insert(self, cal_id, body): self.calls.append(("insert", body["extendedProperties"]["private"])); return "new"
            def patch(self, cal_id, eid, body): self.calls.append(("patch", eid, body["extendedProperties"]["private"]))
        items = gcal.schedule_items({"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35")]})
        items.append(("surprise", "pump|x", {"summary": "pump", "extendedProperties": {"private": {"underwayPump": "pump|x"}}}))
        state = {"last_sync": None, "items": {"sch|CardS-3|CTD-Rosette": {"cal": "schedule", "event_id": "b", "hash": "x"}}}
        with tempfile.TemporaryDirectory() as tmp:
            db = Path(tmp)
            (db / "gcal_queue.json").write_text(json.dumps({"items": [list(x) for x in items]}))
            (db / "gcal_state.json").write_text(json.dumps(state))
            with patch.object(gcal, "DB_DIR", db), patch.object(gcal, "_Api", Api), \
                    patch.object(gcal, "import_calendars", lambda fetch: []), patch.object(gcal, "GCAL_CREDS", db / "gcal_queue.json"):
                info = gcal.push()
                self.assertEqual((info["patched"], info["inserted"], info["pending"]), (1, 1, 0))
                self.assertEqual(Api.calls, [("patch", "b", {"fp": "sch|CardS-3|CTD-Rosette"}),
                                             ("insert", {"underwayPump": "pump|x", "fp": "pump|x"})])
                st = json.loads((db / "gcal_state.json").read_text())["items"]
                self.assertEqual(sorted(st), ["pump|x", "sch|CardS-3|CTD-Rosette"])
                self.assertEqual(st["sch|CardS-3|CTD-Rosette"]["event_id"], "b")
                Api.calls.clear()
                self.assertEqual(gcal.push()["patched"], 0)
                self.assertEqual(Api.calls, [])


class AroundNowTests(unittest.TestCase):
    def test_last_completed_in_progress_and_next(self):
        rows = [_row("JSW-01", "Mapping", "06/09/26", "03:15", "06:00", status="Completed"),
                _row("JSW-01", "Box Core - GEO", "06/09/26", "06:00", "07:30", status="Completed"),
                _row("JSW-01", "Gravity Core", "", "", "", status="Canceled"),
                _row("CardS-3", "Deploy Baited Cam", "06/09/26", "08:35", "09:05", status="In progress"),
                _row("CardS-3", "CTD-Rosette", "06/09/26", "09:05", "10:05"),
                _row("CardS-3", "TM-Rosette", "06/09/26", "10:05", "11:05")]
        n = calendar.around_now(rows)
        self.assertEqual(n["completed"]["operation"], "Box Core - GEO")
        self.assertEqual([r["operation"] for r in n["in_progress"]], ["Deploy Baited Cam"])
        self.assertEqual(n["next"]["operation"], "CTD-Rosette")
        self.assertEqual(n["next"]["key"], "CardS-3|CTD-Rosette")
        n = calendar.around_now(rows[:2])
        self.assertEqual((n["completed"]["operation"], n["in_progress"], n["next"]), ("Box Core - GEO", [], None))
        self.assertEqual(calendar.around_now([]), {"completed": None, "in_progress": [], "next": None})


class EventStationTests(unittest.TestCase):
    def test_stations_without_a_cast_come_from_the_event_log(self):
        from dashboard.build import event_stations
        ev = lambda st, act, t, lat, lon, **kw: dict({"leg": "2026_LEG_03", "station": st, "activity": act, "time_utc": t, "lat": lat, "lon": lon}, **kw)
        events = [ev("JSW-01", "Mapping", "2026-09-06T03:15:00", 76.0, -80.0, station_type="Mapping"),
                  ev("JSW-01", "Box Core - GEO", "2026-09-06T06:10:00", 76.01, -80.02, station_type="Benthic", depth_m=410.0, comment="fine mud"),
                  ev("JSW-01", "Box Core - GEO", "2026-09-06T06:40:00", 76.02, -80.01, station_type="Benthic", depth_m=412.0),
                  ev("CardS-3", "CTD-Rosette", "2026-09-06T09:10:00", 77.0, -81.0, station_type="Full"),
                  ev("Transit", "Transit to JSW-01", "2026-09-05T18:25:00", 75.5, -79.5),
                  ev("", "Crew change", "2026-09-05T12:00:00", 75.0, -79.0),
                  ev("Bad", "Nothing", "2026-09-05T12:00:00", None, None)]
        casts = [{"kind": "cast", "leg": "2026_LEG_03", "station": "CardS-3", "cast": "007"}]
        out = event_stations(events, casts)
        self.assertEqual([s["station"] for s in out], ["JSW-01"])
        s = out[0]
        self.assertEqual((s["kind"], s["leg"], s["lat"], s["lon"], s["bottom_m"]), ("event", "2026_LEG_03", 76.01, -80.01, 412.0))
        self.assertEqual((s["time"], s["time_end"]), ("2026-09-06T03:15", "2026-09-06T06:40"))
        self.assertEqual((s["type"], s["activities"], s["n_events"], s["comments"]), ("Benthic", ["Box Core - GEO", "Mapping"], 3, "fine mud"))


if __name__ == "__main__":
    unittest.main()
