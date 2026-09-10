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

    def test_updated_stamp_moves_only_when_the_row_changes(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(calendar, "DB_DIR", Path(tmp)):
            hist_p = Path(tmp) / "schedule_history.json"
            row = _row("CardS-3", "CTD-Rosette", "06/09/26", "09:05", "10:05")
            calendar._remember([row], "Leg")
            first = json.loads(hist_p.read_text())["CardS-3|CTD-Rosette"]["updated_utc"]
            self.assertTrue(first)
            old = {"CardS-1|CTD": {"first_seen": "2026-09-05T10:00:00+00:00", "last_seen": "2026-09-06T10:00:00+00:00", "start_utc": "x", "station": "CardS-1"}}
            hist_p.write_text(json.dumps(dict(json.loads(hist_p.read_text()), **old)))
            calendar._remember([row], "Leg")
            hist = json.loads(hist_p.read_text())
            self.assertEqual(hist["CardS-1|CTD"]["updated_utc"], "2026-09-06T10:00:00+00:00")     # a former row from before the stamp: when it left
            self.assertEqual(hist["CardS-3|CTD-Rosette"]["updated_utc"], first)
            with patch.object(calendar, "datetime", wraps=calendar.datetime) as dt:
                from datetime import datetime as real
                dt.now.return_value = real(2026, 9, 9, 18, 40, tzinfo=calendar.timezone.utc)
                calendar._remember([row], "Leg")                                       # seen again, unchanged
                self.assertEqual(json.loads(hist_p.read_text())["CardS-3|CTD-Rosette"]["updated_utc"], first)
                calendar._remember([dict(row, status="Completed")], "Leg")
                self.assertEqual(json.loads(hist_p.read_text())["CardS-3|CTD-Rosette"]["updated_utc"], "2026-09-09T18:40:00+00:00")
                canceled = dict(row, status="Canceled", date="", start="", end="", start_utc=None, end_utc=None)
                calendar._remember([canceled], "Leg")
                h = json.loads(hist_p.read_text())["CardS-3|CTD-Rosette"]
                self.assertEqual((h["status"], h["start"]), ("Canceled", "09:05"))


class GcalTests(unittest.TestCase):
    def test_schedule_item_is_the_same_when_moved(self):
        r = _row("CardS-3", "CTD-Rosette", "06/09/26", "09:05", "10:05", comment="deep")
        items = gcal.schedule_items({"rows": [r, dict(r, start_utc=None)]}, history={})
        self.assertEqual(len(items), 1)
        cal, fp, body = items[0]
        self.assertEqual((cal, fp), ("schedule", "sch|CardS-3|CTD-Rosette"))
        moved = gcal.schedule_items({"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35", comment="deep")]})
        self.assertEqual(moved[0][1], fp)
        self.assertNotEqual(gcal._hash(moved[0][2]), gcal._hash(body))
        self.assertEqual(gcal._pending(moved, {"items": {fp: {"cal": cal, "event_id": "e", "hash": gcal._hash(body)}}})[0][0], "patch")

    def test_canceled_row_without_times_keeps_the_remembered_ones(self):
        r = _row("Norwegian Bay 1", "Tucker Net", "07/09/26", "20:50", "21:20")
        hist = {"Norwegian Bay 1|Tucker Net": dict(r)}
        canceled = dict(r, status="Canceled", date="", start="", end="", start_utc=None, end_utc=None)
        items = gcal.schedule_items({"rows": [canceled]}, history=hist)
        self.assertEqual(len(items), 1)
        self.assertEqual(items[0][1], "sch|Norwegian Bay 1|Tucker Net")
        self.assertEqual(items[0][2]["summary"], "[Norwegian Bay 1] [Tucker Net] [Canceled]")
        self.assertEqual(items[0][2]["start"], gcal._when(r["start_utc"]))
        self.assertEqual(gcal.schedule_items({"rows": [canceled]}, history={}), [])       # nothing remembered: nothing to update

    def test_summary_carries_the_last_change(self):
        r = _row("MSG-5", "CTD-Rosette", "09/09/26", "15:40", "16:40", status="")
        hist = {"MSG-5|CTD-Rosette": dict(r, updated_utc="2026-09-09T18:40:00+00:00", last_seen="2026-09-09T20:00:00+00:00")}
        (_, _, body), = gcal.schedule_items({"rows": [r]}, history=hist)
        self.assertEqual(body["summary"], "[MSG-5] [CTD-Rosette] [Scheduled] [last updated Sep 9 14:40 EDT]")
        self.assertIn("Updated: Sep 9 14:40 EDT", body["description"])
        transit = _row("", "Transit to MSG-5", "09/09/26", "14:40", "15:40", status="In progress")
        (_, _, body), = gcal.schedule_items({"rows": [transit]}, history={"|Transit to MSG-5": {"first_seen": "2026-09-09T18:40:00+00:00"}})
        self.assertEqual(body["summary"], "[Transit to MSG-5] [In progress] [last updated Sep 9 14:40 EDT]")

    def test_former_rows_are_kept_up_to_date_and_dropped_ones_deleted(self):
        done = dict(_row("JSW-01", "Mapping", "06/09/26", "03:15", "06:00", status="Completed"), former=True)
        canceled = dict(_row("GF-5", "Gravity Core", "09/09/26", "08:40", "09:10", status="Canceled"), former=True)
        dropped = dict(_row("GF-PC2", "Piston Core", "09/09/26", "16:15", "19:00", status=""), former=True)
        coming = _row("MSG-5", "CTD-Rosette", "09/09/26", "15:40", "16:40", status="")
        items = gcal.schedule_items({"rows": [coming], "former": [done, canceled, dropped]}, history={})
        self.assertEqual([(fp, None if b is None else b["summary"].split("] [")[2].rstrip("]")) for _, fp, b in items],
                         [("sch|MSG-5|CTD-Rosette", "Scheduled"), ("sch|JSW-01|Mapping", "Completed"),
                          ("sch|GF-5|Gravity Core", "Canceled"), ("sch|GF-PC2|Piston Core", None)])
        state = {"items": {"sch|GF-PC2|Piston Core": {"cal": "schedule", "event_id": "p", "hash": "x"}}}
        ops = {fp: op for op, _, fp, _, _ in gcal._pending(items, state)}
        self.assertEqual(ops["sch|GF-PC2|Piston Core"], "delete")
        self.assertEqual(ops["sch|JSW-01|Mapping"], "insert")
        self.assertEqual(gcal._pending([("schedule", "sch|GF-PC2|Piston Core", None)], {"items": {}}), [])   # never pushed: nothing to delete

    def test_push_patches_the_moved_row(self):
        class Api:
            calls = []
            def insert(self, cal_id, body): self.calls.append(("insert", body["extendedProperties"]["private"])); return "new"
            def patch(self, cal_id, eid, body): self.calls.append(("patch", eid, body["extendedProperties"]["private"]))
            def delete(self, cal_id, eid): self.calls.append(("delete", eid))
        items = gcal.schedule_items({"rows": [_row("CardS-3", "CTD-Rosette", "06/09/26", "09:35", "10:35")]}, history={})
        items.append(("surprise", "pump|x", {"summary": "pump", "extendedProperties": {"private": {"underwayPump": "pump|x"}}}))
        items.append(("schedule", "sch|GF-PC2|Piston Core", None))
        state = {"last_sync": None, "items": {"sch|CardS-3|CTD-Rosette": {"cal": "schedule", "event_id": "b", "hash": "x"},
                                              "sch|GF-PC2|Piston Core": {"cal": "schedule", "event_id": "p", "hash": "y"}}}
        with tempfile.TemporaryDirectory() as tmp:
            db = Path(tmp)
            (db / "gcal_queue.json").write_text(json.dumps({"items": [list(x) for x in items]}))
            (db / "gcal_state.json").write_text(json.dumps(state))
            with patch.object(gcal, "DB_DIR", db), patch.object(gcal, "_Api", Api), \
                    patch.object(gcal, "import_calendars", lambda fetch: []), patch.object(gcal, "GCAL_CREDS", db / "gcal_queue.json"):
                info = gcal.push()
                self.assertEqual((info["patched"], info["inserted"], info["deleted"], info["pending"]), (1, 1, 1, 0))
                self.assertEqual(Api.calls, [("patch", "b", {"fp": "sch|CardS-3|CTD-Rosette"}),
                                             ("insert", {"underwayPump": "pump|x", "fp": "pump|x"}), ("delete", "p")])
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


class ProvisionalTests(unittest.TestCase):
    def test_tail_is_unbounded_and_archived(self):
        import pandas as pd
        from dashboard import tsg
        idx = pd.date_range("2026-09-07T02:21Z", periods=90, freq="min")
        frame = pd.DataFrame({"t_sbe38": 0.2, "sal": 31.3, "lat": 76.0, "lon": -89.0, "flow": 1.4}, index=idx)
        cols = ["tsg — hull temperature (deg c)", "tsg — salinity (psu)", "posmv — latitude (deg n)", "posmv — longitude (deg e)"]
        tail = tsg.provisional_tail(frame, pd.Timestamp("2026-09-07T02:20Z"), cols)
        self.assertEqual(len(tail), 90)                                          # every minute the ACSD lacks, not just half an hour
        with tempfile.TemporaryDirectory() as tmp, patch.object(tsg, "DB_DIR", Path(tmp)):
            self.assertEqual(tsg.archive_tail(tail), 90)
            self.assertEqual(tsg.archive_tail(tail.iloc[60:]), 0)                # the same minutes again add nothing
            later = tsg.provisional_tail(frame, pd.Timestamp("2026-09-07T03:00Z"), cols)
            self.assertEqual(tsg.archive_tail(later), 0)
            import csv
            rows = list(csv.DictReader(open(Path(tmp) / "provisional_tsg.csv")))
            self.assertEqual(len(rows), 90); self.assertEqual(rows[0]["time_utc"], "2026-09-07T02:21:00Z")


if __name__ == "__main__":
    unittest.main()


class LiveScrapeTests(unittest.TestCase):
    def test_snapshots_are_recorded_and_replayed_as_minutes(self):
        import pandas as pd
        from dashboard import livescrape
        secs = lambda t, lat, sst: [
            {"title": "Navigation data", "rows": [["Time (UTC)", t], ["Latitude", lat], ["Longitude", "89° 12.6412' W"], ["Speed (knt)", "9.90"], ["Heading (deg)", "322.80"], ["Depth (m)", "139.82"]]},
            {"title": "Atmospheric data (21.6 meters high)", "rows": [["Wind speed (knt)", "11.64"], ["Wind direction (deg)", "326.40"], ["Pressure (hPa)", "997.20"], ["Temperature (deg C)", "0.70"], ["Humidity (%)", "90.21"]]},
            {"title": "Sea water surface data (7 meters depth)", "rows": [["Temperature (deg C)", sst], ["Salinity (psu)", "31.65"], ["Oxygene (ml/L)", "8.43"], ["EcoCdom (mg/m³)", "5.78"]]},
            {"title": "Rosette data", "rows": [["Rosette Depth (m)", "19.79"]]}]
        with tempfile.TemporaryDirectory() as tmp, patch.object(livescrape, "DB_DIR", Path(tmp)):
            livescrape._last_t = None
            self.assertTrue(livescrape.record(secs("2026/09/07 07:46:57", "76° 45.1293' N", "0.01")))
            self.assertFalse(livescrape.record(secs("2026/09/07 07:46:57", "76° 45.1293' N", "0.01")))   # the clock did not move
            self.assertTrue(livescrape.record(secs("2026/09/07 07:47:02", "76° 45.1400' N", "0.03")))
            self.assertTrue(livescrape.record(secs("2026/09/07 07:48:10", "76° 45.2000' N", "0.05")))
            files = list((Path(tmp) / "live_scrape").glob("*.jsonl"))
            self.assertEqual([p.name for p in files], ["20260907.jsonl"])
            cols = ["posmv — latitude (deg n)", "posmv — longitude (deg e)", "tsg — hull temperature (deg c)", "avos — air temperature (deg c)",
                    "multibeam — bottom depth (m)", "avos — true wind direction (deg)", "ctd-rosette — rosette depth (m)"]
            tail = livescrape.provisional_tail(pd.Timestamp("2026-09-07T07:46:00Z"), cols)
            self.assertEqual(len(tail), 3)                                            # the 07:46, 07:47 and 07:48 minutes
            first = tail.iloc[0]
            self.assertAlmostEqual(first["posmv — latitude (deg n)"], 76 + 45.1293 / 60, places=5)
            self.assertAlmostEqual(first["posmv — longitude (deg e)"], -(89 + 12.6412 / 60), places=5)
            self.assertAlmostEqual(first["tsg — hull temperature (deg c)"], 0.01, places=6)
            self.assertEqual(first["avos — air temperature (deg c)"], 0.7)
            self.assertEqual(first["multibeam — bottom depth (m)"], 139.82)
            self.assertTrue(pd.isna(first["avos — true wind direction (deg)"]))    # the page's wind direction is not true wind
            self.assertEqual(len(livescrape.provisional_tail(pd.Timestamp("2026-09-07T07:49:00Z"), cols)), 0)
            naive = livescrape.provisional_tail(pd.Timestamp("2026-09-07T07:46:00"), cols)
            self.assertIsNone(naive.index.tz)
