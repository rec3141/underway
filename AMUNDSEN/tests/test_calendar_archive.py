"""The calendar file the browser fetches every build carries the current
legs; the legs before go to an archive fetched on demand."""

import json
import tempfile
import unittest
from pathlib import Path

from dashboard import calendar
from dashboard.legs import Leg


def _leg(id_, first, live=False):
    year, _, num = id_.partition("_LEG_")
    return Leg(id=id_, year=int(year), number=int(num), first_date=first, live=live)


LEGS = [_leg("2025_LEG_04", "20251001"), _leg("2026_LEG_02", "20260801"), _leg("2026_LEG_03", "20260901", live=True)]
EVENTS = [{"leg": "2025_LEG_04", "time_utc": "2025/10/02 01:00:00", "station": "A"},
          {"leg": "2026_LEG_02", "time_utc": "2026/08/03 01:00:00", "station": "B"},
          {"leg": "2026_LEG_03", "time_utc": "2026/09/02 01:00:00", "station": "C"},
          {"leg": None, "time_utc": "2026/09/03 01:00:00", "station": "D"}]
FEEDS = [{"key": "schedule", "label": "Schedule", "fetched_utc": "x",
          "events": [{"start": "2025-10-02T01:00:00Z", "summary": "old"}, {"start": "2026-09-05T01:00:00Z", "summary": "new"}]},
         {"key": "surprise", "label": "Surprise", "events": [{"start": "2026-09-06", "summary": "recent"}]}]


class PartitionTests(unittest.TestCase):
    def test_live_legs_are_current(self):
        self.assertEqual([l.id for l in calendar.current_legs(LEGS)], ["2026_LEG_03"])

    def test_newest_leg_is_current_when_none_is_live(self):
        legs = [_leg("2025_LEG_04", "20251001"), _leg("2026_LEG_02", "20260801")]
        self.assertEqual([l.id for l in calendar.current_legs(legs)], ["2026_LEG_02"])

    def test_events_and_feed_items_before_the_current_legs_go_to_the_archive(self):
        cur, feeds, archive = calendar.partition(EVENTS, FEEDS, LEGS)
        self.assertEqual([e["station"] for e in cur], ["C", "D"])              # an event without a leg stays
        self.assertEqual([e["station"] for e in archive["events"]], ["A", "B"])
        self.assertEqual(archive["legs"], ["2025_LEG_04", "2026_LEG_02"])
        self.assertEqual(archive["before"], "2026-09-01")
        self.assertEqual([e["summary"] for e in feeds[0]["events"]], ["new"])
        self.assertEqual(feeds[0]["fetched_utc"], "x")                          # the feed keeps its metadata
        self.assertEqual(feeds[1]["events"], FEEDS[1]["events"])
        self.assertEqual(archive["gcal"], [{"key": "schedule", "label": "Schedule", "events": [FEEDS[0]["events"][0]]}])

    def test_nothing_old_means_no_archive(self):
        cur, feeds, archive = calendar.partition(EVENTS[2:], FEEDS[1:], LEGS)
        self.assertIsNone(archive)
        self.assertEqual(len(cur), 2)

    def test_archive_written_once_and_stamped_by_content(self):
        _, _, archive = calendar.partition(EVENTS, FEEDS, LEGS)
        with tempfile.TemporaryDirectory() as d:
            root = Path(d); (root / "data").mkdir()
            meta = calendar.write_archive(root, archive)
            p = root / "data" / calendar.ARCHIVE_FILE
            self.assertEqual(json.loads(p.read_text())["events"], archive["events"])
            self.assertEqual((meta["file"], meta["legs"], meta["before"], meta["events"]), (f"data/{calendar.ARCHIVE_FILE}", archive["legs"], "2026-09-01", 2))
            m1 = p.stat().st_mtime_ns
            again = calendar.write_archive(root, archive)
            self.assertEqual(again["stamp"], meta["stamp"])
            self.assertEqual(p.stat().st_mtime_ns, m1)                          # unchanged content: not rewritten
            other = calendar.write_archive(root, dict(archive, events=archive["events"][:1]))
            self.assertNotEqual(other["stamp"], meta["stamp"])
            self.assertIsNone(calendar.write_archive(root, None))


class BuildTests(unittest.TestCase):
    def test_build_writes_current_file_and_archive(self):
        from unittest.mock import patch
        with tempfile.TemporaryDirectory() as d:
            root = Path(d); (root / "data").mkdir()
            with patch.object(calendar, "fetch_schedule", return_value={"rows": [], "former": []}), \
                 patch("dashboard.gcal.import_calendars", return_value=[dict(f) for f in FEEDS]), \
                 patch("dashboard.gcal.queue", return_value={}), \
                 patch("dashboard.pump.pump_events", return_value=[]):
                meta = calendar.build_calendar(LEGS, root, frame=None, events=list(EVENTS))
            cur = json.loads((root / "data" / "calendar.json").read_text())
            arch = json.loads((root / "data" / calendar.ARCHIVE_FILE).read_text())
            self.assertEqual([e["station"] for e in cur["events"]], ["C", "D"])
            self.assertEqual([e["station"] for e in arch["events"]], ["A", "B"])
            self.assertEqual(cur["archive"]["legs"], ["2025_LEG_04", "2026_LEG_02"])
            self.assertEqual(meta["archive"], cur["archive"])
            self.assertEqual(meta["events"], 4)                                  # the count is of every event
            self.assertEqual(meta["feeds"][0]["key"], "schedule")                # the calendar links, as before


if __name__ == "__main__":
    unittest.main()
