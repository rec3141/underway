"""Discovery tests use temporary shares, never the ship's mounted data."""
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from dashboard.legs import RootsUnavailable, discover


class DiscoveryTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.data = Path(tmp.name) / "Data"
        self.share = Path(tmp.name) / "Share"
        for name, value in [("DATA_ROOT", self.data), ("SHARE_ROOT", self.share)]:
            p = patch("dashboard.legs." + name, value)
            p.start()
            self.addCleanup(p.stop)

    def add_leg(self, leg, date, archive=False, mtime=100):
        root = self.share / leg[:4] if archive else self.data / "FULL_CSV"
        folder = root / leg
        folder.mkdir(parents=True, exist_ok=True)
        file = folder / f"ACSD_{date}.csv"
        file.touch()
        os.utime(file, (mtime, mtime))
        return folder

    def live_id(self):
        legs = discover()
        live = [leg for leg in legs if leg.live]
        self.assertEqual(len(live), 1)
        self.assertTrue(live[0].meta()["live"])
        return live[0].id

    def test_newest_observation_wins_over_archive_mtime(self):
        self.add_leg("2025_LEG_04", "20251001", archive=True, mtime=2000)
        self.add_leg("2026_LEG_03", "20260904", mtime=1000)
        self.assertEqual(self.live_id(), "2026_LEG_03")

    def test_boundary_day_tie_prefers_later_leg(self):
        self.add_leg("2026_LEG_02", "20260801", mtime=2000)
        self.add_leg("2026_LEG_03", "20260801", mtime=1000)
        self.assertEqual(self.live_id(), "2026_LEG_03")

    def test_same_leg_across_roots_merges_dates_and_live_flag(self):
        self.add_leg("2026_LEG_03", "20260903", archive=True)
        self.add_leg("2026_LEG_03", "20260904")
        legs = discover()
        self.assertEqual(len(legs), 1)
        self.assertTrue(legs[0].live)
        self.assertEqual(legs[0].first_date, "20260903")
        self.assertEqual(legs[0].last_date, "20260904")
        self.assertEqual(len(legs[0].indirs), 2)

    def test_either_root_alone_remains_usable(self):
        self.add_leg("2025_LEG_04", "20251001", archive=True)
        self.assertEqual(self.live_id(), "2025_LEG_04")

    def test_unavailable_roots_raise(self):
        with self.assertRaises(RootsUnavailable):
            discover()

    def test_reachable_but_empty_roots_raise(self):
        (self.data / "FULL_CSV" / "2026_LEG_03").mkdir(parents=True)
        self.share.mkdir()
        with self.assertRaises(RootsUnavailable):
            discover()


if __name__ == "__main__":
    unittest.main()
