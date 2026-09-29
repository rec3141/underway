"""The public copy carries only what Amundsen Science has released."""
import json
import tempfile
import unittest
from pathlib import Path

import numpy as np
import pandas as pd

from dashboard import public


def frame(start: str, hours: float, step_s: int = 10) -> pd.DataFrame:
    idx = pd.date_range(start, periods=int(hours * 3600 / step_s), freq=f"{step_s}s", tz="UTC")
    n = len(idx)
    return pd.DataFrame({
        "lat": np.linspace(70, 71, n), "lon": np.linspace(-100, -99, n), "dist_km": np.arange(n, dtype=float),
        "SST (°C)": np.full(n, 1.5), "Air temperature (°C)": np.full(n, -2.0),
        "Heading (°)": np.where(np.arange(n) % 2, 359.0, 1.0),
        "leg": np.full(n, 3.0), "provisional": np.zeros(n),
    }, index=idx)


class PublicFrameTests(unittest.TestCase):
    def test_before_the_cutoff_tsg_and_navigation_stay_per_minute(self):
        out = public.public_frame(frame("2025-08-01T00:00Z", 1))
        self.assertEqual(len(out), 60)
        self.assertTrue(out["SST (°C)"].notna().all())
        self.assertTrue(out["Heading (°)"].notna().all())
        air = out["Air temperature (°C)"].dropna()
        self.assertEqual(list(air.index.minute), [0, 15, 30, 45])

    def test_from_the_cutoff_positions_every_5_minutes_readings_every_15(self):
        out = public.public_frame(frame("2026-09-29T00:00Z", 1))
        self.assertEqual(list(out.index.minute), list(range(0, 60, 5)))
        self.assertTrue(out["lat"].notna().all())
        for name in ("SST (°C)", "Air temperature (°C)", "Heading (°)"):
            self.assertEqual(list(out[name].dropna().index.minute), [0, 15, 30, 45], name)
        self.assertEqual(out["leg"].iloc[0], 3.0)

    def test_headings_average_as_directions(self):
        out = public.public_frame(frame("2026-09-29T00:00Z", 1))
        heading = out["Heading (°)"].dropna()
        self.assertTrue(((heading < 1) | (heading > 359)).all(), heading.tolist())

    def test_a_record_across_the_cutoff_keeps_both_rules(self):
        out = public.public_frame(frame("2025-12-31T23:00Z", 2))
        early, late = out[out.index < public.PER_MINUTE_UNTIL], out[out.index >= public.PER_MINUTE_UNTIL]
        self.assertEqual(len(early), 60)
        self.assertEqual(len(late), 12)

    def test_windows_are_no_finer_than_the_readings(self):
        windows = public.public_windows()
        self.assertTrue(windows)
        self.assertTrue(all(w.hours >= public.MIN_WINDOW_HOURS for w in windows))
        self.assertTrue(all(w.step_s >= public.READING_STEP_S for w in windows))
        self.assertNotIn("1h", [w.label for w in windows])


class RestrictTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = root = Path(self.tmp.name)
        (root / "data/casts/2024_LEG_02").mkdir(parents=True)
        (root / "data/casts/2026_LEG_03").mkdir(parents=True)
        (root / "data/casts/2024_LEG_02/CTD_001.json").write_text("{}")
        (root / "data/casts/2026_LEG_03/CTD_001.json").write_text("{}")
        (root / "data/casts/index.json").write_text(json.dumps({"ladcp_file": "data/casts/ladcp.json", "variables": [], "casts": [
            {"id": "2024_LEG_02:CTD_001", "leg": "2024_LEG_02", "file": "data/casts/2024_LEG_02/CTD_001.json"},
            {"id": "2026_LEG_03:CTD_001", "leg": "2026_LEG_03", "file": "data/casts/2026_LEG_03/CTD_001.json"}]}))
        (root / "data/casts/ladcp.json").write_text(json.dumps({"casts": [
            {"parent_cast_id": "2026_LEG_03:CTD_001"}, {"parent_cast_id": "2024_LEG_02:CTD_001"}]}))
        (root / "data/calendar.json").write_text(json.dumps({"schedule": {"rows": [], "whiteboard": "science meeting at 8"}}))
        for label in ("1h", "12h", "leg"):
            (root / f"data/w-{label}.json").write_text("{}")
        (root / "data/track").mkdir()
        for chunk in ("aaaaaaaaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbbbbbbbbb"):
            (root / f"data/track/{chunk}.json").write_text("{}")
        (root / "data/manifest.json").write_text(json.dumps({
            "default_window": "leg", "casts": {"n": 2},
            "track": {"levels": [{"chunks": [{"file": "data/track/aaaaaaaaaaaaaaaaaaaaaaaa.json"}]}]},
            "windows": [{"label": "12h", "file": "data/w-12h.json"}, {"label": "leg", "file": "data/w-leg.json"}],
            "calendar": {"update": {"kind": "whiteboard", "text": "Whiteboard: science meeting at 8"}}}))

    def tearDown(self):
        self.tmp.cleanup()

    def test_withholds_the_whiteboard_uncatalogued_casts_and_stale_windows(self):
        public.restrict(self.root)
        manifest = json.loads((self.root / "data/manifest.json").read_text())
        self.assertNotIn("update", manifest["calendar"])
        self.assertEqual(manifest["casts"]["n"], 1)
        self.assertEqual(json.loads((self.root / "data/calendar.json").read_text())["schedule"]["whiteboard"], "")
        index = json.loads((self.root / "data/casts/index.json").read_text())
        self.assertEqual([c["leg"] for c in index["casts"]], ["2024_LEG_02"])
        ladcp = json.loads((self.root / "data/casts/ladcp.json").read_text())
        self.assertEqual([c["parent_cast_id"] for c in ladcp["casts"]], ["2024_LEG_02:CTD_001"])
        rules = (self.root / ".public-filter").read_text().splitlines()
        self.assertEqual(sorted(rules), ["H /data/casts/2026_LEG_03/CTD_001.json",
                                        "H /data/track/bbbbbbbbbbbbbbbbbbbbbbbb.json", "H /data/w-1h.json"])

    def test_a_second_run_changes_nothing(self):
        public.restrict(self.root)
        first = {p: p.read_bytes() for p in self.root.rglob("*.json")}
        public.restrict(self.root)
        self.assertEqual(first, {p: p.read_bytes() for p in self.root.rglob("*.json")})

    def test_a_default_window_the_public_copy_lacks_falls_back_to_its_first(self):
        path = self.root / "data/manifest.json"
        path.write_text(json.dumps({**json.loads(path.read_text()), "default_window": "6h"}))
        public.restrict(self.root)
        self.assertEqual(json.loads(path.read_text())["default_window"], "12h")


if __name__ == "__main__":
    unittest.main()
