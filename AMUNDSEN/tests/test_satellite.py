"""Satellite pictures: the box around the ship, its corners for the map, and
when a new one is due. No network."""
import json
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import patch

from dashboard import satellite as sat


class SatelliteTests(unittest.TestCase):
    def test_region_tiles_cover_the_picture_exactly(self):
        bbox = sat.region_bbox()
        size = sat.region_size(bbox)
        self.assertTrue(2500 < size[0] < 5000 and 2000 < size[1] < 4000, size)
        ts = sat.tiles(bbox, size, (2, 2))
        self.assertEqual(len(ts), 4)
        self.assertTrue(all(t[1][0] <= 2500 and t[1][1] <= 2500 for t in ts))   # the service's cap
        self.assertEqual(ts[0][2], (0, 0)); self.assertEqual(ts[-1][2][0] + ts[-1][1][0], size[0]); self.assertEqual(ts[-1][2][1] + ts[-1][1][1], size[1])
        self.assertAlmostEqual(ts[0][0][3], bbox[3]); self.assertAlmostEqual(ts[-1][0][1], bbox[1])   # top tile at the top, bottom at the bottom
        c = sat.corners(bbox)
        self.assertAlmostEqual(c[0][0], -130); self.assertAlmostEqual(c[0][1], 83.5, places=6)
        self.assertAlmostEqual(c[2][0], -60); self.assertAlmostEqual(c[2][1], 74, places=6)

    def test_due_by_age_and_region(self):
        now = datetime(2026, 9, 7, 12, 0, tzinfo=timezone.utc)
        fresh = {"fetched": (now - timedelta(hours=1)).isoformat(), "region": list(sat.REGION)}
        old = {"fetched": (now - timedelta(hours=7)).isoformat(), "region": list(sat.REGION)}
        elsewhere = {"fetched": now.isoformat(), "region": [0, 0, 1, 1]}
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": old}}, now), ["s2"])          # s2 past six hours; no fix: the near box waits
        self.assertEqual(sat.due({"images": {"s1": elsewhere, "s2": fresh}}, now), ["s1"])    # another region
        self.assertEqual(sat.due({}, now), ["s1", "s2"])                                        # nothing yet
        near = {"fetched": now.isoformat(), "centre": [77.5, -91.8]}
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": fresh, "s1near": near}}, now, ship=(77.5, -91.8)), [])
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": fresh, "s1near": near}}, now, ship=(77.5, -94.1)), ["s1near"])   # ~55 km east, past NEAR_MOVE_KM
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": fresh}}, now, ship=(77.5, -91.8)), ["s1near"])

    def test_archive_keeps_new_scenes_only_and_prunes(self):
        now = datetime(2026, 9, 7, 12, 0, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)), patch.object(sat, "ARCHIVE_MAX", 2):
            info = {}
            sat.archive(info, "s1", b"a", "2026-09-07T10:30:37Z", now)
            sat.archive(info, "s1", b"a", "2026-09-07T10:30:37Z", now + timedelta(hours=3))   # the same newest scene: skipped
            self.assertEqual([e["file"] for e in info["archive"]["s1"]], ["s1_20260907103037.webp"])
            sat.archive(info, "s1", b"b", "2026-09-07T13:00:00Z", now + timedelta(hours=3))
            sat.archive(info, "s1", b"c", None, now + timedelta(hours=6))                      # no scene time: dated by the fetch
            self.assertEqual([e["file"] for e in info["archive"]["s1"]], ["s1_20260907130000.webp", "s1_20260907180000.webp"])
            self.assertFalse((Path(d) / "sat" / "archive" / "s1_20260907103037.webp").exists())   # pruned with its entry
            self.assertTrue((Path(d) / "sat" / "archive" / "s1_20260907180000.webp").exists())

    def test_refresh_without_credentials_renders_nothing(self):
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)), patch.dict("os.environ", {"COPERNICUS_ID": "", "COPERNICUS_SECRET": ""}):
            self.assertEqual(sat.refresh(force=True), {})
            self.assertFalse((Path(d) / "sat").exists())

    def test_refresh_renders_only_a_new_scene(self):
        now = datetime(2026, 9, 8, 15, 0, tzinfo=timezone.utc)
        old = (now - timedelta(hours=7)).isoformat(timespec="seconds")     # past both sensors' max age
        seen = {"s1": "2026-09-07T21:06:45Z", "s2": "2026-09-08T00:32:17Z"}
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)), \
             patch.object(sat, "credentials", return_value=("id", "secret")), patch.object(sat, "token", return_value="tok"), \
             patch.object(sat, "ship_position", return_value=(76.0, -90.0)), \
             patch.object(sat, "render", return_value=(b"webp", 15.0, (10, 10))) as render, \
             patch.object(sat, "newest_scene", side_effect=lambda tok, k, *a: seen[k]) as catalog:
            (Path(d) / "sat").mkdir()
            info = {"images": {"s1": {"file": "s1.webp", "fetched": old, "scene": seen["s1"], "region": list(sat.REGION), "corners": []},
                               "s2": {"file": "s2.webp", "fetched": old, "scene": "2026-09-07T00:12:33Z", "region": list(sat.REGION), "corners": []}}}
            (Path(d) / "sat" / "sat.json").write_text(json.dumps(info))
            out = sat.refresh(now=now, kinds=("s1", "s2"))
            self.assertEqual(catalog.call_count, 2)                          # both were due by age
            self.assertEqual(render.call_count, 1)                           # only the optical had a new scene
            self.assertEqual(out["images"]["s2"]["scene"], seen["s2"])
            self.assertEqual(out["images"]["s1"]["fetched"], old)            # the radar picture stays as it was
            self.assertEqual(json.loads((Path(d) / "sat" / "sat.json").read_text())["images"]["s2"]["scene"], seen["s2"])
            # the catalog down: nothing is bought, nothing is rewritten
            catalog.side_effect = lambda *a: None
            stamp = (Path(d) / "sat" / "sat.json").stat().st_mtime_ns
            info2 = json.loads((Path(d) / "sat" / "sat.json").read_text())
            for im in info2["images"].values():
                im["fetched"] = old
            (Path(d) / "sat" / "sat.json").write_text(json.dumps(info2)); stamp = (Path(d) / "sat" / "sat.json").stat().st_mtime_ns
            sat.refresh(now=now, kinds=("s1", "s2"))
            self.assertEqual(render.call_count, 1)
            self.assertEqual((Path(d) / "sat" / "sat.json").stat().st_mtime_ns, stamp)
            # force buys everything regardless
            sat.refresh(force=True, now=now, kinds=("s1", "s2"))
            self.assertEqual(render.call_count, 3)

    def test_publish_copies_pictures_and_versions_urls(self):
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)):
            (Path(d) / "sat").mkdir(); (Path(d) / "sat" / "s1.webp").write_bytes(b"webp")
            (Path(d) / "sat" / "sat.json").write_text(json.dumps({"images": {"s1": {"file": "s1.webp", "fetched": "2026-09-07T12:00:00+00:00", "corners": [[0, 1], [1, 1], [1, 0], [0, 0]], "label": "Sentinel-1 radar"}}}))
            root = Path(d) / "www"
            out = sat.publish(root)
            self.assertTrue((root / "data" / "sat" / "s1.webp").is_file())
            self.assertEqual(out["images"]["s1"]["url"], "data/sat/s1.webp?v=20260907120000")
            self.assertEqual(out["archive"], {})
            (Path(d) / "sat" / "archive").mkdir(); (Path(d) / "sat" / "archive" / "s1_20260907103037.webp").write_bytes(b"w")
            info = json.loads((Path(d) / "sat" / "sat.json").read_text()); info["archive"] = {"s1": [{"file": "s1_20260907103037.webp", "scene": "2026-09-07T10:30:37Z", "fetched": "2026-09-07T12:00:00+00:00"}]}
            (Path(d) / "sat" / "sat.json").write_text(json.dumps(info))
            out = sat.publish(root)
            self.assertEqual(out["archive"]["s1"][0]["url"], "data/sat/archive/s1_20260907103037.webp")
            self.assertTrue((root / "data" / "sat" / "archive" / "s1_20260907103037.webp").is_file())


if __name__ == "__main__":
    unittest.main()
