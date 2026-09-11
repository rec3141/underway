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
    def test_near_archive_retains_moved_boxes_and_original_bounds(self):
        now = datetime(2026, 9, 11, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)), patch.object(sat, "ARCHIVE_MAX", 1):
            info = {"images": {}}
            first = {"corners": [[0, 1], [1, 1], [1, 0], [0, 0]], "centre": [0.5, 0.5], "size": [4800, 3200]}
            second = {**first, "corners": [[1, 1], [2, 1], [2, 0], [1, 0]]}
            sat.archive(info, "s1near", b"first", now.isoformat(), now, first)
            sat.archive(info, "s1near", b"duplicate", now.isoformat(), now, first)
            sat.archive(info, "s1near", b"moved", now.isoformat(), now, second)
            entries = info["archive"]["s1near"]
            self.assertEqual(len(entries), 2)
            self.assertNotEqual(entries[0]["file"], entries[1]["file"])
            self.assertEqual(entries[0]["corners"], first["corners"])
            self.assertEqual((sat.sat_dir()/"archive"/entries[0]["file"]).read_bytes(), b"first")

    def test_migrate_current_near_cache_without_download(self):
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)):
            sat.sat_dir().mkdir()
            im = dict(file="s1near.webp", fetched="2026-09-11T12:00:00+00:00", scene="2026-09-11T10:00:00Z",
                      corners=[[0, 1], [1, 1], [1, 0], [0, 0]], label="Radar 50 m")
            (sat.sat_dir()/im["file"]).write_bytes(b"near")
            (sat.sat_dir()/"sat.json").write_text(json.dumps({"images": {"s1near": im}}))
            self.assertEqual(sat.preserve_near_cache(), 1)
            self.assertEqual(sat.preserve_near_cache(), 0)
            out = sat.publish(Path(d)/"www")
            entry = out["archive"]["s1near"][0]
            self.assertEqual(entry["corners"], im["corners"])
            self.assertTrue((Path(d)/"www"/entry["url"]).is_file())

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
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": fresh, "s1near": near, "s2near": near}}, now, ship=(77.5, -91.8)), [])
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": fresh, "s1near": near, "s2near": near}}, now, ship=(77.5, -94.1)), ["s1near", "s2near"])   # ~55 km east, past NEAR_MOVE_KM
        self.assertEqual(sat.due({"images": {"s1": fresh, "s2": fresh}}, now, ship=(77.5, -91.8)), ["s1near", "s2near"])

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

    def test_backfill_fills_the_days_without_a_picture(self):
        now = datetime(2026, 9, 8, 15, 0, tzinfo=timezone.utc)
        scenes = {("s1", "2026-09-06"): "2026-09-06T21:06:45Z", ("s1", "2026-09-07"): "2026-09-07T20:00:00Z", ("s2", "2026-09-07"): "2026-09-07T21:11:50Z"}
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)), \
             patch.object(sat, "credentials", return_value=("id", "secret")), patch.object(sat, "token", return_value="tok"), \
             patch.object(sat, "render", return_value=(b"webp", 4.4, (1850, 1850))) as render, \
             patch.object(sat, "newest_scene", side_effect=lambda tok, k, bbox, start, end, **kw: scenes.get((k, f"{start:%Y-%m-%d}"))) as catalog:
            (Path(d) / "sat" / "archive").mkdir(parents=True)
            had = {"file": "s1_20260907103037.webp", "scene": "2026-09-07T10:30:37Z", "fetched": "x"}
            (Path(d) / "sat" / "sat.json").write_text(json.dumps({"images": {}, "archive": {"s1": [had]}}))
            info = sat.backfill(datetime(2026, 9, 5, tzinfo=timezone.utc), within_km=500, now=now, ship=(78.5, -92.0))
            self.assertEqual(render.call_count, 2)                           # s1 the 6th, s2 the 7th; s1 the 7th was there, the 5th and 8th have no scene
            self.assertEqual(catalog.call_count, 7)                          # 4 days x 2 sensors, less the day s1 already had
            s1 = info["archive"]["s1"]
            self.assertEqual([e["scene"] for e in s1], ["2026-09-06T21:06:45Z", "2026-09-07T10:30:37Z"])   # in scene order
            self.assertEqual(s1[0]["file"], "s1_20260906210645.webp")
            self.assertTrue((Path(d) / "sat" / "archive" / "s1_20260906210645.webp").is_file())
            self.assertEqual(len(s1[0]["corners"]), 4)
            self.assertEqual(s1[0]["centre"], [78.5, -92.0])
            self.assertEqual(info["archive"]["s2"][0]["scene"], "2026-09-07T21:11:50Z")
            self.assertAlmostEqual(info["cost_pu_total"], 8.8)
            self.assertEqual(json.loads((Path(d) / "sat" / "sat.json").read_text())["archive"]["s2"][0]["file"], "s2_20260907211150.webp")
            # the box is the wanted size: 1000 km across at the ship's latitude
            bbox = render.call_args[0][2]
            w, s_, e, n = bbox; lat = 78.5
            self.assertAlmostEqual((e - w) * __import__("math").cos(__import__("math").radians(lat)) / 1000, 1000, delta=1)

    def test_backfill_refreshes_an_expired_token_and_reports_failures(self):
        now = datetime(2026, 9, 8, 15, 0, tzinfo=timezone.utc)
        tokens = iter(["tok1", "tok2", "tok3"])
        def catalog(tok, k, bbox, start, end, raise_errors=False):
            if tok == "tok1":
                raise RuntimeError("401 Unauthorized")                    # expired: tried again with a fresh token
            if f"{start:%Y-%m-%d}" == "2026-09-07":
                raise RuntimeError("catalog down")                        # fails twice: the day is reported failed
            return "2026-09-06T21:06:45Z" if f"{start:%Y-%m-%d}" == "2026-09-06" else None
        with tempfile.TemporaryDirectory() as d, patch.object(sat, "DB_DIR", Path(d)), \
             patch.object(sat, "credentials", return_value=("id", "secret")), patch.object(sat, "token", side_effect=lambda c: next(tokens)) as token, \
             patch.object(sat, "render", return_value=(b"webp", 4.4, (10, 10))), \
             patch.object(sat, "newest_scene", side_effect=catalog), self.assertLogs("dashboard.satellite", level="INFO") as logs:
            (Path(d) / "sat").mkdir()
            info = sat.backfill(datetime(2026, 9, 6, tzinfo=timezone.utc), kinds=("s1",), now=now, ship=(78.5, -92.0))
        self.assertEqual([e["scene"][:10] for e in info["archive"]["s1"]], ["2026-09-06"])
        self.assertEqual(token.call_count, 3)                                # the start, the expiry, the failing day
        self.assertTrue(any("2026-09-07 failed" in m for m in logs.output))
        self.assertFalse(any("2026-09-07: no scene" in m for m in logs.output))

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
