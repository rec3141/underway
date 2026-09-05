from datetime import datetime, timezone
from pathlib import Path
import shutil
import json
import tempfile
import unittest

try:
    from PIL import Image
except ImportError:
    Image = None

from dashboard.cameras import frames, timelapse, build_leg, compose_frame


@unittest.skipIf(Image is None, "Install the cameras extra for image tests")
class CameraTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.source = self.root / "source"
        self.source.mkdir()
        self.end = datetime(2026, 9, 5, 12, tzinfo=timezone.utc)

    def add(self, stamp, mosaic=False):
        folder = self.source / stamp[:8]
        if mosaic:
            folder /= stamp[8:]
        folder.mkdir(parents=True, exist_ok=True)
        name = f"Camera360_{stamp}_mosaic.jpg" if mosaic else f"{stamp}_11.jpg"
        path = folder / name
        Image.new("RGB", (320, 240), "navy").save(path)
        return path

    def test_portrait_layout(self):
        path = self.add("20260905110000", True)
        for number, color in ((1,"red"),(2,"green"),(3,"blue")):
            image = Image.new("RGB", (160,90), color)
            image.paste("white", (0,0,160,20))
            image.save(path.with_name(path.name.replace("_mosaic",f"_cam_{number}")))
        result = compose_frame(path,540,"portrait")
        self.assertEqual(result.size,(540,960))
        self.assertGreater(result.getpixel((270,240))[1],100)
        self.assertGreater(result.getpixel((135,319))[1],100)
        self.assertGreater(result.getpixel((135,320))[0],240)
        self.assertGreater(result.getpixel((405,320))[2],240)
        self.assertGreater(result.getpixel((135,720))[0],240)
        self.assertGreater(result.getpixel((405,720))[2],240)
        # Opposite rotations move the original sky to the outside edges.
        self.assertTrue(all(c > 230 for c in result.getpixel((10,720))))
        self.assertTrue(all(c > 230 for c in result.getpixel((530,720))))
        with self.assertRaises(ValueError):
            compose_frame(path,640,"portrait")
        path.with_name(path.name.replace("_mosaic","_cam_3")).unlink()
        with self.assertRaises(OSError):
            compose_frame(path,540,"portrait")

    def test_selection_window_and_sampling(self):
        self.add("20260905100000")
        self.add("20260905110000", True)
        self.add("20260905110030", True)
        self.add("20260905113000", True)
        selected = frames(self.source, self.end, hours=1, interval=120)
        self.assertEqual(len(selected), 2)
        self.assertEqual(selected[0][0].hour, 11)

    def test_ice_proxy_and_dark_rejection(self):
        from dashboard.ice import estimate
        path = self.add("20260905110000")
        image = Image.new("RGB", (100,100), "black")
        image.paste("white", (50,0,100,100))
        image.save(path)
        result = estimate(path, [0,0,1,1], threshold=128)
        self.assertAlmostEqual(result["ice_fraction_proxy"], .5, places=2)
        Image.new("RGB", (100,100), "black").save(path)
        self.assertIsNone(estimate(path, [0,0,1,1])["ice_fraction_proxy"])
        with self.assertRaises(ValueError):
            estimate(path, [0,0,2,1])

    def test_reference_model_and_explicit_exclusion(self):
        from dashboard.ice import estimate, train
        path = self.add("20260905110000")
        image = Image.new("RGB", (100,100), "navy")
        image.paste("white", (50,0,100,100))
        image.save(path)
        calibration = self.root / "patches.json"
        calibration.write_text(json.dumps([
            {"file":str(path.relative_to(self.source)),"label":"water","roi":[0,0,.4,1]},
            {"file":str(path.relative_to(self.source)),"label":"ice","roi":[.6,0,1,1]}]))
        model = train(self.source,calibration)
        self.assertAlmostEqual(estimate(path,[0,0,1,1],model=model)["ice_fraction_proxy"],.5,places=2)
        self.assertIsNone(estimate(path,[0,0,1,1],model=model,exclude_reason="fog")["ice_fraction_proxy"])

    def test_missing_or_corrupt_frames_preserve_previous(self):
        output = self.root / "out"
        output.mkdir()
        (output / "latest.mp4").write_bytes(b"previous video")
        self.add("20260905110000").write_bytes(b"bad image")
        self.add("20260905113000").write_bytes(b"bad image")
        with self.assertRaises(ValueError):
            timelapse(self.source, output, end=self.end)
        self.assertEqual((output / "latest.mp4").read_bytes(), b"previous video")

    @unittest.skipUnless(shutil.which("ffmpeg"), "ffmpeg not installed")
    def test_real_encode_and_metadata(self):
        self.add("20260905110000")
        self.add("20260905113000")
        output = self.root / "out"
        result = timelapse(self.source, output, end=self.end, width=320)
        self.assertEqual(len(result["frames"]), 2)
        self.assertGreater((output / "latest.mp4").stat().st_size, 100)
        self.assertTrue((output / "latest.json").is_file())

    @unittest.skipUnless(shutil.which("ffmpeg"), "ffmpeg not installed")
    def test_daily_boundaries_stitch_and_unchanged_skip(self):
        self.add("20260904233000")
        self.add("20260904235000")
        self.add("20260905000000")
        self.add("20260905113000")
        output = self.root / "leg"
        days = build_leg(self.source, output, width=320, now=self.end)
        self.assertEqual([d["utc_day"] for d in days], ["20260904", "20260905"])
        self.assertTrue(days[0]["complete_day"])
        self.assertFalse(days[1]["complete_day"])
        self.assertTrue(days[0]["end_utc"].startswith("2026-09-04"))
        self.assertTrue(days[1]["start_utc"].startswith("2026-09-05"))
        before = (output / "full-leg.mp4").stat().st_mtime_ns
        build_leg(self.source, output, width=320, now=self.end)
        self.assertEqual((output / "full-leg.mp4").stat().st_mtime_ns, before)


if __name__ == "__main__":
    unittest.main()
