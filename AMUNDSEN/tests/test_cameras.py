from datetime import datetime, timezone
from pathlib import Path
import shutil
import tempfile
import unittest

try:
    from PIL import Image
except ImportError:
    Image = None

from dashboard.cameras import frames, timelapse


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


if __name__ == "__main__":
    unittest.main()
