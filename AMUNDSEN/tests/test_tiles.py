"""The GEBCO tile pyramid is described to the map from its directory alone."""

import tempfile
import unittest
from pathlib import Path

from dashboard.build import raster_pyramid


def make_pyramid(root: Path, levels: dict[int, tuple[int, int, int, int]]) -> Path:
    tiles = root / "gebco"
    for z, (x0, y0, x1, y1) in levels.items():
        for x in range(x0, x1 + 1):
            d = tiles / str(z) / str(x)
            d.mkdir(parents=True)
            for y in range(y0, y1 + 1):
                (d / f"{y}.png").write_bytes(b"")
    return tiles


class TilesTests(unittest.TestCase):
    def test_missing_pyramid(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertIsNone(raster_pyramid(Path(tmp) / "gebco"))
            (Path(tmp) / "gebco").mkdir()
            self.assertIsNone(raster_pyramid(Path(tmp) / "gebco"))

    def test_globe_then_box(self):
        with tempfile.TemporaryDirectory() as tmp:
            # the globe at z0-2, then a box at z3 covering tiles x 1..2, y 0..1
            tiles = make_pyramid(Path(tmp), {0: (0, 0, 0, 0), 1: (0, 0, 1, 1), 2: (0, 0, 3, 3), 3: (1, 0, 2, 1)})
            r = raster_pyramid(tiles)
            self.assertEqual(r["sources"][0], {"minzoom": 0, "maxzoom": 2, "bounds": None})
            box = r["sources"][1]
            self.assertEqual((box["minzoom"], box["maxzoom"]), (3, 3))
            self.assertEqual(box["bounds"], [-135.0, 66.513260, -45.0, 85.051129])   # x 1..2 of 8 is lon -135..-45; y 0..1 is lat 66.5..85
            self.assertEqual(len(r["sources"]), 2)
            self.assertIn("{z}/{x}/{y}.png?v=", r["url"])

    def test_box_only_bounds_come_from_the_finest_zoom(self):
        with tempfile.TemporaryDirectory() as tmp:
            # z2 covers the whole north-west quarter; z3 only tiles x 1..2, y 1..2 of it
            tiles = make_pyramid(Path(tmp), {2: (0, 0, 1, 1), 3: (1, 1, 2, 2)})
            r = raster_pyramid(tiles)
            self.assertEqual(len(r["sources"]), 1)
            self.assertEqual((r["sources"][0]["minzoom"], r["sources"][0]["maxzoom"]), (2, 3))
            self.assertEqual(r["sources"][0]["bounds"], [-135.0, 40.979898, -45.0, 79.171335])


if __name__ == "__main__":
    unittest.main()
