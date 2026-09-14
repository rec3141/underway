"""The names layer's banding: which zoom a name first shows at."""

import importlib.util
import unittest
from pathlib import Path

spec = importlib.util.spec_from_file_location("make_names_tiles", Path(__file__).resolve().parents[1] / "tools" / "make_names_tiles.py")
names = importlib.util.module_from_spec(spec)
spec.loader.exec_module(names)


class Banding(unittest.TestCase):
    def test_scale_to_band(self):
        # coarser scales show earlier; the finest go to the last band
        self.assertEqual([names.band_of_scale(s) for s in (30_000_000, 15_000_000, 5_000_000, 1_000_000, 250_000, 50_000)], [2, 3, 4, 7, 9, 11])
        self.assertEqual(names.band_of_scale(0), names.BANDS[-1])

    def test_generic_floor(self):
        self.assertEqual(names.floor_of("Sea"), 2)
        self.assertEqual(names.floor_of("Strait"), 4)
        self.assertEqual(names.floor_of("Détroit"), 4)
        self.assertEqual(names.floor_of("Bay"), 5)
        self.assertEqual(names.floor_of("Île"), 5)
        self.assertEqual(names.floor_of("Lake"), 7)
        self.assertEqual(names.floor_of("Cove"), 9)
        self.assertEqual(names.floor_of("Nunatak"), 8)

    def test_french_generic(self):
        self.assertTrue(names.FRENCH.search("Baie d'Hudson"))
        self.assertTrue(names.FRENCH.search("Détroit d'Hudson"))
        self.assertFalse(names.FRENCH.search("Hudson Bay"))

    def test_specific(self):
        self.assertEqual(names.specific("Nares Stræde"), "nares")
        self.assertEqual(names.specific("Nares Strait"), "nares")
        self.assertEqual(names.specific("Melville Bugt"), "melville")


if __name__ == "__main__":
    unittest.main()
