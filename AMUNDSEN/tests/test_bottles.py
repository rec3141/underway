"""The SeaBird .btl parser: firings with pressure, depth and time."""
import tempfile
import unittest
from pathlib import Path

from dashboard.casts import parse_btl

BTL = """* Sea-Bird SBE 9 Data File:
# start_time = Sep 07 2026 09:07:36 [System UpLoad Time]
    Bottle        Date      Sal00       PrDM      DepSM      T090C
  Position        Time
      1    Sep 07 2026    34.3981    368.874    364.687     0.0383 (avg)
              09:15:28                  0.050      0.050     0.0001 (sdev)
     19    Sep 07 2026    34.3974    349.828    345.873     0.0366 (avg)
              09:17:57                  0.056      0.055     0.0002 (sdev)
"""


class BottleTests(unittest.TestCase):
    def test_parse(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "CTD_2026_03_008.btl"; p.write_text(BTL)
            b = parse_btl(p)
        self.assertEqual([x["bottle"] for x in b], [1, 19])
        self.assertAlmostEqual(b[0]["p"], 368.874); self.assertAlmostEqual(b[0]["depth_m"], 364.687)
        self.assertEqual(b[0]["time"], "2026-09-07T09:15:28"); self.assertEqual(b[1]["time"], "2026-09-07T09:17:57")


if __name__ == "__main__":
    unittest.main()
