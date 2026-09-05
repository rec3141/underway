"""Compass means agree across plotted windows and aggregate exports."""
import json
from types import SimpleNamespace
import unittest

import numpy as np
import pandas as pd

from dashboard.build import _circular_mean_deg, aggregate

WIND = "True wind direction (°)"
TEMP = "SST (°C)"


class AggregateTests(unittest.TestCase):
    def result(self, wind, rule="1h"):
        frame = pd.DataFrame({WIND: wind, TEMP: list(range(len(wind))),
                              "lat": 70., "lon": -60., "leg": 0},
                             index=pd.date_range("2026-09-04", periods=len(wind), freq="min", tz="UTC"))
        result = aggregate(SimpleNamespace(frame=frame), rule)
        json.dumps(result, allow_nan=False)
        return result["rows"][0]

    def test_wraparound_hourly_and_daily(self):
        for rule in ["1h", "1D"]:
            row = self.result([359., 1.], rule)
            self.assertEqual(row[WIND], [0., 1., 359., 2])
            self.assertEqual(row[TEMP], [0.5, 0., 1., 2])

    def test_missing_and_single_direction(self):
        self.assertEqual(self.result([np.nan, 42.])[WIND], [42., 42., 42., 1])
        self.assertIsNone(self.result([np.nan, np.nan])[WIND])

    def test_opposing_directions_have_no_mean_but_keep_count(self):
        for values in [[0., 180.], [90., 270.], [0., 90., 180., 270.]]:
            result = self.result(values)[WIND]
            self.assertIsNone(result[0])
            self.assertEqual(result[3], len(values))
            self.assertTrue(np.isnan(_circular_mean_deg(pd.Series(values))))

    def test_ordinary_mean_and_rounded_north(self):
        self.assertEqual(self.result([10., 20.])[WIND][0], 15.)
        self.assertEqual(self.result([359.99999])[WIND][0], 0.)
        self.assertEqual(_circular_mean_deg(pd.Series([359., 1.])), 0.)


if __name__ == "__main__":
    unittest.main()
