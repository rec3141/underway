"""Native track payloads retain observations even across long spans."""
from types import SimpleNamespace
import unittest

import pandas as pd

from dashboard.build import FINE_STEP_S, slice_window
from dashboard.config import Window


class TrackDetailTests(unittest.TestCase):
    def test_native_points_are_not_averaged_on_long_spans(self):
        index = pd.date_range('2025-01-01', periods=12, freq='5s', tz='UTC').append(
            pd.date_range('2026-01-01', periods=12, freq='5s', tz='UTC'))
        frame = pd.DataFrame({'lat': 70., 'lon': -60., 'dist_km': range(24),
                              'leg': 0, 'SST (°C)': range(24)}, index=index)
        result = slice_window(SimpleNamespace(frame=frame), Window('2y', 24 * 730, FINE_STEP_S), index[-1])
        self.assertEqual(result['t'], index.as_unit('ms').asi8.tolist())
        self.assertEqual(result['vars']['SST (°C)'], list(range(24)))
        self.assertEqual(result['n'], 24)
