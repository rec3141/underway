import json
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

from dashboard.pump import FLOW, pump_events
from dashboard.build import slice_window
from dashboard.config import Window
from dashboard import gcal


class PumpTests(unittest.TestCase):
    def frame(self, flow):
        return pd.DataFrame({FLOW:flow, "leg":0, "lat":70., "lon":-60.,
                             "dist_km":0., "SST (°C)":np.arange(len(flow), dtype=float)},
                            index=pd.date_range("2026-09-05", periods=len(flow), freq="min", tz="UTC"))

    def test_episodes_cutoff_missing_and_recovery(self):
        f = self.frame([1., .06, 0., .5, np.nan, .1, np.nan, .1])
        events = pump_events(f, [SimpleNamespace(id="2026_LEG_03")])
        self.assertEqual(len(events), 3)
        self.assertEqual(pd.Timestamp(events[0]["time_utc"]), f.index[1])
        self.assertEqual(pd.Timestamp(events[0]["end_utc"]), f.index[3])
        self.assertIn("flow restored", events[0]["comment"])
        self.assertIn("coverage ended", events[1]["comment"])
        self.assertEqual(len({e["id"] for e in events}), 3)
        self.assertEqual(pump_events(f.drop(columns=FLOW), []), [])

    def test_gaps_and_leg_boundaries_split_episodes(self):
        f = self.frame([0., 0., 0., 0.])
        f.loc[f.index[2:], "leg"] = 1
        events = pump_events(f, [SimpleNamespace(id="a"), SimpleNamespace(id="b")])
        self.assertEqual([e["leg"] for e in events], ["a", "b"])
        f = self.frame([0., 0., 0.]).iloc[[0, 2]]
        self.assertEqual(len(pump_events(f, [SimpleNamespace(id="a")])), 2)

    def test_window_flags_any_low_sample_without_changing_values(self):
        f = self.frame([1., 0., 1., np.nan])
        result = slice_window(SimpleNamespace(frame=f), Window("test", 1, 120), f.index[-1])
        self.assertEqual(result["tsg_low"], [True, False])
        self.assertEqual(result["vars"]["SST (°C)"], [.5, 2.5])
        self.assertNotIn("_tsg_low", f)
        json.dumps(result, allow_nan=False)

    def test_ongoing_episode_extends_without_new_identity(self):
        legs = [SimpleNamespace(id="a")]
        first = pump_events(self.frame([1.,0.]), legs)[0]
        second = pump_events(self.frame([1.,0.,0.]), legs)[0]
        self.assertEqual(first["id"], second["id"])
        self.assertLess(first["end_utc"], second["end_utc"])
        self.assertEqual(pump_events(self.frame([np.nan, np.nan]), legs), [])

    def test_google_queue_keeps_individual_episodes_stable(self):
        events = pump_events(self.frame([0.,1.,0.]), [SimpleNamespace(id="a")])
        with tempfile.TemporaryDirectory() as tmp, patch.object(gcal, "DB_DIR", Path(tmp)), \
             patch.object(gcal, "GCAL_SINCE", "2026-01-01"), patch.dict("os.environ", {"UNDERWAY_GCAL":"1"}):
            gcal.queue(events, {"rows":[]}, None)
            first = json.loads((Path(tmp) / "gcal_queue.json").read_text())["items"]
            gcal.queue(events, {"rows":[]}, None)
            second = json.loads((Path(tmp) / "gcal_queue.json").read_text())["items"]
        self.assertEqual(first, second)
        self.assertEqual(len(first), 2)
        self.assertTrue(all(item[0] == "surprise" for item in first))


if __name__ == "__main__":
    unittest.main()
