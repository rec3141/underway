"""What a build keeps for the next: leg frames keyed on their store, the
surprise scoring before the live leg, and the long windows' entries."""

import json
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

import numpy as np
import pandas as pd

from dashboard import buildcache as bc
from dashboard.config import SURPRISE
from dashboard.surprise import score_minutes, surprise_scores


class FrameCacheTests(unittest.TestCase):
    def test_frame_read_once_while_store_and_columns_hold(self):
        with tempfile.TemporaryDirectory() as d, patch.object(bc, "CACHE_DIR", Path(d) / "build"):
            db = Path(d) / "leg.db"; db.write_bytes(b"one")
            reads = []
            frame = pd.DataFrame({"a": [1.0, 2.0]}, index=pd.to_datetime(["2026-09-01", "2026-09-02"], utc=True))
            read = lambda: (reads.append(1), frame)[1]
            self.assertTrue(bc.cached_frame("L", db, ["a", "b"], read).equals(frame))
            self.assertTrue(bc.cached_frame("L", db, ["a", "b"], read).equals(frame))
            self.assertEqual(len(reads), 1)                                      # the second came from the pickle
            bc.cached_frame("L", db, ["a"], read)
            self.assertEqual(len(reads), 2)                                      # other columns: read again
            db.write_bytes(b"one more")
            bc.cached_frame("L", db, ["a"], read)
            self.assertEqual(len(reads), 3)                                      # the store changed: read again
            (Path(d) / "build" / "frames" / "L.pkl").write_bytes(b"garbage")
            self.assertTrue(bc.cached_frame("L", db, ["a"], read).equals(frame))  # unreadable pickle: read again
            self.assertEqual(len(reads), 4)


def _minutes(days=40, seed=1):
    rng = np.random.default_rng(seed)
    idx = pd.date_range("2026-07-01", periods=days * 1440, freq="1min", tz="UTC")
    n = len(idx); t = np.arange(n) / 1440
    base = {"tsg — hull temperature (deg c)": 2 + np.sin(t / 3) + rng.normal(0, .05, n),
            "tsg — salinity (psu)": 31 + np.cos(t / 5) + rng.normal(0, .02, n),
            "tsg — fluorescence (ug/l)": np.exp(rng.normal(0, .3, n))}
    m = pd.DataFrame(base, index=idx)
    m.iloc[n // 2: n // 2 + 600] += 3.0                                        # a front, well before the cutoff
    m.iloc[-300:] -= 2.0                                                        # and one in the live leg
    return m.iloc[::1].drop(m.index[1000:1400])                                 # a gap


class SurpriseCacheTests(unittest.TestCase):
    def test_live_leg_scored_from_kept_scaling_matches_the_full_scoring(self):
        minute = _minutes()
        cutoff = pd.Timestamp("2026-08-05", tz="UTC")
        full = surprise_scores(minute, SURPRISE)
        with tempfile.TemporaryDirectory() as d, patch.object(bc, "CACHE_DIR", Path(d)):
            calls = []
            def counting(m, cfg, stats=None):
                calls.append(len(m)); return score_minutes(m, cfg, stats)
            first = bc.surprise_cached(minute, SURPRISE, cutoff, "frozen-1", counting)
            self.assertTrue((Path(d) / "surprise.pkl").is_file())
            pd.testing.assert_frame_equal(first, full)
            later = pd.concat([minute, pd.DataFrame({c: [minute[c].iloc[-1]] for c in minute.columns},
                                                    index=[minute.index[-1] + pd.Timedelta(minutes=1)])])
            second = bc.surprise_cached(later, SURPRISE, cutoff, "frozen-1", counting)
            self.assertEqual(calls[1], int((later.index >= cutoff - bc.SURPRISE_WARMUP).sum()))   # the live part and its warm-up
            # before the cutoff: the kept scoring, unchanged (its scaling is pinned);
            # from it: within a hair of scoring the longer record whole
            pd.testing.assert_frame_equal(second[second.index < cutoff], first[first.index < cutoff])
            fresh = surprise_scores(later, SURPRISE)
            new, ref = second[second.index >= cutoff], fresh[fresh.index >= cutoff]
            self.assertEqual(len(new), len(ref))
            diff = np.nanmax(np.abs(new.to_numpy() - ref.to_numpy()))
            self.assertLess(diff, 1e-2, f"scores from the warm-up differ from the full scoring by {diff}")   # −log10 p; alerts fire at 3
            # the legs before changed: scored whole again
            bc.surprise_cached(later, SURPRISE, cutoff, "frozen-2", counting)
            self.assertEqual(calls[2], len(later))


class LongWindowTests(unittest.TestCase):
    def test_long_window_kept_between_reslices(self):
        with tempfile.TemporaryDirectory() as d, patch.object(bc, "CACHE_DIR", Path(d) / "build"):
            f = Path(d) / "w-1y.json"; f.write_text("{}")
            meta = {"label": "1y", "hours": 8760, "step_s": 3600, "file": "data/w-1y.json", "n": 5}
            bc.remember_windows([meta, {"label": "1h", "step_s": 10, "file": "data/w-1h.json"}])
            at = lambda minute: datetime(2026, 9, 8, 12, minute, tzinfo=timezone.utc)
            self.assertEqual(bc.kept_window("1y", 3600, f, at(3)), meta)
            self.assertIsNone(bc.kept_window("1y", 3600, f, at(10)))            # due
            self.assertIsNone(bc.kept_window("1h", 10, Path(d) / "w-1h.json", at(3)))   # fine-binned: every minute
            f.unlink()
            self.assertIsNone(bc.kept_window("1y", 3600, f, at(3)))             # no file to keep


if __name__ == "__main__":
    unittest.main()
