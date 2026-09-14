import json
import os
import time
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import numpy as np
import pandas as pd

from dashboard.track import publish_track, _select
from dashboard.derive import haversine_km


def frame(n=31):
    return pd.DataFrame({'lat': np.full(n, 70.), 'lon': np.arange(n) * .001,
        'dist_km': np.arange(n) * .038, 'leg': np.ones(n), 'pump_low': np.zeros(n),
        'provisional': np.zeros(n), 'SST (°C)': np.arange(n) / 10},
        index=pd.date_range('2026-09-13 23:58:00', periods=n, freq='10s', tz='UTC'))


class TrackTests(unittest.TestCase):
    def test_spacing_and_turns_preserve_native_observations(self):
        f = frame(100)
        selected = _select(f, .1)
        self.assertLess(len(selected), len(f))
        lengths = haversine_km(f.lat.to_numpy()[:-1], f.lon.to_numpy()[:-1], f.lat.to_numpy()[1:], f.lon.to_numpy()[1:])
        distance = np.r_[0, np.cumsum(lengths)]
        self.assertTrue((np.diff(distance[selected]) <= .1000001).all())
        turn = frame(5)
        turn['lon'] = [0, .0001, .0002, .0002, .0002]
        turn['lat'] = [70, 70, 70, 70.0001, 70.0002]
        self.assertIn(2, _select(turn, .025))

    def test_one_km_level_reduces_points_with_bounded_spacing(self):
        f = frame(1000)
        with tempfile.TemporaryDirectory() as directory:
            manifest = publish_track(f, Path(directory))
            coarse, finer = manifest['levels'][:2]
            self.assertEqual(coarse['spacing_km'], 1)
            self.assertEqual(finer['spacing_km'], .1)
            self.assertLess(sum(c['n'] for c in coarse['chunks']), sum(c['n'] for c in finer['chunks']))
        selected = _select(f, 1)
        distance = np.r_[0, np.cumsum(haversine_km(f.lat.to_numpy()[:-1], f.lon.to_numpy()[:-1], f.lat.to_numpy()[1:], f.lon.to_numpy()[1:]))]
        self.assertTrue((np.diff(distance[selected]) <= 1.0000001).all())

    def test_bounded_chunks_overlap_and_aligned_values(self):
        f = frame()
        with tempfile.TemporaryDirectory() as directory, patch('dashboard.track.CHUNK_ROWS', 8):
            root = Path(directory)
            manifest = publish_track(f, root)
            for level in manifest['levels']:
                previous = None
                seen = set()
                for entry in level['chunks']:
                    payload = json.loads((root / entry['file']).read_text())
                    self.assertLessEqual(payload['n'], 8)
                    self.assertEqual(len(payload['vars']['SST (°C)']), payload['n'])
                    self.assertEqual(len(payload['pump_low']), payload['n'])
                    if previous:
                        self.assertEqual(previous['t'][-1], payload['t'][0])
                    for t, lat, lon, value in zip(payload['t'], payload['lat'], payload['lon'], payload['vars']['SST (°C)']):
                        row = f.loc[pd.Timestamp(t, unit='ms', tz='UTC')]
                        self.assertAlmostEqual(value, row['SST (°C)'])
                        west, south, east, north = entry['bounds']
                        self.assertTrue(west <= lon <= east and south <= lat <= north)
                    seen.update(payload['t'])
                    previous = payload
                if level['spacing_km'] == 0:
                    self.assertEqual(len(seen), len(f))
            self.assertEqual(manifest, publish_track(f, root))

    def test_leg_missing_position_time_gap_and_jump_break(self):
        f = frame(8)
        f.loc[f.index[2], 'lat'] = np.nan
        f.loc[f.index[4]:, 'leg'] = 2
        f.index = f.index[:6].append(f.index[6:] + pd.Timedelta(hours=2))
        f.iloc[7, f.columns.get_loc('lon')] = 20
        with tempfile.TemporaryDirectory() as directory:
            manifest = publish_track(f, Path(directory))
            entries = manifest['levels'][-1]['chunks']
            self.assertEqual(len({entry['segment'] for entry in entries}), 5)
            self.assertEqual(sum(entry['n'] for entry in entries), 7)

    def test_cache_reuses_completed_days_and_recovers_missing_chunks(self):
        from dashboard.track import _payload
        f = frame(40)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            first = publish_track(f.iloc[:31], root)
            with patch('dashboard.track._payload', wraps=_payload) as serialize:
                self.assertEqual(first, publish_track(f.iloc[:31], root))
                serialize.assert_not_called()
            extended = publish_track(f, root)
            self.assertEqual(first['levels'][0]['chunks'][0], extended['levels'][0]['chunks'][0])
            missing = root / extended['levels'][-1]['chunks'][0]['file']
            missing.unlink()
            self.assertEqual(extended, publish_track(f, root))
            self.assertTrue(missing.exists())

    def test_pruning_keeps_current_recent_and_unrelated_files(self):
        from dashboard.track import prune_track
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = publish_track(frame(), root)
            current = root / manifest['levels'][0]['chunks'][0]['file']
            destination = current.parent
            stale = destination / ('a' * 24 + '.json')
            recent = destination / ('b' * 24 + '.json')
            unrelated = destination / 'notes.json'
            index = destination / ('c' * 24 + '.index')
            for path in (stale, recent, unrelated, index):
                path.write_text('{}')
            old = time.time() - 8 * 86400
            for path in (stale, current, unrelated, index):
                os.utime(path, (old, old))
            self.assertEqual(prune_track(root, manifest), 2)
            self.assertFalse(stale.exists())
            self.assertFalse(index.exists())
            self.assertTrue(all(p.exists() for p in (current, recent, unrelated)))

    def test_reused_index_refreshes_before_pruning(self):
        from dashboard.track import prune_track
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            f = frame()
            manifest = publish_track(f, root)
            indexes = list((root / 'data/track').glob('*.index'))
            old = time.time() - 8 * 86400
            for path in indexes:
                os.utime(path, (old, old))
            self.assertEqual(manifest, publish_track(f, root))
            self.assertEqual(prune_track(root, manifest), 0)
            self.assertTrue(all(path.exists() and path.stat().st_mtime > old for path in indexes))

    def test_empty(self):
        with tempfile.TemporaryDirectory() as directory:
            self.assertEqual(publish_track(frame(0), Path(directory))['levels'][0]['chunks'], [])

    def test_dateline_bounds_cover_crossing_without_changing_native_longitudes(self):
        f = frame(3)
        f['lon'] = [179.999, -180., -179.999]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = publish_track(f, root)
            for level in manifest['levels']:
                entry = level['chunks'][0]
                west, _, east, _ = entry['bounds']
                self.assertLess(west, 180.)
                self.assertGreater(east, 180.)
                self.assertLess(east - west, .01)
                payload = json.loads((root / entry['file']).read_text())
                self.assertEqual(payload['lon'][0], 179.999)
                self.assertEqual(payload['lon'][-1], -179.999)
