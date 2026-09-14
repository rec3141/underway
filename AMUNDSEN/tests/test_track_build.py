"""Build publication integration, without ship sources or deployed writes."""
import json
import sqlite3
import tempfile
import unittest
from contextlib import ExitStack
from pathlib import Path
from unittest.mock import Mock, patch

import numpy as np
import pandas as pd

from dashboard import build as module
from dashboard.config import Window
from dashboard.derive import Analysis
from dashboard.legs import Leg


class TrackBuildTests(unittest.TestCase):
    def test_build_publishes_chunks_and_migrates_cached_window_metadata(self):
        index = pd.date_range('2026-09-13', periods=40, freq='10s', tz='UTC')
        frame = pd.DataFrame({'lat': 70., 'lon': -80 + np.arange(40) * .0001,
            'dist_km': np.arange(40) * .004, 'pump_low': 0., 'SST (°C)': 1.}, index=index)
        conn = sqlite3.connect(':memory:')
        conn.execute('CREATE TABLE columns (display TEXT, first_seen TEXT)')
        conn.execute('CREATE TABLE files (name TEXT)')
        conn.execute("INSERT INTO files VALUES ('synthetic.csv')")
        store = Mock(conn=conn)
        store.time_range.return_value = (index.min(), index.max())
        store.display_map.return_value = {'sample': 'sample'}
        store.column_map.return_value = {'sample': 'sample'}
        leg = Leg('2026_LEG_01', 2026, 1)
        cached = {}

        def kept(label, *args):
            return cached.get(label)

        def remember(windows):
            cached.update({w['label']: {**w, 'fine_file': 'data/obsolete-fine.json', 'fine_n': 999} for w in windows})

        patches = {
            'discover': Mock(return_value=[leg]),
            'sync': Mock(return_value={'files_total': 1, 'files_loaded': 0}),
            'Store': Mock(return_value=store),
            'needed_keys': Mock(return_value=(list(frame.columns), [], [], [])),
            'cached_frame': Mock(side_effect=lambda *args: frame.copy()),
            'build_analysis': Mock(side_effect=lambda *args, **kwargs: Analysis(frame.copy(), [], 'synthetic', [], 'test')),
            'WINDOWS': [Window('6h', 6, 60)],
            'kept_window': kept,
            'remember_windows': remember,
            'read_stations': Mock(return_value=[]),
            'event_stations': Mock(return_value=[]),
            'camera_index': Mock(return_value=[]),
            '_alerts_info': Mock(return_value={}),
            '_source_info': Mock(return_value={}),
            'raster_pyramid': Mock(return_value=None),
            'vector_tiles': Mock(return_value=None),
        }
        with tempfile.TemporaryDirectory() as directory, ExitStack() as stack:
            root = Path(directory)
            for name, replacement in patches.items():
                stack.enter_context(patch.object(module, name, replacement))
            for target, result in {
                'dashboard.tsg.minute_frame': None,
                'dashboard.casts.build_casts': {'casts': [], 'variables': []},
                'dashboard.calendar.read_eventlog': [],
                'dashboard.calendar.build_calendar': {},
                'dashboard.satellite.publish': None,
                'dashboard.plan.publish': None,
                'dashboard.history.publish': None,
            }.items():
                stack.enter_context(patch(target, return_value=result))
            first_track = None
            for _ in range(2):
                module.build(root, 'Synthetic track', [])
                manifest = json.loads((root / 'data/manifest.json').read_text())
                self.assertEqual(manifest['track']['version'], 1)
                self.assertTrue(manifest['track']['levels'][0]['chunks'])
                for level in manifest['track']['levels']:
                    for chunk in level['chunks']:
                        self.assertTrue((root / chunk['file']).is_file())
                self.assertFalse(any(k.startswith('fine_') for w in manifest['windows'] for k in w))
                self.assertFalse(list((root / 'data').glob('*-fine.json')))
                self.assertIn('track-data.js', (root / 'index.html').read_text())
                if first_track is not None:
                    self.assertEqual(first_track, manifest['track'])
                first_track = manifest['track']
        conn.close()
