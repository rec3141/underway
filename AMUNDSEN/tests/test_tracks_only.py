import json
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
import unittest
from unittest.mock import MagicMock, patch
from contextlib import ExitStack
import pandas as pd
from dashboard import build as b
from dashboard.config import Window
from dashboard.legs import RootsUnavailable

class TracksOnlyTests(unittest.TestCase):
    def test_requires_manifest(self):
        with TemporaryDirectory() as tmp, patch.object(b, 'discover') as discover:
            with self.assertRaisesRegex(ValueError, 'requires the ship manifest'):
                b.build(Path(tmp), '', [], tracks_only=True)
            discover.assert_not_called()

    def test_bad_sources_preserve_manifest(self):
        for discovery in (RootsUnavailable('missing'), [SimpleNamespace(id='wrong')],
                          [SimpleNamespace(id='2026_LEG_03'), SimpleNamespace(id='extra')]):
            with self.subTest(discovery=discovery), TemporaryDirectory() as tmp:
                root = Path(tmp); (root / 'data').mkdir()
                manifest = root / 'data/manifest.json'
                original = '{"legs": [{"id": "2026_LEG_03"}]}'
                manifest.write_text(original)
                kw = {'side_effect': discovery} if isinstance(discovery, Exception) else {'return_value': discovery}
                with patch.object(b, 'discover', **kw), self.assertRaises((RootsUnavailable, ValueError)):
                    b.build(root, '', [], tracks_only=True)
                self.assertEqual(manifest.read_text(), original)
                self.assertFalse(list((root / 'data').glob('w-*.json')))

    def test_native_points_and_ship_products(self):
        with TemporaryDirectory() as tmp, ExitStack() as stack:
            root = Path(tmp); (root / 'data').mkdir()
            original = {'legs': [{'id': '2026_LEG_03'}], 'calendar': {'keep': 1}, 'casts': {'keep': 2}, 'history': {'keep': 3}, 'sources': {'calendar': 'kept'}, 'variables': [], 'windows': []}
            (root / 'data/manifest.json').write_text(json.dumps(original))
            (root / 'index.html').write_text('ship shell')
            leg = SimpleNamespace(id='2026_LEG_03', db=root / 'leg.db', indirs=[], live=True)
            index = pd.date_range('2026-09-13', periods=3, freq='5s')
            frame = pd.DataFrame({'lat': [70., 70.00001, 70.00002], 'lon': [-60., -60.00001, -60.00002], 'dist_km': [0., 1., 2.]}, index=index)
            store = MagicMock(); store.time_range.return_value = (index[0], index[-1]); store.display_map.return_value = {}; store.column_map.return_value = {}
            def execute(sql):
                result = MagicMock(); result.__iter__.return_value = iter([])
                result.fetchone.return_value = ('file.csv',) if 'MAX' in sql else (1,)
                return result
            store.conn.execute.side_effect = execute
            for name, value in {'discover': [leg], 'sync': {'files_total': 1, 'files_loaded': 1}, 'Store': store, 'needed_keys': (list(frame.columns), [], [], []), 'cached_frame': frame, 'build_analysis': SimpleNamespace(frame=frame.copy())}.items():
                stack.enter_context(patch.object(b, name, return_value=value))
            stack.enter_context(patch.object(b, 'WINDOWS', (Window('1h', 1, 60),)))
            stack.enter_context(patch.object(b, 'kept_window', side_effect=AssertionError('cache used')))
            stack.enter_context(patch.object(b, 'remember_windows'))
            stack.enter_context(patch('dashboard.tsg.minute_frame', return_value=None))
            stack.enter_context(patch('dashboard.livescrape.provisional_tail', return_value=pd.DataFrame()))
            for target in ('dashboard.casts.build_casts', 'dashboard.calendar.build_calendar', 'dashboard.history.publish'):
                stack.enter_context(patch(target, side_effect=AssertionError('unrelated publish called')))
            result = b.build(root, '', [], tracks_only=True)
            self.assertEqual(result['legs'], 1)
            manifest = json.loads((root / 'data/manifest.json').read_text())
            for key in ('legs', 'calendar', 'casts', 'history', 'variables'):
                self.assertEqual(manifest[key], original[key])
            self.assertEqual(manifest['sources']['calendar'], 'kept')
            self.assertEqual((root / 'index.html').read_text(), 'ship shell')
            for window in manifest['windows']:
                self.assertNotIn('fine_file', window)
                self.assertTrue((root / window['file']).is_file())
            self.assertFalse(list((root / 'data').glob('*-fine.json')))
            native = next(level for level in manifest['track']['levels'] if level['spacing_km'] == 0)
            points = {}
            for chunk in native['chunks']:
                payload = json.loads((root / chunk['file']).read_text())
                points.update(zip(payload['t'], payload['lat']))
            self.assertEqual(list(points.values()), [70., 70.00001, 70.00002])
            self.assertEqual(manifest['latest']['lat'], 70.00002)
            self.assertEqual(manifest['data_range']['end'], index[-1].isoformat())
