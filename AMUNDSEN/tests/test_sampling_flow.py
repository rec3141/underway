"""Historical scoring uses synthetic windows without files or notifications."""

from datetime import datetime
import unittest
from unittest.mock import patch

import numpy as np

from dashboard.config import SURPRISE_NAME
from dashboard.sampling_flow import score_window


class HistoricalFlowTests(unittest.TestCase):
    def setUp(self):
        self.manifest = {'legs': [{'id': '2025_LEG_03'}, {'id': '2026_LEG_03'}]}
        self.cfg = {'legs': ['2026_LEG_03'], 'algorithm': 'coverage', 'count': 3, 'spacing': 15}
        self.model = {'ref': np.array([[0., 0.], [.1, .1], [.2, .2], [.3, .3], [.4, .4]]),
                      'sampled': np.array([[0., 0.]]), 'med': np.array([0., 30.]),
                      'scale': np.ones(2)}
        clock = patch('dashboard.sampling_flow.time.time', return_value=10000)
        clock.start()
        self.addCleanup(clock.stop)

    def window(self, times=(1000, 1900, 2800), temperatures=None):
        n = len(times)
        return {'t': [t * 1000 for t in times], 'leg': [1] * n, 'pump_low': [False] * n,
                'vars': {'SST (°C)': temperatures or [3.] * n, 'Salinity (PSU)': [33.] * n,
                         'TSG flow (V)': [1.] * n, SURPRISE_NAME: [5.] * n}}

    def score(self, data, algorithm='coverage'):
        return score_window(data, self.manifest, self.model, {**self.cfg, 'algorithm': algorithm})['targets']

    def test_old_observations_are_hypothetical_not_stale(self):
        results = self.score(self.window())
        self.assertEqual(len(results), 3)
        self.assertTrue(all(r['historical'] and 'Not sent' in r['reason'] for r in results))
        self.assertEqual(results[0]['algorithm'], 'coverage')
        self.assertEqual(datetime.fromisoformat(results[0]['time']).timestamp(), 1000)

    def test_interval_is_chronological_and_at_least_fifteen_minutes(self):
        results = self.score(self.window((2800, 1000, 1050, 1899, 1900)))
        self.assertEqual([datetime.fromisoformat(r['time']).timestamp() for r in results], [1000, 1900, 2800])

    def test_leg_pump_missing_properties_and_future_values_are_filtered(self):
        for change in (
            lambda d: d.update(leg=[0] * 3),
            lambda d: d.update(pump_low=[True] * 3),
            lambda d: d['vars'].update({'TSG flow (V)': [0.1] * 3}),
            lambda d: d['vars'].update({'TSG flow (V)': [None] * 3}),
            lambda d: d['vars'].update({'SST (°C)': [None] * 3}),
            lambda d: d.update(t=[11000 * 1000] * 3),
            lambda d: d.update(t=[float('nan')] * 3),
        ):
            data = self.window()
            change(data)
            with self.subTest(data=data):
                self.assertEqual(self.score(data), [])

    def test_identical_sampled_water_still_fills_explicit_display_budget(self):
        self.model['sampled'] = np.array([[3., 3.]])
        for algorithm in ('coverage', 'rarity', 'gradient', 'surprise', 'hybrid'):
            with self.subTest(algorithm=algorithm):
                self.assertEqual(len(self.score(self.window(), algorithm)), 3)

    def test_surprise_prefers_available_scores_then_falls_back_to_fill(self):
        data = self.window()
        data['vars'][SURPRISE_NAME] = [2., 4.]
        results = self.score(data, 'surprise')
        self.assertEqual(len(results), 3)
        self.assertEqual(results[0]['score'], 4.)
        self.assertEqual(datetime.fromisoformat(results[0]['time']).timestamp(), 1900)
        self.assertTrue(results[-1]['fallback'])

    def test_gradient_uses_observed_minute_change(self):
        data = self.window((1000, 1030, 1060), [0.1, 0.2, 3.])
        results = self.score(data, 'gradient')
        self.assertEqual(len(results), 3)
        self.assertEqual(datetime.fromisoformat(results[0]['time']).timestamp(), 1060)

    def test_gradient_does_not_bridge_time_pump_or_leg_gaps(self):
        data = self.window((1000, 1900), [0.1, 3.])
        self.assertTrue(all(r['score'] == 0 for r in self.score(data, 'gradient')))
        data = self.window((1000, 1030, 1060), [0.1, 0.2, 3.])
        data['pump_low'][1] = True
        self.assertTrue(all(r['score'] == 0 for r in self.score(data, 'gradient')))
        data = self.window((1000, 1060), [0.1, 3.])
        data['leg'][0] = 0
        self.cfg['legs'].append('2025_LEG_03')
        self.assertTrue(all(r['score'] == 0 for r in self.score(data, 'gradient')))

    def test_coverage_rarity_and_hybrid_find_unusual_water(self):
        for algorithm in ('coverage', 'rarity', 'hybrid'):
            with self.subTest(algorithm=algorithm):
                results = self.score(self.window(), algorithm)
                self.assertEqual(len(results), 3)
                self.assertTrue(all(r['score'] >= r['threshold'] for r in results))

    def test_missing_flow_record_and_empty_reference_return_no_picks(self):
        data = self.window()
        del data['vars']['TSG flow (V)']
        self.assertEqual(self.score(data), [])
        self.model['ref'] = np.empty((0, 2))
        self.assertEqual(self.score(self.window()), [])

    def test_budget_fill_and_distribution_cover_all_healthy_window_candidates(self):
        self.cfg.update(count=4, spacing=1440)
        data = self.window((1000, 1060, 1120, 1180, 1240), [1., 2., 3., 4., 5.])
        self.model['bottle_sampled'] = np.array([[0., 0.], [1., 1.], [3., 3.]])
        result = score_window(data, self.manifest, self.model, self.cfg)
        self.assertEqual(len(result['targets']), 4)
        self.assertEqual(result['requested_count'], 4)
        self.assertEqual(result['available_count'], 5)
        self.assertEqual(len(result['distribution']['values']), 5)
        self.assertEqual(result['distribution']['bottle_count'], 3)
        self.assertEqual([p['rank'] for p in result['targets']], [1, 2, 3, 4])
        self.assertTrue(any(p['spacing_relaxed'] for p in result['targets']))
        self.assertTrue(result['warnings'])


if __name__ == '__main__':
    unittest.main()
