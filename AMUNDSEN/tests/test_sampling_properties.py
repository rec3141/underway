"""Archive colour channels and bottle overlays preserve missingness and inventory."""
import unittest
import numpy as np
from dashboard import sampling as S


def cast():
    return {'id': 'L:CTD_001', 'leg': 'L', 'cast': '001', 'p': [0., 5., 10.],
            'vars': {'Temperature': [1., 2., 3.], 'Salinity': [30., 31., 32.],
                     'Nitrates': [None, 4., 6.], 'Oxygen': [8., 7., 6.]},
            'units': {'Temperature': '°C', 'Salinity': 'PSU', 'Nitrates': 'µM', 'Oxygen': 'mL/L'},
            'bottles': [{'bottle': 1, 'p': 5.}, {'bottle': 2, 'p': 5.}, {'bottle': 3, 'p': 50.}]}


class PropertyTests(unittest.TestCase):
    def test_all_channels_are_available_without_filling_colour_gaps(self):
        points = S.reference_points([cast()])
        self.assertIn('Nitrates [µM]', points[0]['properties'])
        self.assertIsNone(points[0]['properties']['Nitrates [µM]'])
        self.assertEqual(points[1]['properties']['Nitrates [µM]'], 4.)
        self.assertAlmostEqual(points[0]['properties']['Oxygen [µM]'], 8. * 44.6596)
        self.assertEqual({v['name'] for v in S.variable_catalog([cast()])}, {'Temperature', 'Salinity', 'Nitrates', 'Oxygen'})

    def test_bottle_overlay_does_not_mark_inventory_as_sampled(self):
        levels = S.collected_points([cast()])
        self.assertEqual(len(levels), 1)
        self.assertTrue(levels[0]['collected'])
        self.assertFalse(levels[0]['sampled'])
        self.assertEqual(levels[0]['bottles'], [1, 2])
        self.assertEqual(levels[0]['properties']['Nitrates [µM]'], 4.)
        self.assertEqual(S.resolve_samples([], [cast()]), ([], []))

    def test_unavailable_variable_is_not_interpolated_across_missing_endpoint(self):
        row = S.parse_samples('cast\tpressure\nL:CTD_001\t2\n')
        samples, errors = S.resolve_samples(row, [cast()])
        self.assertFalse(errors)
        self.assertNotIn('Nitrates [µM]', samples[0]['properties'])
        self.assertAlmostEqual(samples[0]['Temperature'], 1.4)

    def test_rarity_prefers_new_water_then_fills_budget_with_remaining_levels(self):
        result = S.rank_candidates(np.array([[0., 0.], [.1, .1], [.2, .2]]),
                                   np.array([[5., 5.]]), np.array([[5., 5.], [3., 3.]]),
                                   np.array([10., 30.]), 'rarity', 2, 10)
        self.assertEqual([r['index'] for r in result], [1, 0])
