"""Sampling decisions use synthetic profiles and never send notifications."""

import json
from pathlib import Path
import tempfile
import unittest

import numpy as np

from dashboard.sampling import parse_samples, read_reference, resolve_samples, rank_candidates, novelty_distribution, bottle_template


def cast(leg="2026_LEG_03", number="001", kind="CTD"):
    return {
        "id": f"{leg}:CTD_{number}", "leg": leg, "cast": number,
        "kind": kind, "p": [10.0, 15.0, 20.0, 25.0, 30.0],
        "vars": {"Temperature": [1.0, 1.5, 2.0, 2.5, 3.0],
                 "Salinity": [30.0, 30.5, 31.0, 31.5, 32.0]},
        "units": {"Temperature": "°C", "Salinity": "PSU"},
        "bottles": [{"bottle": 1, "p": 20.0, "depth_m": 19.8}],
    }


class SampleInputTests(unittest.TestCase):
    def test_marked_bottle_template_counts_only_ones(self):
        profile = cast()
        profile['bottles'].append({'bottle': 2, 'p': 25., 'depth_m': 24.8})
        content = 'leg\tcast\tbottle\tsampled\n2026_LEG_03\t2026_LEG_03:CTD_001\t1\t\n2026_LEG_03\t2026_LEG_03:CTD_001\t2\t1\n'
        rows = parse_samples(content)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]['bottle'], '2')
        samples, unmatched = resolve_samples(rows, [profile])
        self.assertEqual(unmatched, [])
        self.assertEqual(len(samples), 1)
        self.assertEqual(samples[0]['pressure'], 25.)
        self.assertEqual(parse_samples(content.replace('\t1\n', '\t0\n')), [])
        with self.assertRaisesRegex(ValueError, 'sampled must be'):
            parse_samples(content.replace('\t1\n', '\tyes\n'))

    def test_cast_bottle_defaults_to_ctd(self):
        rows = parse_samples("cast\tbottle\n001\t1\n")
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0].get("source"), "ctd")
        samples, unmatched = resolve_samples(rows, [cast()])
        self.assertEqual(unmatched, [])
        self.assertEqual(len(samples), 1)
        self.assertEqual(samples[0]["pressure"], 20.0)
        self.assertEqual(samples[0]["Temperature"], 2.0)
        self.assertEqual(samples[0]["Salinity"], 31.0)

    def test_invalid_headers_and_rows_are_rejected(self):
        for content in (
            "station\tdepth\nA\t20\n",
            "cast\tbottle\n001\tnot-a-number\n",
            "source\tcast\tbottle\nunknown\t001\t1\n",
        ):
            with self.subTest(content=content), self.assertRaises(ValueError):
                parse_samples(content)

    def test_repeated_cast_numbers_require_a_leg(self):
        casts = [cast("2025_LEG_03"), cast("2026_LEG_03")]
        samples, unmatched = resolve_samples(parse_samples("cast\tbottle\n001\t1\n"), casts)
        self.assertEqual(samples, [])
        self.assertEqual(len(unmatched), 1)
        rows = parse_samples("leg\tcast\tbottle\n2026_LEG_03\t001\t1\n")
        samples, unmatched = resolve_samples(rows, casts)
        self.assertEqual(unmatched, [])
        self.assertEqual(samples[0]["leg"], "2026_LEG_03")

    def test_exact_id_disambiguates_repeated_cast_numbers(self):
        casts = [cast("2025_LEG_03"), cast("2026_LEG_03")]
        rows = parse_samples("cast\tbottle\n2026_LEG_03:CTD_001\t1\n")
        samples, unmatched = resolve_samples(rows, casts)
        self.assertEqual(unmatched, [])
        self.assertEqual(len(samples), 1)
        self.assertEqual(samples[0]["leg"], "2026_LEG_03")

    def test_missing_bottle_and_outside_profile_are_unmatched(self):
        rows = parse_samples("cast\tbottle\tpressure\n001\t24\t\n001\t\t500\n")
        samples, unmatched = resolve_samples(rows, [cast()])
        self.assertEqual(samples, [])
        self.assertEqual(len(unmatched), 2)

    def test_bottle_with_depth_without_pressure_is_resolved(self):
        profile = cast()
        profile["bottles"][0]["p"] = None
        samples, unmatched = resolve_samples(parse_samples("cast\tbottle\n001\t1\n"), [profile])
        self.assertEqual(unmatched, [])
        self.assertEqual(len(samples), 1)
        self.assertTrue(np.isfinite(samples[0]["pressure"]))
        self.assertLess(abs(samples[0]["Temperature"] - 2.0), 0.1)

    def test_flow_samples_use_measured_water_properties(self):
        rows = parse_samples("source\ttime\ttemperature\tsalinity\nflow\t2026-09-14T12:00:00Z\t2.5\t32.1\n")
        samples, unmatched = resolve_samples(rows, [])
        self.assertEqual(unmatched, [])
        self.assertEqual(len(samples), 1)
        self.assertEqual(samples[0]["Temperature"], 2.5)
        self.assertEqual(samples[0]["Salinity"], 32.1)


class ReferenceTests(unittest.TestCase):
    def test_selected_legs_and_ctd_tm_kinds_only(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            folder = root / "data" / "casts"
            folder.mkdir(parents=True)
            casts = [cast(), cast(number="002", kind="TM"),
                     cast(number="003", kind="MVP"), cast("2025_LEG_03")]
            index = []
            for i, profile in enumerate(casts):
                filename = f"data/casts/profile-{i}.json"
                (root / filename).write_text(json.dumps(profile), encoding="utf-8")
                index.append({**profile, "file": filename})
            (folder / "index.json").write_text(json.dumps({"casts": index}), encoding="utf-8")
            (root / "data" / "manifest.json").write_text(json.dumps({"casts": {"index": "data/casts/index.json"}}), encoding="utf-8")
            found = read_reference(root, ["2026_LEG_03"])
            self.assertEqual({x["id"] for x in found}, {casts[0]["id"], casts[1]["id"]})
            self.assertTrue(all("vars" in x and "p" in x for x in found))

    def test_bottle_template_lists_every_firing_in_selected_casts(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            folder = root / 'data' / 'casts'
            folder.mkdir(parents=True)
            selected = cast()
            selected['bottles'].append({'bottle': 2, 'p': 25.})
            other = cast('2025_LEG_03')
            index = []
            for i, profile in enumerate((selected, other)):
                filename = f'data/casts/profile-{i}.json'
                (root / filename).write_text(json.dumps(profile))
                index.append({**profile, 'file': filename})
            (folder / 'index.json').write_text(json.dumps({'casts': index}))
            content = bottle_template(root, ['2026_LEG_03'])
            self.assertEqual(content.splitlines(), ['leg\tcast\tbottle\tsampled',
                '2026_LEG_03\t2026_LEG_03:CTD_001\t1\t',
                '2026_LEG_03\t2026_LEG_03:CTD_001\t2\t'])


class PriorityTests(unittest.TestCase):
    def setUp(self):
        self.reference = np.array([[0., 0.], [.1, 0.], [0., .1], [.1, .1]])
        self.sampled = np.array([[0., 0.]])
        self.candidates = np.array([[.1, 0.], [1., 0.], [3., 0.]])
        self.pressure = np.array([10., 20., 30.])

    def rank(self, algorithm="coverage", count=1, spacing=0, **kwargs):
        return rank_candidates(self.reference, self.sampled, self.candidates,
                               self.pressure, algorithm, count, spacing, **kwargs)

    def test_coverage_prioritizes_unsampled_water(self):
        ranked = self.rank()
        self.assertEqual(len(ranked), 1)
        self.assertEqual(ranked[0]["index"], 2)

    def test_rarity_prioritizes_distance_from_reference(self):
        ranked = self.rank("rarity")
        self.assertEqual(len(ranked), 1)
        self.assertEqual(ranked[0]["index"], 2)

    def test_target_spacing_is_preferred_then_relaxed_to_fill_budget(self):
        ranked = self.rank(count=3, spacing=15)
        chosen = [self.pressure[item["index"]] for item in ranked]
        self.assertEqual(len(chosen), 3)
        self.assertGreaterEqual(abs(chosen[0] - chosen[1]), 15)
        self.assertTrue(ranked[-1]['spacing_relaxed'])
        self.assertTrue(all(np.isfinite(item["score"]) and item["reason"] for item in ranked))

    def test_unavailable_surprise_fills_budget_using_coverage(self):
        self.assertEqual(len(self.rank("surprise", count=3)), 3)
        self.assertTrue(all(r['fallback'] for r in self.rank("surprise", count=3, surprise=np.full(3, np.nan))))
        ranked = self.rank("surprise", surprise=np.array([9., 2., 1.]))
        self.assertEqual(ranked[0]["index"], 0)

    def test_novelty_z_uses_leave_one_out_archived_bottle_distances(self):
        bottles = np.array([[0., 0.], [1., 0.], [3., 0.]])
        distribution = novelty_distribution(self.reference, bottles, np.array([[2., 0.]]))
        self.assertEqual(distribution['bottle_count'], 3)
        self.assertAlmostEqual(distribution['mean'], 4 / 3)
        self.assertAlmostEqual(distribution['values'][0], -1 / np.sqrt(2), places=5)
        self.assertEqual(len(distribution['baseline_values']), 3)
        self.assertEqual(distribution['warning'], '')

    def test_novelty_without_bottles_has_explicit_reference_fallback(self):
        result = novelty_distribution(self.reference, np.empty((0, 2)), self.candidates)
        self.assertEqual(len(result['values']), 3)
        self.assertTrue(result['warning'])


if __name__ == "__main__":
    unittest.main()
