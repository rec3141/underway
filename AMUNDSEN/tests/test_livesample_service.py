"""Sampling service checks isolate files, live scans and Telegram delivery."""

import copy
import json
import os
from pathlib import Path
import tempfile
import threading
import unittest
from unittest.mock import patch

import numpy as np

from dashboard import livesample as L


def live(depth=False):
    key = "depth_m" if depth else "pressure"
    return {"tcp_state": "connected", "last_packet_age_s": 1,
            "current": {"started": 1000, "direction": "down", "depth_like": depth,
                        "pressure_col": key, "t": [1000, 1010, 1020, 1030, 1040],
                        "cols": {key: [10., 20., 30., 25., 15.],
                                 "temperature": [1., 2., 3., 9., 9.],
                                 "salinity": [30., 31., 32., 39., 39.]}}}


def model():
    return {"ref": np.array([[0., 0.], [.1, .1], [.2, .2], [.3, .3], [.4, .4]]),
            "sampled": np.array([[0., 0.]]), "med": np.array([0., 30.]),
            "scale": np.array([1., 1.]), "samples": [], "points": [{}] * 5,
            "features": ["Temperature", "Salinity"], "casts": 1,
            "unmatched": [], "embeddings": {}}


class LiveCandidateTests(unittest.TestCase):
    def setUp(self):
        clock = patch.object(L.time, "time", return_value=1041)
        clock.start()
        self.addCleanup(clock.stop)

    def test_only_observed_downcast_levels_are_ranked(self):
        points, error = L.live_candidates(live())
        self.assertEqual(error, "")
        self.assertEqual([x["pressure"] for x in points], [10., 20., 30.])
        self.assertEqual([x["Temperature"] for x in points], [1., 2., 3.])
        self.assertEqual([x["time"] for x in points], [1000., 1010., 1020.])

    def test_depth_feed_keeps_depth_and_converts_pressure(self):
        points, error = L.live_candidates(live(depth=True))
        self.assertEqual(error, "")
        for point, depth in zip(points, [10., 20., 30.]):
            self.assertEqual(point["position"], depth)
            self.assertEqual(point["unit"], "m")
            self.assertAlmostEqual(point["depth_m"], depth, places=6)
            self.assertGreater(point["pressure"], depth)

    def test_stale_ended_and_missing_casts_produce_no_targets(self):
        for age in (None, -1, 31, 1000):
            state = live()
            state["last_packet_age_s"] = age
            with self.subTest(age=age):
                self.assertEqual(L.live_candidates(state)[0], [])
        state = live()
        state["current"]["ended"] = 1100
        self.assertEqual(L.live_candidates(state)[0], [])
        self.assertEqual(L.live_candidates({"last": live()["current"]})[0], [])

    def test_invalid_stream_packets_do_not_keep_old_scans_fresh(self):
        state = live()
        state["current"]["t"] = [900, 910, 920, 1030, 1040]
        state["current"]["cols"]["temperature"][-2:] = [None, None]
        points, reason = L.live_candidates(state)
        self.assertEqual(points, [])
        self.assertIn("fresh valid", reason)
        state = live()
        state["current"]["t"][-1] = 1100
        self.assertEqual(L.live_candidates(state)[0], [])

    def test_soak_cycle_is_excluded_from_real_downcast_candidates(self):
        state = live()
        pressure = [3., 10., 20., 20., 10., 4., 3., 3., 5., 10., 20., 30., 25.]
        state["current"]["t"] = list(range(1029, 1042))
        state["current"]["cols"]["pressure"] = pressure
        state["current"]["cols"]["temperature"] = list(range(len(pressure)))
        state["current"]["cols"]["salinity"] = [30. + i / 10 for i in range(len(pressure))]
        points, error = L.live_candidates(state)
        self.assertEqual(error, "")
        self.assertEqual([p["pressure"] for p in points], [5., 10., 20., 30.])
        self.assertTrue(all(p["Temperature"] >= 8 for p in points))
        segment = L.cast_segment(state["current"])
        self.assertTrue(segment["soak_detected"])
        self.assertEqual(segment["start_index"], 8)

    def test_soak_return_waits_for_confirmed_real_descent(self):
        state = live()
        pressure = [3., 10., 20., 20., 10., 4., 3.]
        state["current"]["t"] = list(range(1035, 1042))
        state["current"]["cols"]["pressure"] = pressure
        state["current"]["cols"]["temperature"] = [1.] * len(pressure)
        state["current"]["cols"]["salinity"] = [30.] * len(pressure)
        points, error = L.live_candidates(state)
        self.assertEqual(points, [])
        self.assertIn("Soak cycle excluded", error)
        self.assertIsNone(L.cast_segment(state["current"])["start_time"])

    def test_initial_soak_descent_never_becomes_a_sampling_candidate(self):
        state = live()
        pressure = [3., 10., 18., 20., 20.]
        state["current"]["t"] = list(range(1037, 1042))
        state["current"]["cols"]["pressure"] = pressure
        state["current"]["cols"]["temperature"] = [1.] * len(pressure)
        state["current"]["cols"]["salinity"] = [30.] * len(pressure)
        points, error = L.live_candidates(state)
        self.assertEqual(points, [])
        self.assertIn("waiting for the surface return", error)
        self.assertTrue(L.cast_segment(state["current"])["soak_pending"])


class ServiceTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name) / "www"
        (self.root / "data").mkdir(parents=True)
        self.state_dir = Path(tmp.name) / "state"
        self.write("manifest.json", {"legs": [{"id": "2026_LEG_03"}], "live": "2026_LEG_03"})
        self.live = live()
        self.service = L.SampleService(self.root, self.state_dir, lambda: copy.deepcopy(self.live), start=False)
        clock = patch.object(L.time, "time", return_value=1041)
        clock.start()
        self.addCleanup(clock.stop)

    def write(self, filename, value):
        (self.root / "data" / filename).write_text(json.dumps(value), encoding="utf-8")

    def flow(self, now=2000, stamp=1999, flow_v=1., pump=False):
        self.write("w-1h.json", {"t": [(stamp - 60) * 1000, stamp * 1000],
                               "vars": {"SST (°C)": [0.1, 3.], "Salinity (PSU)": [30.1, 33.],
                                        "TSG flow (V)": [1., flow_v]}, "pump_low": [False, pump]})
        return self.service._flow(model(), self.service.config, now)

    def test_flow_fresh_water_can_trigger(self):
        self.assertTrue(self.flow()["recommendation"])

    def test_flow_stale_future_low_unknown_and_flagged_pump_are_suppressed(self):
        for kwargs in ({"stamp": 1000}, {"stamp": 2100}, {"flow_v": 0.1}, {"pump": True}):
            with self.subTest(kwargs=kwargs):
                self.assertFalse(self.flow(**kwargs)["recommendation"])

    def test_flow_uses_recent_complete_bin_while_latest_flow_is_pending(self):
        result = self.flow(flow_v=None)
        self.assertNotIn("unknown", result["status"].lower())
        self.assertEqual(result["time"], "1970-01-01T00:32:19+00:00")

    def test_flow_requires_selected_live_leg(self):
        self.service.config["legs"] = ["2025_LEG_03"]
        self.assertFalse(self.flow()["recommendation"])

    def test_short_surprise_array_is_unavailable_without_blocking_other_scores(self):
        self.flow()
        path = self.root / "data" / "w-1h.json"
        data = json.loads(path.read_text())
        data["vars"][L.SURPRISE_NAME] = [8.]
        self.write("w-1h.json", data)
        result = self.service._flow(model(), self.service.config, 2000)
        self.assertIsNone(result["surprise"])
        self.assertTrue(result["recommendation"])
        cfg = {**self.service.config, "algorithm": "surprise"}
        self.assertFalse(self.service._flow(model(), cfg, 2000)["recommendation"])

    def test_settings_malformed_types_are_value_errors_and_do_not_mutate(self):
        original = copy.deepcopy(self.service.config)
        cases = [None, [], "bad", {"unknown": 1}]
        cases += [{"legs": x} for x in (None, {}, "2026_LEG_03", [], [[]])]
        cases += [{"algorithm": x} for x in (None, {}, [], 1, "bad")]
        cases += [{"count": x} for x in (None, {}, [], "2", True, 0, 25)]
        cases += [{"min_spacing": x} for x in (None, {}, [], True, "bad", 0, float("inf"), 10 ** 400)]
        cases += [{"telegram": x} for x in (None, {}, [], 1, "yes")]
        cases += [{"tsv": x} for x in (None, {}, [])]
        for payload in cases:
            with self.subTest(payload=payload), self.assertRaises(ValueError):
                self.service.update(payload)
            self.assertEqual(self.service.config, original)
        self.assertFalse((self.state_dir / "settings.json").exists())

    def test_settings_and_inventory_survive_restart(self):
        self.service.update({"algorithm": "rarity", "count": 3, "min_spacing": 20,
                             "tsv": "cast\tbottle\n001\t1\n"})
        restarted = L.SampleService(self.root, self.state_dir, start=False)
        self.assertEqual(restarted.config, self.service.config)
        self.assertEqual(restarted.tsv, self.service.tsv)

    def test_upcast_keeps_entire_downcast_plan_including_passed_targets(self):
        self.live["current"]["cols"]["pressure"] = [10., 20., 30., 35., 40.]
        with patch.object(self.service, "_model", return_value=model()), \
             patch.object(self.service, "_flow", return_value={"recommendation": False}), \
             patch.object(L.S, "rank_candidates", return_value=[{"index": 0, "score": 2., "reason": "coverage"},
                                                               {"index": 2, "score": 3., "reason": "coverage"}]) as rank:
            self.service.compute()
            down = copy.deepcopy(self.service.status()["recommendations"])
            self.assertEqual([r["position"] for r in down], [10., 30.])
            self.live["current"]["direction"] = "up"
            self.live["current"]["cols"]["pressure"][-1] = 25.
            rank.return_value = [{"index": 1, "score": 100., "reason": "changed model"}]
            self.service.compute()
            self.assertEqual(self.service.status()["recommendations"], down)
            self.live["last_packet_age_s"] = 31
            self.service.compute()
            stale = self.service.status()
            self.assertEqual(stale["recommendations"], down)
            self.assertEqual(stale["status"], "Stale cast targets for display only; alerts paused")

    def test_telegram_deduplication_survives_restart(self):
        self.service.model = model()
        recs = [{"pressure": 20., "depth_m": 19.8}]
        cfg = {**self.service.config, "telegram": True}
        with patch("dashboard.alerts.telegram_token", return_value="test-token"), \
             patch("dashboard.alerts.ops_targets", return_value=(None, None, "test-chat")), \
             patch("dashboard.alerts.Telegram.send") as send, patch.object(L.time, "time", return_value=2000):
            self.service._notify(cfg, self.live, recs, {"recommendation": False})
            self.service._notify(cfg, self.live, recs, {"recommendation": False})
            self.assertEqual(send.call_count, 1)
            restarted = L.SampleService(self.root, self.state_dir, start=False)
            restarted.model = model()
            restarted._notify(cfg, self.live, recs, {"recommendation": False})
            self.assertEqual(send.call_count, 1)
            self.assertEqual(restarted.notices, self.service.notices)

    def test_completed_cast_shows_full_budget_and_distribution_without_alerts(self):
        finished = live()['current']
        finished['ended'] = 1040
        self.live = {'last': finished, 'tcp_state': 'off'}
        self.service.config.update(count=3, min_spacing=500, algorithm='surprise')
        with patch.object(self.service, '_model', return_value=model()), \
             patch.object(self.service, '_flow', return_value={'recommendation': False}), \
             patch.object(self.service, '_notify') as notify:
            self.service.compute()
            result = self.service.status()
        self.assertEqual(result['status'], 'Last completed cast targets')
        self.assertEqual(len(result['recommendations']), 3)
        self.assertEqual(result['requested_count'], 3)
        self.assertEqual(result['available_count'], 3)
        self.assertEqual(len(result['distributions']['ctd']['values']), 3)
        self.assertTrue(all('rank' in p and 'z' in p for p in result['recommendations']))
        self.assertTrue(any(p['spacing_relaxed'] for p in result['recommendations']))
        notify.assert_not_called()

    def test_too_few_cast_bins_reports_available_count(self):
        with patch.object(self.service, '_model', return_value=model()), \
             patch.object(self.service, '_flow', return_value={'recommendation': False}):
            self.service.compute()
        result = self.service.status()
        self.assertEqual(len(result['recommendations']), 3)
        self.assertEqual(result['requested_count'], 6)
        self.assertEqual(result['available_count'], 3)
        self.assertTrue(any('Only 3' in warning for warning in result['warnings']))

    def test_failed_telegram_send_is_retried(self):
        self.service.model = model()
        cfg = {**self.service.config, "telegram": True}
        with patch("dashboard.alerts.telegram_token", return_value="test-token"), \
             patch("dashboard.alerts.ops_targets", return_value=(None, None, "test-chat")), \
             patch("dashboard.alerts.Telegram.send", side_effect=[OSError("offline"), None]) as send, \
             patch.object(L.time, "time", return_value=2000):
            self.service._notify(cfg, self.live, [{"pressure": 20., "depth_m": 19.8}], {"recommendation": False})
            self.assertEqual(self.service.notices, {})
            self.service._notify(cfg, self.live, [{"pressure": 20., "depth_m": 19.8}], {"recommendation": False})
            self.assertEqual(send.call_count, 2)
            self.assertIn("ctd_key", self.service.notices)

    def test_slow_telegram_does_not_block_status_or_send_old_settings(self):
        entered, release, observed = threading.Event(), threading.Event(), threading.Event()
        def delayed_send(*args):
            entered.set()
            release.wait(2)
        cfg = {**self.service.config, "telegram": True}
        flow = {"recommendation": True, "time": "2026-09-14T12:00:00Z",
                "temperature": 2., "salinity": 32., "reason": "coverage"}
        with patch("dashboard.alerts.telegram_token", return_value="test-token"), \
             patch("dashboard.alerts.ops_targets", return_value=(None, None, "test-chat")), \
             patch("dashboard.alerts.Telegram.send", side_effect=delayed_send) as send, \
             patch.object(L.time, "time", return_value=2000):
            worker = threading.Thread(target=self.service._notify,
                                      args=(cfg, self.live, [{"pressure": 20., "depth_m": 19.8}], flow),
                                      kwargs={"expected_revision": self.service.revision})
            def observe():
                self.service.status()
                self.service.update({"algorithm": "rarity"})
                observed.set()
            observer = threading.Thread(target=observe)
            worker.start()
            try:
                self.assertTrue(entered.wait(1))
                observer.start()
                self.assertTrue(observed.wait(.5), "Telegram send held the settings/status lock")
            finally:
                release.set()
                worker.join(2)
                if observer.ident is not None:
                    observer.join(2)
            self.assertEqual(send.call_count, 1, "A settings update must suppress pending messages")

    def test_settings_change_during_flow_does_not_publish_old_results(self):
        def changed_settings(*args):
            self.service.update({"algorithm": "rarity"})
            return {"recommendation": False}
        with patch.object(self.service, "_model", return_value=model()), \
             patch.object(self.service, "_flow", side_effect=changed_settings), \
             patch.object(self.service, "_notify") as notify:
            self.service.compute()
            self.assertEqual(self.service.status()["status"], "Updating sampling model")
            self.assertEqual(self.service.status()["recommendations"], [])
            notify.assert_not_called()

    def test_unchanged_profile_rewrite_reuses_embeddings(self):
        folder = self.root / "data" / "casts"
        folder.mkdir()
        profile = {"id": "2026_LEG_03:CTD_001", "leg": "2026_LEG_03", "cast": "001", "kind": "CTD",
                   "p": [10., 15., 20., 25., 30.], "bottles": [],
                   "vars": {"Temperature": [1., 1.5, 2., 2.5, 3.], "Salinity": [30., 30.5, 31., 31.5, 32.]}}
        file = folder / "profile.json"
        file.write_text(json.dumps(profile), encoding="utf-8")
        (folder / "index.json").write_text(json.dumps({"casts": [{**profile, "file": "data/casts/profile.json"}]}))
        with patch.object(L.S, "embed", return_value=({}, ["Temperature", "Salinity"])) as embed:
            first = self.service._model(self.service.config, "")
            st = file.stat()
            file.write_text(json.dumps(profile), encoding="utf-8")
            os.utime(file, ns=(st.st_atime_ns, st.st_mtime_ns + 1_000_000))
            second = self.service._model(self.service.config, "")
            self.assertIs(first, second)
            self.assertEqual(embed.call_count, 1)
            profile["vars"]["Temperature"][0] = 1.1
            file.write_text(json.dumps(profile), encoding="utf-8")
            self.assertIsNot(first, self.service._model(self.service.config, ""))
            self.assertEqual(embed.call_count, 2)


if __name__ == "__main__":
    unittest.main()
