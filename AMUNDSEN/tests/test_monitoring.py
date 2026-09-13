"""Usage aggregation and uptime state transitions without contacting recipients."""
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch

from dashboard import uptime, usage, status


class MonitoringTests(unittest.TestCase):
    def test_usage_counts_only_known_pages(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(usage, 'DB_DIR', Path(tmp)):
            usage.record('underway')
            usage.record('underway')
            usage.record('wiki')
            with self.assertRaises(ValueError):
                usage.record('someone/private-query')
            self.assertEqual({row['page']: row['views'] for row in usage.report()}, {'underway': 2, 'wiki': 1})

    def test_transient_failure_outage_and_recovery(self):
        state = {}
        bad = {'server': {'ok': False, 'error': 'timeout', 'ms': 5000}}
        good = {'server': {'ok': True, 'ms': 5}}
        uptime.update(state, bad, 'one')
        uptime.update(state, good, 'two')
        self.assertEqual(state['pending'], [])
        uptime.update(state, bad, 'three')
        uptime.update(state, bad, 'four')
        uptime.update(state, bad, 'five')
        self.assertEqual(len(state['pending']), 1)
        uptime.update(state, good, 'six')
        uptime.update(state, good, 'seven')
        self.assertEqual(len(state['pending']), 2)
        self.assertIn('recovered', state['pending'][-1]['text'])
        self.assertEqual([e['status'] for e in state['events']], ['down', 'recovered'])

    def test_status_marks_old_checks_stale_and_omits_notice_text(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(uptime, 'DB_DIR', Path(tmp)), patch.object(usage, 'DB_DIR', Path(tmp)):
            self.assertTrue(status.report()['stale'])
            state = {'checked_at': '2020-01-01T00:00:00+00:00', 'pending': [{'text': 'private delivery detail'}],
                     'checks': {'http://test': {'result': {'ok': True, 'ms': 12}}}}
            (Path(tmp) / 'uptime.json').write_text(json.dumps(state))
            result = status.report()
            self.assertTrue(result['stale'])
            self.assertEqual(result['pending_email'], 1)
            self.assertNotIn('private delivery detail', json.dumps(result))
            self.assertEqual(result['checks'][0]['ms'], 12)

    def test_email_retries_without_telegram(self):
        state = {'pending': [{'text': 'Underway unavailable: test', 'sent': []}]}
        email = Mock(side_effect=OSError('offline'))
        telegram = Mock()
        with patch('dashboard.alerts.ops_targets', return_value=('ops@example.test', {'host': 'test'}, 'chat')), \
             patch('dashboard.alerts.send_email', email), patch('dashboard.alerts.Telegram', return_value=telegram), \
             patch('dashboard.alerts.TELEGRAM_TOKEN', 'test'):
            uptime.deliver(state)
            self.assertEqual(state['pending'][0]['sent'], [])
            email.side_effect = None
            uptime.deliver(state)
        self.assertEqual(state['pending'], [])
        self.assertEqual(email.call_count, 2)
        telegram.send.assert_not_called()
