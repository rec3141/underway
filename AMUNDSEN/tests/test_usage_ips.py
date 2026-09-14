"""Distinct addresses across page changes, days, proxies and existing databases."""
import sqlite3
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import patch

from dashboard import usage


class UsageIPTests(unittest.TestCase):
    def test_proxy_and_address_normalization(self):
        self.assertEqual(usage.client_ip('127.0.0.1', '198.51.100.99, 192.0.2.1'), '192.0.2.1')
        self.assertEqual(usage.client_ip('192.0.2.2', '198.51.100.99'), '192.0.2.2')
        self.assertEqual(usage.client_ip('::ffff:192.0.2.1'), '192.0.2.1')
        self.assertEqual(usage.client_ip('::1', 'bad'), '::1')

    def test_deduplicates_pages_and_days_and_expires_old_addresses(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(usage, 'DB_DIR', Path(tmp)):
            usage.record('underway', '192.0.2.1')
            usage.record('casts', '::ffff:192.0.2.1')
            usage.record('wiki', '2001:db8::1')
            today = datetime.now(timezone.utc).date()
            with sqlite3.connect(Path(tmp) / 'usage.sqlite') as db:
                for age, address in ((1, '192.0.2.1'), (6, '192.0.2.2'), (7, '192.0.2.3'), (89, '192.0.2.4'), (90, '192.0.2.5')):
                    db.execute('INSERT INTO client_ips VALUES (?,?)', ((today-timedelta(days=age)).isoformat(), address))
            counts = usage.report_ips()
            self.assertEqual([counts[k] for k in ('today','week','total')], [2, 3, 5])
            self.assertIsNotNone(counts['since'])
            usage.record('wiki', '192.0.2.1')
            self.assertEqual(usage.report_ips(), counts)
            with sqlite3.connect(Path(tmp) / 'usage.sqlite') as db:
                self.assertEqual(db.execute("SELECT COUNT(*) FROM client_ips WHERE ip='192.0.2.5'").fetchone()[0], 0)
            self.assertEqual(sum(r['views'] for r in usage.report()), 4)

    def test_old_database_remains_readable_and_migrates_on_next_beacon(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(usage, 'DB_DIR', Path(tmp)):
            self.assertEqual(usage.report_ips(), dict(today=0, week=0, total=0, since=None))
            with sqlite3.connect(Path(tmp) / 'usage.sqlite') as db:
                db.execute('CREATE TABLE views (day TEXT, page TEXT, n INTEGER, PRIMARY KEY(day,page))')
                db.execute("INSERT INTO views VALUES (?,'wiki',5)", ((datetime.now(timezone.utc).date()-timedelta(days=1)).isoformat(),))
            self.assertIsNone(usage.report_ips()['since'])
            usage.record('wiki', '192.0.2.1')
            self.assertEqual(usage.report_ips()['today'], 1)
            self.assertGreaterEqual(sum(r['views'] for r in usage.report()), 6)
