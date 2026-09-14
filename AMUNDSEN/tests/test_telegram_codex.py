"""Codex routing and persistence, without contacting Telegram or a model."""
import json
import os
import sqlite3
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from dashboard import telegram_codex as c


class CodexTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        for p in (patch.object(c, 'DB_DIR', self.root),
                  patch.dict(os.environ, {'TELEGRAM_ID': '42', 'UNDERWAY_CODEX_TELEGRAM_ID': ''}),
                  patch.object(c, 'start')):
            p.start(); self.addCleanup(p.stop)

    def message(self, text, **extra):
        return {'chat': {'id': 42, 'type': 'private'}, 'from': {'id': 42}, 'text': text, **extra}

    def test_authorization(self):
        for msg in (self.message('/codex test hello', **{'from': {'id': 7}}),
                    self.message('/codex test hello', chat={'id': -42, 'type': 'group'})):
            self.assertIn('only', c.submit(msg, 1, None))
        self.assertFalse((self.root / 'codex_bot.sqlite').exists())

    def test_reply_and_resume_alias_and_deduplication(self):
        msg = self.message('Explain this', reply_to_message={'text': 'Explain the freezing point'})
        self.assertIsNone(c.submit(msg, 1, None))
        c.submit(msg, 1, None)
        c.submit(self.message('expand on that'), 2, None)
        with c.connect() as db:
            jobs = db.execute('SELECT * FROM jobs ORDER BY id').fetchall()
        self.assertEqual(len(jobs), 2)
        self.assertIn('Explain the freezing point', jobs[0]['prompt'])
        self.assertIn('Explain this', jobs[0]['prompt'])
        self.assertEqual(jobs[1]['name'], jobs[0]['name'])
        self.assertEqual(jobs[1]['name'], 'main')

    def test_validation_and_context(self):
        for text in ('', '/start', '/help'):
            self.assertEqual(c.submit(self.message(text), 1, None), c.USAGE)
        c.submit(self.message('explain this', reply_to_message={'caption': 'SST -1 C'}), 2, None)
        with c.connect() as db:
            prompt = db.execute('SELECT prompt FROM jobs').fetchone()[0]
        self.assertIn('SST -1 C', prompt)
        self.assertIn('explain this', prompt)

    def test_execution_persists_thread_and_resumes(self):
        fake = self.root / 'codex'
        fake.write_text('''#!/usr/bin/env python3
import json, sys
from pathlib import Path
Path('args.json').write_text(json.dumps(sys.argv[1:]))
Path('prompt.txt').write_text(sys.stdin.read())
print(json.dumps({'type':'thread.started','thread_id':'12345678-1234-1234-1234-123456789abc'}), flush=True)
print(json.dumps({'type':'item.completed','item':{'type':'agent_message','text':'Answer'}}), flush=True)
print(json.dumps({'type':'turn.completed'}), flush=True)
''')
        fake.chmod(0o700)
        job = {'chat': '42', 'name': 'heat', 'prompt': 'Hello'}
        with patch.dict(os.environ, {'UNDERWAY_CODEX_BIN': str(fake), 'UNDERWAY_CODEX_CWD': str(self.root)}):
            self.assertEqual(c.execute(job), 'Answer')
            self.assertNotIn('resume', json.loads((self.root / 'args.json').read_text()))
            self.assertEqual(c.execute(job), 'Answer')
        args = json.loads((self.root / 'args.json').read_text())
        self.assertIn('resume', args)
        self.assertIn('12345678-1234-1234-1234-123456789abc', args)
        self.assertEqual((self.root / 'prompt.txt').read_text(), 'Hello')
        self.assertIn('sandbox_workspace_write.network_access=true', args)
        self.assertEqual(args[args.index('-C') + 1], str(self.root))
        roots = json.loads(next(a.split('=', 1)[1] for a in args if a.startswith('sandbox_workspace_write.writable_roots=')))
        self.assertIn('/data/dev', roots)
        self.assertIn(str(Path(os.environ.get('UNDERWAY_CODEX_DOWNLOADS') or Path.home() / 'Downloads')), roots)
        self.assertIn(str(self.root / '.git'), roots)

    def test_migrate_latest_session_without_old_updates_or_jobs(self):
        with sqlite3.connect(self.root / 'telegram_codex.sqlite') as db:
            db.executescript('''CREATE TABLE sessions(chat TEXT, name TEXT, thread TEXT);
                CREATE TABLE jobs(id INTEGER, chat TEXT, name TEXT);
                INSERT INTO sessions VALUES ('42', 'old', 'thread-old'), ('42', 'latest', 'thread-latest');
                INSERT INTO jobs VALUES (100, '42', 'old'), (200, '42', 'latest');''')
        c.submit(self.message('continue'), 1, None)
        with c.connect() as db:
            self.assertEqual(tuple(db.execute('SELECT name, thread FROM sessions').fetchone()), ('main', 'thread-latest'))
            self.assertEqual(db.execute('SELECT count(*) FROM jobs').fetchone()[0], 1)

    def test_separate_persistent_offset_and_duplicate_delivery(self):
        class Telegram:
            sent = []
            def updates(inner, offset, wait):
                return [u for u in [{'update_id': 1, 'message': self.message('hello')}] if u['update_id'] >= offset]
            def send(inner, chat, text): inner.sent.append(text)
        tg = Telegram()
        self.assertEqual(c.handle(tg), 1)
        self.assertEqual(c.handle(tg), 0)
        self.assertEqual(tg.sent, [])
        with c.connect() as db:
            self.assertEqual(db.execute("SELECT value FROM metadata WHERE key='offset'").fetchone()[0], '2')
        self.assertFalse((self.root / 'alerts_state.json').exists())

    def test_dedicated_token(self):
        from dashboard import alerts
        with patch.dict(os.environ, {'TELEGRAM_KEY_CODEX': 'new-token'}), patch.object(alerts, 'Telegram') as tg:
            tg.return_value.updates.side_effect = KeyboardInterrupt
            with self.assertRaises(KeyboardInterrupt): c.bot_loop()
            tg.assert_called_once_with('new-token')

    def test_long_replies_split_and_persist(self):
        c.submit(self.message('/codex heat hello'), 1, None)
        with c.connect() as db:
            job = db.execute('SELECT * FROM jobs').fetchone()
            c.finish(db, job, '\U0001f30a' * 5000)
        with c.connect() as db:
            result = db.execute('SELECT * FROM jobs').fetchone()
        replies = json.loads(result['replies'])
        self.assertEqual(len(replies), 3)
        self.assertEqual(''.join(replies), '\U0001f30a' * 5000)
        self.assertTrue(all(len(s.encode('utf-16-le')) // 2 < 4096 for s in replies))


if __name__ == '__main__':
    unittest.main()
