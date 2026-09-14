"""Codex routing and persistence, without contacting Telegram or a model."""
import json
import os
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
        self.assertFalse((self.root / 'telegram_codex.sqlite').exists())

    def test_reply_and_resume_alias_and_deduplication(self):
        msg = self.message('/codex heat', reply_to_message={'text': 'Explain the freezing point'})
        self.assertIn('queued', c.submit(msg, 1, None))
        c.submit(msg, 1, None)
        c.submit(self.message('/codex heat expand on that'), 2, None)
        with c.connect() as db:
            jobs = db.execute('SELECT * FROM jobs ORDER BY id').fetchall()
        self.assertEqual(len(jobs), 2)
        self.assertEqual(jobs[0]['prompt'], 'Explain the freezing point')
        self.assertEqual(jobs[1]['name'], jobs[0]['name'])

    def test_validation_and_context(self):
        for text in ('/codex', '/codex ../bad hello', '/codex heat'):
            self.assertEqual(c.submit(self.message(text), 1, None), c.USAGE)
        c.submit(self.message('/codex heat explain this', reply_to_message={'caption': 'SST -1 C'}), 2, None)
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

    def test_long_replies_split_and_persist(self):
        c.submit(self.message('/codex heat hello'), 1, None)
        with c.connect() as db:
            job = db.execute('SELECT * FROM jobs').fetchone()
            c.finish(db, job, '\U0001f30a' * 5000)
        with c.connect() as db:
            result = db.execute('SELECT * FROM jobs').fetchone()
        replies = json.loads(result['replies'])
        self.assertEqual(len(replies), 3)
        self.assertTrue(all(len(s.encode('utf-16-le')) // 2 < 4096 for s in replies))


if __name__ == '__main__':
    unittest.main()
