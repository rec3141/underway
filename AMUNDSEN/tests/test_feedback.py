"""Feedback persists outside the web root and retries do not duplicate it."""
import functools
import http.client
import json
from pathlib import Path
import tempfile
import threading
import unittest
from unittest.mock import patch
from uuid import uuid4

from dashboard import feedback
from dashboard.serve import Handler, ThreadingHTTPServer


class FeedbackTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.patch = patch.object(feedback, 'DB_DIR', self.base / 'db')
        self.patch.start()
        self.addCleanup(self.patch.stop)
        self.web = self.base / 'web'
        self.web.mkdir()
        self.server = ThreadingHTTPServer(('127.0.0.1', 0), functools.partial(Handler, directory=str(self.web)))
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.addCleanup(self.stop)

    def stop(self):
        self.server.shutdown()
        self.server.server_close()

    def post(self, payload):
        conn = http.client.HTTPConnection(*self.server.server_address, timeout=5)
        conn.request('POST', '/api/feedback', json.dumps(payload), {'Content-Type': 'application/json'})
        response = conn.getresponse()
        result = response.status, json.loads(response.read())
        conn.close()
        return result

    def test_submission_and_retry_preserve_page_context(self):
        payload = dict(id=str(uuid4()), message='Map points disappear', name='Tester',
                       context={'tab': 'wiki', 'url': 'http://ship/#wiki/observation/one', 'window': '1h'})
        for _ in range(2):
            code, result = self.post(payload)
            self.assertEqual(code, 200)
            self.assertEqual(result['id'], payload['id'])
        con = feedback.connect()
        try:
            rows = con.execute('SELECT * FROM feedback').fetchall()
        finally:
            con.close()
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]['message'], payload['message'])
        self.assertEqual(json.loads(rows[0]['context']), payload['context'])
        self.assertFalse((self.web / 'feedback.sqlite').exists())

    def test_reject_invalid_or_oversized_feedback(self):
        for payload in [[], {}, {'id':str(uuid4()), 'message':' '},
                        {'id':str(uuid4()), 'message':'x'*5001},
                        {'id':str(uuid4()), 'message':'ok', 'context':[]},
                        {'id':str(uuid4()), 'message':'x'*33000}]:
            with self.subTest(payload=str(payload)[:60]):
                self.assertEqual(self.post(payload)[0], 400)
