"""HTTP compression and caching contracts for progressively loaded JSON."""
import functools
import gzip
import http.client
import json
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

from dashboard.serve import Handler, ThreadingHTTPServer


class JsonCompressionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory()
        cls.root = Path(cls.tmp.name)
        (cls.root / 'data/track').mkdir(parents=True)
        cls.payload = {'t': list(range(1000)), 'vars': {'SST': [1.23] * 1000}}
        cls.raw = json.dumps(cls.payload).encode()
        (cls.root / 'data/window.json').write_bytes(cls.raw)
        cls.chunk = '/data/track/' + 'a' * 32 + '.json'
        (cls.root / cls.chunk.lstrip('/')).write_bytes(cls.raw)
        with (cls.root / 'data/large.json').open('wb') as f:
            f.truncate(8 * 1024 * 1024 + 1)
        cls.server = ThreadingHTTPServer(('127.0.0.1', 0), functools.partial(Handler, directory=str(cls.root)))
        cls.thread = threading.Thread(target=cls.server.serve_forever, daemon=True)
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()
        cls.thread.join(timeout=3)
        cls.tmp.cleanup()

    def request(self, path, encoding='gzip', method='GET', **headers):
        conn = http.client.HTTPConnection('127.0.0.1', self.server.server_port, timeout=3)
        try:
            conn.request(method, path, headers={'Accept-Encoding': encoding, **headers})
            response = conn.getresponse()
            return response.status, dict(response.getheaders()), response.read()
        finally:
            conn.close()

    def test_static_json_and_head(self):
        status, headers, body = self.request('/data/window.json')
        self.assertEqual(status, 200)
        self.assertEqual(gzip.decompress(body), self.raw)
        self.assertEqual(int(headers['Content-Length']), len(body))
        self.assertEqual(headers['Vary'], 'Accept-Encoding')
        self.assertEqual(headers['Cache-Control'], 'no-store')
        _, head, empty = self.request('/data/window.json', method='HEAD')
        self.assertEqual(empty, b'')
        for name in ('Content-Encoding', 'Content-Length', 'Vary'):
            self.assertEqual(head[name], headers[name])

    def test_negotiation(self):
        for encoding in ('identity', 'gzip;q=0', '*;q=1, gzip;q=0', 'gzip;q=invalid'):
            with self.subTest(encoding=encoding):
                _, headers, body = self.request('/data/window.json', encoding)
                self.assertNotIn('Content-Encoding', headers)
                self.assertEqual(body, self.raw)
        for encoding in ('br, GZIP; q=0.5', '*;q=1'):
            _, headers, body = self.request('/data/window.json', encoding)
            self.assertEqual(headers['Content-Encoding'], 'gzip')
            self.assertEqual(gzip.decompress(body), self.raw)

    def test_api_json(self):
        with patch('dashboard.status.report', return_value=self.payload):
            _, headers, body = self.request('/status.html?format=json')
            self.assertEqual(json.loads(gzip.decompress(body)), self.payload)
            self.assertEqual(int(headers['Content-Length']), len(body))
            self.assertEqual(headers['Vary'], 'Accept-Encoding')
            self.assertEqual(headers['Cache-Control'], 'no-store')
            _, headers, body = self.request('/status.html?format=json', 'gzip;q=0')
            self.assertNotIn('Content-Encoding', headers)
            self.assertEqual(json.loads(body), self.payload)

    def test_immutable_chunks_and_large_file_limit(self):
        _, headers, _ = self.request(self.chunk)
        self.assertEqual(headers['Cache-Control'], 'public, max-age=31536000, immutable')
        _, headers, _ = self.request('/data/large.json', method='HEAD')
        self.assertNotIn('Content-Encoding', headers)
        self.assertEqual(int(headers['Content-Length']), 8 * 1024 * 1024 + 1)

    def test_conditional_request(self):
        _, headers, _ = self.request('/data/window.json')
        status, _, body = self.request('/data/window.json', **{'If-Modified-Since': headers['Last-Modified']})
        self.assertEqual(status, 304)
        self.assertEqual(body, b'')
