"""HTTP sampling checks use temporary storage and no live integrations."""

import copy
from functools import partial
import http.client
import json
from pathlib import Path
import tempfile
import threading
import unittest
import numpy as np

from dashboard.livesample import Handler, SampleService, ThreadingHTTPServer


class SamplingHTTPTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name) / "www"
        (self.root / "data").mkdir(parents=True)
        (self.root / "data" / "manifest.json").write_text(json.dumps({
            "legs": [{"id": "2026_LEG_03"}], "live": "2026_LEG_03"}), encoding="utf-8")
        self.state_dir = Path(temp.name) / "state"
        self.service = SampleService(self.root, self.state_dir,
                                     live_provider=lambda: {"tcp_state": "off"}, start=False)
        self.addCleanup(self.service.close)
        self.server = ThreadingHTTPServer(("127.0.0.1", 0),
                                         partial(Handler, service=self.service, directory=str(self.root)))
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.addCleanup(self.stop_server)

    def stop_server(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=2)

    def request(self, method, path="/api/livesample", body=None, headers=None):
        conn = http.client.HTTPConnection("127.0.0.1", self.server.server_port, timeout=2)
        try:
            conn.request(method, path, body=body, headers=headers or {})
            response = conn.getresponse()
            return response.status, dict(response.getheaders()), response.read()
        finally:
            conn.close()

    def post(self, payload, headers=None):
        return self.request("POST", body=json.dumps(payload).encode(),
                            headers={"Content-Type": "application/json", **(headers or {})})

    def test_page_assets_and_api(self):
        for path, content_type, marker in (
            ("/livesample.html", "text/html", b"static/livesample.js"),
            ("/static/livesample.js", "javascript", b"api/livesample"),
            ("/static/livesample.css", "text/css", b"sample"),
        ):
            with self.subTest(path=path):
                status, headers, body = self.request("GET", path)
                self.assertEqual(status, 200)
                self.assertIn(content_type, headers["Content-Type"])
                self.assertIn(marker, body)
        status, headers, body = self.request("GET", "/api/livesample?t=123")
        self.assertEqual(status, 200)
        self.assertEqual(headers["Cache-Control"], "no-store")
        data = json.loads(body)
        self.assertEqual(data["config"]["legs"], ["2026_LEG_03"])
        self.assertEqual(data["live"], {"tcp_state": "off"})

    def test_template_endpoint_uses_selected_leg_and_validates_picker(self):
        folder = self.root / 'data' / 'casts'
        folder.mkdir(parents=True)
        profile = {'id': '2026_LEG_03:CTD_001', 'leg': '2026_LEG_03', 'cast': '001',
                   'kind': 'CTD', 'bottles': [{'bottle': 1, 'p': 20.}, {'bottle': 12, 'p': 100.}]}
        (folder / 'profile.json').write_text(json.dumps(profile))
        (folder / 'index.json').write_text(json.dumps({'casts': [{**profile, 'file': 'data/casts/profile.json'}]}))
        status, headers, body = self.request('GET', '/api/livesample/template?leg=2026_LEG_03')
        self.assertEqual(status, 200)
        self.assertIn('tab-separated-values', headers['Content-Type'])
        self.assertEqual(len(body.decode().splitlines()), 3)
        self.assertIn(b'2026_LEG_03:CTD_001\t12\t', body)
        self.assertEqual(self.request('GET', '/api/livesample/template?leg=other')[0], 400)
        self.assertEqual(self.request('GET', '/api/livesample/template')[0], 400)

    def test_json_settings_and_tsv_are_saved(self):
        tsv = "cast\tbottle\n001\t1\n"
        payload = {"algorithm": "rarity", "count": 4, "min_spacing": 15, "tsv": tsv}
        origin = f"http://127.0.0.1:{self.server.server_port}"
        status, _, body = self.post(payload, {"Origin": origin, "Sec-Fetch-Site": "same-origin"})
        self.assertEqual(status, 200)
        self.assertEqual(json.loads(body), {"ok": True})
        saved = json.loads((self.state_dir / "settings.json").read_text())
        self.assertEqual(saved["tsv"], tsv)
        self.assertEqual(saved["config"]["algorithm"], "rarity")
        self.assertEqual(saved["config"]["count"], 4)
        self.assertEqual(saved["config"]["min_spacing"], 15.)
        self.assertEqual(self.post({"count": 5})[0], 200)
        self.assertEqual(self.service.tsv, tsv)
        self.assertEqual(json.loads(self.request("GET")[2])["config"]["count"], 5)

    def test_request_origin_and_content_type_rejections_preserve_state(self):
        self.assertEqual(self.post({"count": 4})[0], 200)
        original = copy.deepcopy(self.service.config)
        saved = (self.state_dir / "settings.json").read_bytes()
        for headers in ({"Sec-Fetch-Site": "cross-site"}, {"Origin": "http://other.example"},
                        {"Content-Type": "text/plain"}, {"Transfer-Encoding": "chunked"}):
            with self.subTest(headers=headers):
                status, _, body = self.post({"count": 5}, headers)
                self.assertEqual(status, 400)
                self.assertTrue(json.loads(body)["error"])
                self.assertEqual(self.service.config, original)
                self.assertEqual((self.state_dir / "settings.json").read_bytes(), saved)

    def test_malformed_json_and_setting_types_return_400_without_mutation(self):
        original = copy.deepcopy(self.service.config)
        payloads = [None, [], "settings", {"count": "3"}, {"count": True},
                    {"algorithm": []}, {"legs": {}}, {"min_spacing": {}},
                    {"min_spacing": 10 ** 400}, {"telegram": "yes"}, {"tsv": []},
                    {"tsv": "cast\tbottle\n001\tbad\n"}]
        for payload in payloads:
            with self.subTest(payload=payload):
                status, _, body = self.post(payload)
                self.assertEqual(status, 400)
                self.assertTrue(json.loads(body)["error"])
                self.assertEqual(self.service.config, original)
        status, _, body = self.request("POST", body=b"{not-json", headers={"Content-Type": "application/json"})
        self.assertEqual(status, 400)
        self.assertTrue(json.loads(body)["error"])
        self.assertEqual(self.service.config, original)
        self.assertFalse((self.state_dir / "settings.json").exists())

    def test_flow_controls_are_validated_and_do_not_change_saved_settings(self):
        manifest_path = self.root / 'data' / 'manifest.json'
        manifest = json.loads(manifest_path.read_text())
        manifest['windows'] = [{'label': '6h', 'file': 'data/w-6h.json'}]
        manifest_path.write_text(json.dumps(manifest))
        data = {'t': [1000000, 1060000, 1120000, 1180000, 1240000], 'leg': [0] * 5,
                'vars': {'SST (°C)': [1, 2, 3, 4, 5], 'Salinity (PSU)': [31] * 5, 'TSG flow (V)': [1] * 5}}
        (self.root / 'data' / 'w-6h.json').write_text(json.dumps(data))
        self.service.model = {'ref': np.array([[0., 0.], [1., 1.], [2., 2.]]),
                              'sampled': np.empty((0, 2)), 'bottle_sampled': np.array([[0., 0.], [1., 1.]]),
                              'med': np.array([0., 30.]), 'scale': np.ones(2)}
        original = copy.deepcopy(self.service.config)
        code, _, body = self.request('GET', '/api/livesample/flow?window=6h&algorithm=rarity&count=4&spacing=1440')
        self.assertEqual(code, 200)
        result = json.loads(body)
        self.assertEqual(result['algorithm'], 'rarity')
        self.assertEqual(result['requested_count'], 4)
        self.assertEqual(len(result['targets']), 4)
        self.assertEqual(len(result['distribution']['values']), 5)
        for query in ('count=0', 'count=25', 'count=1.5', 'count=', 'algorithm=nope',
                      'spacing=0', 'spacing=1441', 'spacing=nan', 'spacing=bad'):
            with self.subTest(query=query):
                code, _, body = self.request('GET', '/api/livesample/flow?window=6h&' + query)
                self.assertEqual(code, 400)
                self.assertTrue(json.loads(body)['error'])
        self.assertEqual(self.service.config, original)
        self.assertFalse((self.state_dir / 'settings.json').exists())

    def test_large_plot_payload_compresses_when_requested(self):
        import gzip
        self.service.result['embeddings'] = {'umap': {'points': [{'x': i, 'y': i, 'cast': 'example'} for i in range(100)]}}
        code, headers, body = self.request('GET', headers={'Accept-Encoding': 'gzip'})
        self.assertEqual(code, 200)
        self.assertEqual(headers['Content-Encoding'], 'gzip')
        self.assertEqual(len(json.loads(gzip.decompress(body))['embeddings']['umap']['points']), 100)
        _, headers, body = self.request('GET', headers={'Accept-Encoding': 'gzip;q=0'})
        self.assertNotIn('Content-Encoding', headers)
        self.assertEqual(len(json.loads(body)['embeddings']['umap']['points']), 100)


if __name__ == "__main__":
    unittest.main()
