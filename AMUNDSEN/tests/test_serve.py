"""HTTP regression checks; never start the production live/chat integrations."""

import functools
import http.client
import json
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

from dashboard import alerts
from dashboard.serve import Handler, ThreadingHTTPServer


class TileServingTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        self.tiles = base / "tiles"
        self.tiles.mkdir()
        self.web = base / "web"
        self.web.mkdir()
        (self.web / "index.html").write_text("dashboard", encoding="utf-8")
        (self.tiles / "tile.png").write_bytes(b"tile bytes")
        self.outside = base / "outside.txt"
        self.outside.write_text("not public", encoding="utf-8")
        self.patch = patch("dashboard.serve.TILES_DIR", self.tiles)
        self.patch.start()
        self.addCleanup(self.patch.stop)
        self.server = ThreadingHTTPServer(
            ("127.0.0.1", 0), functools.partial(Handler, directory=str(self.web)))
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.addCleanup(self.stop_server)

    def stop_server(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=3)

    def request(self, path, method="GET"):
        conn = http.client.HTTPConnection("127.0.0.1", self.server.server_port, timeout=3)
        try:
            conn.request(method, path)
            response = conn.getresponse()
            return response.status, response.read()
        finally:
            conn.close()

    def test_ordinary_tile_and_site(self):
        self.assertEqual(self.request("/static/tiles/tile.png?v=123"), (200, b"tile bytes"))
        self.assertEqual(self.request("/static/tiles/tile.png", "HEAD"), (200, b""))
        self.assertEqual(self.request("/"), (200, b"dashboard"))

    def test_absolute_and_traversal_requests(self):
        paths = [
            "/static/tiles/" + self.outside.as_posix(),
            "/static/tiles/../outside.txt",
            "/static/tiles/%2e%2e/outside.txt",
            "/static/tiles/%2f" + self.outside.as_posix().lstrip("/"),
            "/static%2ftiles/%2e%2e/outside.txt",
            "/static/tiles/..%5coutside.txt",
            "/static/tiles/%00.png",
        ]
        for path in paths:
            for method in ("GET", "HEAD"):
                with self.subTest(path=path, method=method):
                    self.assertEqual(self.request(path, method)[0], 404)

    def test_symlink_outside_root(self):
        try:
            (self.tiles / "escape.png").symlink_to(self.outside)
        except (OSError, NotImplementedError):
            self.skipTest("symlinks unavailable")
        self.assertEqual(self.request("/static/tiles/escape.png")[0], 404)

    def test_symlink_inside_root_and_encoded_filename(self):
        (self.tiles / "tile space.png").write_bytes(b"space tile")
        self.assertEqual(self.request("/static/tiles/tile%20space.png"), (200, b"space tile"))
        try:
            (self.tiles / "alias.png").symlink_to(self.tiles / "tile.png")
        except (OSError, NotImplementedError):
            self.skipTest("symlinks unavailable")
        self.assertEqual(self.request("/static/tiles/alias.png"), (200, b"tile bytes"))


if __name__ == "__main__":
    unittest.main()


class HistoryFlagTests(unittest.TestCase):
    """The flag on an artifact's card, through the server."""
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        for name, target in (("DB_DIR", base / "db"), ("is_admin", lambda name, token: False)):
            p = patch.object(alerts, name, target); p.start(); self.addCleanup(p.stop)
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(base)))
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True); self.thread.start()
        self.addCleanup(self.server.server_close); self.addCleanup(self.server.shutdown)

    def call(self, path, body=None):
        conn = http.client.HTTPConnection("127.0.0.1", self.server.server_port, timeout=3)
        try:
            if body is None:
                conn.request("GET", path)
            else:
                conn.request("POST", path, json.dumps(body), {"Content-Type": "application/json"})
            r = conn.getresponse()
            return r.status, json.loads(r.read())
        finally:
            conn.close()

    def test_flag_round_trip(self):
        self.assertEqual(self.call("/api/history/flags"), (200, {"flags": [], "admin": False}))
        st, r = self.call("/api/history/flag", {"id": "x-1", "on": True, "token": "t1", "name": "Ann", "title": "X", "page": "artifact/x-1", "note": "hm"})
        self.assertEqual((st, r["ok"], r["flags"][0]["mine"], r["flags"][0]["raisers"][0]["who"]), (200, True, True, "Ann"))
        st, r = self.call("/api/history/flags?token=t2")
        self.assertEqual((st, r["flags"][0]["mine"]), (200, False))
        self.assertEqual(self.call("/api/history/flag", {"id": "x-1", "on": False, "token": "t2"})[0], 403)
        self.assertEqual(self.call("/api/history/flag", {"id": "bad/id", "on": True, "token": "t2"})[0], 400)
        with patch.object(alerts, "FLAGS_PER_HOUR", 1):
            self.assertEqual(self.call("/api/history/flag", {"id": "x-2", "on": True, "token": "t1"})[0], 429)
        st, r = self.call("/api/history/flag", {"id": "x-1", "on": False, "token": "t1"})
        self.assertEqual((st, r["flags"]), (200, []))
