"""HTTP regression checks; never start the production live/chat integrations."""

import functools
import http.client
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch

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
