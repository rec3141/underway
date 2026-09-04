"""Serve the dashboard directory.

A thin wrapper over the standard static server that marks data files as
uncacheable, so a page polling for updates always sees the newest build.
"""

from __future__ import annotations

import logging
import os
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

log = logging.getLogger(__name__)


# Raster tile pyramids are hundreds of thousands of small files; they are kept
# on local disk and served from here rather than written to the CIFS share.
TILES_DIR = Path(os.environ.get("UNDERWAY_TILES_DIR", "/data/gis/tiles"))


class Handler(SimpleHTTPRequestHandler):
    def log_message(self, fmt, *args):          # only failures are worth a line
        if str(args[1:2]).startswith(("('4", "('5")):
            log.info("%s %s", self.address_string(), fmt % args)

    def translate_path(self, path):
        p = path.split("?", 1)[0].split("#", 1)[0]
        if p.startswith("/static/tiles/"):
            rel = Path(p[len("/static/tiles/"):])
            if ".." in rel.parts:
                return str(TILES_DIR / "nonexistent")
            return str(TILES_DIR / rel)
        return super().translate_path(path)

    def end_headers(self):
        p = self.path.split("?")[0]
        if p.startswith("/data/") or p.endswith(".json"):
            self.send_header("Cache-Control", "no-store")            # rebuilt every few minutes
        elif p == "/" or p.endswith(".html"):
            self.send_header("Cache-Control", "no-cache")            # revalidate; carries the asset versions
        elif p.startswith(("/static/geo/", "/static/tiles/")) or p.endswith("plotly.min.js"):
            self.send_header("Cache-Control", "public, max-age=604800")   # big, rarely change, versioned URL
        else:
            self.send_header("Cache-Control", "no-cache")
        super().end_headers()


def serve(root: Path, port: int, bind: str) -> None:
    httpd = ThreadingHTTPServer((bind, port), partial(Handler, directory=str(root)))
    log.info("serving %s on http://%s:%d/", root, bind or "0.0.0.0", port)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        httpd.server_close()
