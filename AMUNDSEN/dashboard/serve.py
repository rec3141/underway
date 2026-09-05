"""Serve the dashboard directory.

A thin wrapper over the standard static server that marks data files as
uncacheable, so a page polling for updates always sees the newest build, and
carries the one dynamic thing on the site: a chat for whoever has the page
open (``/api/chat``), kept in a small SQLite file outside the web root.
"""

from __future__ import annotations

import json
import logging
import os
import sqlite3
import threading
import time
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, unquote, urlsplit

log = logging.getLogger(__name__)


# ---------------------------------------------------------------- chat
CHAT_DB = Path(os.environ.get("UNDERWAY_CHAT_DB", "/data/underway/chat/chat.sqlite"))
CHAT_KEEP = 2000            # messages kept
CHAT_PAGE = 100             # messages sent to a fresh page
NAME_MAX, TEXT_MAX = 24, 500
_chat_lock = threading.Lock()
_online: dict[str, float] = {}          # name -> last poll, for "who has the page open"
_last_post: dict[str, float] = {}       # address -> last post, a light rate limit


_emoji: dict[str, str] = {}             # name -> avatar last seen with
CREW = None                             # the model-driven crew, once the server is up
LIVE = None                             # the live CTD listener, once the server is up


def _chat_conn() -> sqlite3.Connection:
    CHAT_DB.parent.mkdir(parents=True, exist_ok=True)
    c = sqlite3.connect(CHAT_DB, timeout=5)
    c.execute("CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY, t REAL NOT NULL, addr TEXT, name TEXT NOT NULL, text TEXT NOT NULL)")
    cols = {r[1] for r in c.execute("PRAGMA table_info(messages)")}
    if "emoji" not in cols:
        c.execute("ALTER TABLE messages ADD COLUMN emoji TEXT")
    return c


def _clean_emoji(e: str) -> str:
    e = (e or "").strip()
    return e[:8] if e and "<" not in e else ""


def chat_read(since: int, name: str | None, emoji: str = "") -> dict:
    now = time.time()
    with _chat_lock:
        if name:
            _online[name] = now
            if emoji:
                _emoji[name] = _clean_emoji(emoji)
        for k in [k for k, v in _online.items() if now - v > 45]:
            del _online[k]
        online = [{"name": n, "emoji": _emoji.get(n, "")} for n in sorted(_online)]
        c = _chat_conn()
        try:
            if since > 0:
                rows = c.execute("SELECT id, t, name, text, emoji FROM messages WHERE id > ? ORDER BY id", (since,)).fetchall()
            else:
                rows = c.execute("SELECT id, t, name, text, emoji FROM messages ORDER BY id DESC LIMIT ?", (CHAT_PAGE,)).fetchall()[::-1]
        finally:
            c.close()
    typing = sorted(CREW.typing) if CREW else []
    return {"messages": [{"id": i, "t": t, "name": n, "text": x, "emoji": e or ""} for i, t, n, x, e in rows],
            "online": online, "typing": typing, "crew": crew_list(), "model": crew_model(), "now": now}


def crew_model() -> str:
    from .chatbot import LLM_MODEL
    return LLM_MODEL if CREW and CREW.enabled else ""


def crew_list() -> list[dict]:
    from .chatbot import PERSONAS
    return [{"handle": h, "name": p["name"], "emoji": p["emoji"]} for h, p in PERSONAS.items()] if CREW and CREW.enabled else []


def chat_post(addr: str, name: str, text: str, emoji: str = "", bot: bool = False) -> dict:
    name = " ".join(name.split())[:NAME_MAX] or "anon"
    text = text.strip()[:TEXT_MAX if not bot else 2600]
    emoji = _clean_emoji(emoji)
    if not text:
        return {"error": "empty"}
    now = time.time()
    with _chat_lock:
        if not bot:
            if now - _last_post.get(addr, 0) < 1.0:
                return {"error": "slow down"}
            _last_post[addr] = now
            _online[name] = now
            if emoji:
                _emoji[name] = emoji
        c = _chat_conn()
        try:
            cur = c.execute("INSERT INTO messages (t, addr, name, text, emoji) VALUES (?, ?, ?, ?, ?)", (now, addr, name, text, emoji))
            c.execute("DELETE FROM messages WHERE id <= (SELECT MAX(id) FROM messages) - ?", (CHAT_KEEP,))
            c.commit()
            mid = cur.lastrowid
        finally:
            c.close()
    if not bot and CREW:
        CREW.on_message(name, text)
    return {"ok": True, "id": mid, "t": now}


# Raster tile pyramids are hundreds of thousands of small files; they are kept
# on local disk and served from here rather than written to the CIFS share.
TILES_DIR = Path(os.environ.get("UNDERWAY_TILES_DIR", "/data/gis/tiles"))
# the camera timelapses are built by their own job outside the web root
from .config import CAMERA_OUTPUT


class Handler(SimpleHTTPRequestHandler):
    def log_message(self, fmt, *args):          # only failures are worth a line
        if str(args[1:2]).startswith(("('4", "('5")):
            log.info("%s %s", self.address_string(), fmt % args)

    def _json(self, code: int, payload: dict) -> None:
        body = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        SimpleHTTPRequestHandler.end_headers(self)
        self.wfile.write(body)

    def _client(self) -> str:
        return self.headers.get("X-Forwarded-For", "").split(",")[0].strip() or self.client_address[0]

    def do_GET(self):
        u = urlsplit(self.path)
        if u.path == "/api/live":
            return self._json(200, LIVE.status() if LIVE else {"port": 0})
        if u.path == "/api/chat":
            q = parse_qs(u.query)
            try:
                since = int(q.get("since", ["0"])[0])
            except ValueError:
                since = 0
            name = (q.get("name", [""])[0] or "").strip()[:NAME_MAX] or None
            try:
                return self._json(200, chat_read(since, name, q.get("emoji", [""])[0]))
            except Exception as e:                       # noqa: BLE001
                log.warning("chat read failed: %s", e)
                return self._json(500, {"error": "chat unavailable"})
        return super().do_GET()

    def do_POST(self):
        u = urlsplit(self.path)
        if u.path == "/api/live" and LIVE:
            # retune the listener from the page: {"port": 5555, "columns": "scan,pressure,..."}
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 <= n <= 4096:
                    raise ValueError("Configuration body must be at most 4096 bytes")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise ValueError("Configuration must be a JSON object")
                LIVE.configure(payload.get("port", LIVE.port), payload.get("columns"))
                return self._json(200, LIVE.status())
            except Exception as e:                       # noqa: BLE001
                return self._json(400, {"error": str(e)})
        if u.path != "/api/chat":
            return self._json(404, {"error": "not found"})
        try:
            n = min(int(self.headers.get("Content-Length", "0")), 4096)
            payload = json.loads(self.rfile.read(n) or b"{}")
            r = chat_post(self._client(), str(payload.get("name", "")), str(payload.get("text", "")), str(payload.get("emoji", "")))
        except Exception as e:                           # noqa: BLE001
            log.warning("chat post failed: %s", e)
            r = {"error": "bad request"}
        return self._json(200 if r.get("ok") else 400, r)

    def translate_path(self, path):
        p = unquote(urlsplit(path).path)
        for prefix, base in (("/static/tiles/", TILES_DIR), ("/camera/", CAMERA_OUTPUT)):   # read at call time: tests swap the roots
            if not p.startswith(prefix):
                continue
            rel = Path(p[len(prefix):])
            # An absolute suffix discards the root when joined. Resolve before
            # checking containment so symlinks cannot escape it either.
            if rel.is_absolute() or ".." in rel.parts or "\\" in p:
                raise ValueError("invalid path")
            try:
                root = base.resolve()
                candidate = (root / rel).resolve()
            except (OSError, RuntimeError) as e:
                raise ValueError("invalid path") from e
            if not candidate.is_relative_to(root):
                raise ValueError("invalid path")
            return str(candidate)
        return super().translate_path(path)

    def send_head(self):
        # Both GET and HEAD pass through here. Do not substitute a sentinel
        # filename: that file could actually exist inside the configured root.
        try:
            return super().send_head()
        except ValueError:
            self.send_error(404, "File not found")
            return None

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
    global CREW, LIVE
    from .chatbot import Crew
    from .live import LiveCTD
    try:
        LIVE = LiveCTD()
    except Exception as e:                  # noqa: BLE001 — a taken port must not stop the site
        log.warning("live CTD listener not started: %s", e)
        LIVE = LiveCTD(port=0, columns="scan,pressure,temperature,conductivity,salinity,oxygen,fluorescence")
    CREW = Crew(root, post=lambda n, e, t: chat_post("crew", n, t, e, bot=True), read=lambda: chat_read(0, None))
    CREW.start()
    httpd = ThreadingHTTPServer((bind, port), partial(Handler, directory=str(root)))
    log.info("serving %s on http://%s:%d/", root, bind or "0.0.0.0", port)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        httpd.server_close()
        if LIVE:
            LIVE.close()
