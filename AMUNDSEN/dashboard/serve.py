"""Serve the dashboard directory.

A thin wrapper over the standard static server that marks data files as
uncacheable, so a page polling for updates always sees the newest build, and
carries the one dynamic thing on the site: a chat for whoever has the page
open (``/api/chat``), kept in a small SQLite file outside the web root.
"""

from __future__ import annotations

import html
import json
from datetime import datetime, timezone
import urllib.request
import re
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
INTRANET = None                         # the intranet live-page poller, once the server is up


class IntranetLive:
    """The ship intranet's live page (``INTRANET_BASE/live.html``: navigation,
    atmosphere, sea-water surface, rosette and 500HP winch tables, refreshed
    by the acquisition host every few seconds), polled every ``every``
    seconds and kept as label/value rows per table for ``/api/intranet``.
    The page has no data endpoint, so the tables are read off the HTML: each
    ``<table>`` has a ``<th>`` title and then ``<td>`` label, ``<td>`` value
    pairs. Failures are reported, never raised."""

    def __init__(self, url: str, every: float = 4.0):
        self.url, self.every = url, every
        self.lock = threading.Lock()
        self.sections: list[dict] = []
        self.fetched = 0.0
        self.error = ""
        self._stop = threading.Event()
        threading.Thread(target=self._loop, daemon=True).start()

    @staticmethod
    def parse(text: str) -> list[dict]:
        import html as _html
        out = []
        for tbl in re.findall(r"<table[^>]*>(.*?)</table>", text, flags=re.S | re.I):
            th = re.search(r"<th[^>]*>(.*?)</th>", tbl, flags=re.S | re.I)
            cells = [" ".join(_html.unescape(re.sub(r"<[^>]+>", " ", c)).split()) for c in re.findall(r"<td[^>]*>(.*?)</td>", tbl, flags=re.S | re.I)]
            rows = [[cells[i], cells[i + 1]] for i in range(0, len(cells) - 1, 2)]
            if th and rows:
                out.append({"title": " ".join(_html.unescape(re.sub(r"<[^>]+>", " ", th.group(1))).split()), "rows": rows})
        return out

    def _loop(self) -> None:
        while not self._stop.is_set():
            try:
                with urllib.request.urlopen(self.url, timeout=6) as r:
                    raw = r.read()
                try:
                    text = raw.decode("utf-8")               # the page's "mg/m³" is UTF-8 despite its charset header
                except UnicodeDecodeError:
                    text = raw.decode("latin-1")
                sections = self.parse(text)
                with self.lock:
                    self.sections, self.fetched, self.error = sections, time.time(), "" if sections else "no tables on the page"
                try:
                    from .livescrape import record
                    record(sections)                     # the page keeps no history; we do
                except Exception as e:                   # noqa: BLE001
                    log.warning("live page not recorded: %s", e)
            except Exception as e:                       # noqa: BLE001
                with self.lock:
                    self.error = str(e)[:120]
            self._stop.wait(self.every)

    def status(self) -> dict:
        with self.lock:
            return {"url": self.url, "fetched": self.fetched, "age_s": round(time.time() - self.fetched, 1) if self.fetched else None,
                    "error": self.error, "sections": self.sections}


def _chat_conn() -> sqlite3.Connection:
    CHAT_DB.parent.mkdir(parents=True, exist_ok=True)
    c = sqlite3.connect(CHAT_DB, timeout=5)
    c.execute("CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY, t REAL NOT NULL, addr TEXT, name TEXT NOT NULL, text TEXT NOT NULL)")
    cols = {r[1] for r in c.execute("PRAGMA table_info(messages)")}
    if "emoji" not in cols:
        c.execute("ALTER TABLE messages ADD COLUMN emoji TEXT")
    if "channel" not in cols:
        # two rooms share the one log: the crew's chat and the historian's
        c.execute("ALTER TABLE messages ADD COLUMN channel TEXT NOT NULL DEFAULT 'crew'")
        c.execute("CREATE INDEX IF NOT EXISTS messages_channel ON messages (channel, id)")
    if "meta" not in cols:
        c.execute("ALTER TABLE messages ADD COLUMN meta TEXT")       # JSON: the pages an answer drew on
    return c


CHANNELS = ("crew", "historian")
HISTORIAN = {"name": "Historian", "emoji": "📜", "handle": "historian"}
ROOT: Path | None = None                    # the web root, for the historian's wiki
_asking: set[str] = set()                   # channels with an answer in flight, for the typing line


def _channel(v: str) -> str:
    return v if v in CHANNELS else "crew"


def _clean_emoji(e: str) -> str:
    e = (e or "").strip()
    return e[:8] if e and "<" not in e else ""


def chat_read(since: int, name: str | None, emoji: str = "", leave: str | None = None, channel: str = "crew") -> dict:
    channel = _channel(channel)
    now = time.time()
    with _chat_lock:
        if name:
            _online[name] = now
            if emoji:
                _emoji[name] = _clean_emoji(emoji)
        if leave:                            # the chat was closed: not "here" any more
            _online.pop(leave, None)
        for k in [k for k, v in _online.items() if now - v > 45]:
            del _online[k]
        online = [{"name": n, "emoji": _emoji.get(n, "")} for n in sorted(_online)]
        c = _chat_conn()
        try:
            if since > 0:
                rows = c.execute("SELECT id, t, name, text, emoji, meta FROM messages WHERE channel = ? AND id > ? ORDER BY id",
                                 (channel, since)).fetchall()
            else:
                rows = c.execute("SELECT id, t, name, text, emoji, meta FROM messages WHERE channel = ? ORDER BY id DESC LIMIT ?",
                                 (channel, CHAT_PAGE)).fetchall()[::-1]
            latest = {ch: mid or 0 for ch, mid in c.execute("SELECT channel, MAX(id) FROM messages GROUP BY channel")}
        finally:
            c.close()
    typing = (sorted(CREW.typing) if CREW and channel == "crew" else []) + (["historian"] if channel in _asking else [])
    msgs = []
    for i, t, n, x, e, meta in rows:
        m = {"id": i, "t": t, "name": n, "text": x, "emoji": e or ""}
        if meta:
            try:
                m["meta"] = json.loads(meta)
            except ValueError:
                pass
        msgs.append(m)
    return {"messages": msgs, "online": online, "typing": typing, "crew": crew_list(channel), "model": crew_model(),
            "channel": channel, "latest": {ch: latest.get(ch, 0) for ch in CHANNELS}, "now": now}


def crew_model() -> str:
    from .chatbot import LLM_MODEL
    return LLM_MODEL if CREW and CREW.enabled else ""


def crew_list(channel: str = "crew") -> list[dict]:
    from .chatbot import PERSONAS
    if channel == "historian":
        return [dict(HISTORIAN)] if CREW and CREW.enabled else []
    return [{"handle": h, "name": p["name"], "emoji": p["emoji"]} for h, p in PERSONAS.items()] if CREW and CREW.enabled else []


def chat_post(addr: str, name: str, text: str, emoji: str = "", bot: bool = False, channel: str = "crew",
              meta: dict | None = None, slug: str = "") -> dict:
    channel = _channel(channel)
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
            cur = c.execute("INSERT INTO messages (t, addr, name, text, emoji, channel, meta) VALUES (?, ?, ?, ?, ?, ?, ?)",
                            (now, addr, name, text, emoji, channel, json.dumps(meta) if meta else None))
            c.execute("DELETE FROM messages WHERE id <= (SELECT MAX(id) FROM messages) - ?", (CHAT_KEEP,))
            c.commit()
            mid = cur.lastrowid
        finally:
            c.close()
    if not bot and channel == "crew" and CREW:
        CREW.on_message(name, text)
    if not bot and channel == "historian":
        threading.Thread(target=_historian_reply, args=(text, slug), daemon=True).start()
    return {"ok": True, "id": mid, "t": now}


def _historian_reply(question: str, slug: str) -> None:
    """Answer a question posted in the historian's room, from the wiki, and
    post it as the Historian with the pages it read."""
    if not ROOT or not (CREW and CREW.enabled):
        return
    _asking.add("historian")
    try:
        r = history_ask(ROOT, question, slug)
        chat_post("historian", HISTORIAN["name"], r["answer"], HISTORIAN["emoji"], bot=True, channel="historian",
                  meta={"pages": r["pages"]})
    except ValueError as e:
        chat_post("historian", HISTORIAN["name"], str(e), HISTORIAN["emoji"], bot=True, channel="historian")
    except Exception as e:                   # noqa: BLE001
        log.info("historian stayed quiet (%s)", e)
    finally:
        _asking.discard("historian")


# Raster tile pyramids are hundreds of thousands of small files; they are kept
# on local disk and served from here rather than written to the CIFS share.
TILES_DIR = Path(os.environ.get("UNDERWAY_TILES_DIR", "/data/gis/tiles"))
# the camera timelapses are built by their own job outside the web root
from .config import CAMERA_OUTPUT


class Handler(SimpleHTTPRequestHandler):
    # The basemap is GeoJSON, a megabyte a file. Served as application/geo+json
    # the front proxy leaves it uncompressed (its compression list does not
    # know the type); as application/json it goes out gzipped at a quarter the size.
    extensions_map = {**SimpleHTTPRequestHandler.extensions_map, ".geojson": "application/json"}

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
        if u.path == "/api/alerts/following":
            from .alerts import following
            q = parse_qs(u.query)
            return self._json(200, following(q.get("channel", ["email"])[0], q.get("to", [""])[0]))
        if u.path == "/api/alerts/inbox":
            # the page's own alerts: what the timer has queued for this browser since an instant
            from .alerts import inbox
            q = parse_qs(u.query)
            try:
                msgs = inbox(q.get("to", [""])[0], q.get("since", [""])[0])
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            return self._json(200, {"messages": msgs, "now": datetime.now(timezone.utc).isoformat(timespec="seconds")})
        if u.path == "/api/alerts/unsubscribe":
            from .alerts import unsubscribe
            gone = unsubscribe(parse_qs(u.query).get("token", [""])[0])
            body = ("<p>Unsubscribed: no more schedule alerts to " + html.escape(gone["to"]) + ".</p>") if gone else "<p>That subscription is already gone.</p>"
            data = ("<!doctype html><meta charset=utf-8><title>Amundsen alerts</title><body style='font:16px system-ui;padding:2em'>" + body).encode()
            self.send_response(200); self.send_header("Content-Type", "text/html; charset=utf-8"); self.send_header("Content-Length", str(len(data))); self.end_headers()
            self.wfile.write(data)
            return
        if u.path == "/api/live":
            return self._json(200, LIVE.status() if LIVE else {"tcp": "", "tcp_state": "off"})
        if u.path == "/api/intranet":
            return self._json(200, INTRANET.status() if INTRANET else {"sections": [], "error": "not polling"})
        if u.path == "/history/requests":
            # the review page for the research crew's requests: downloads over
            # the limit, API keys, questions a person has to answer
            from .history import connect, list_requests
            try:
                c = connect(create=False)
                try:
                    rows = list_requests(c)
                finally:
                    c.close()
            except FileNotFoundError:
                rows = []
            data = requests_page(rows).encode()
            self.send_response(200); self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(data))); self.send_header("Cache-Control", "no-store")
            SimpleHTTPRequestHandler.end_headers(self)
            self.wfile.write(data)
            return
        if u.path == "/api/history/requests":
            from .history import connect, list_requests
            q = parse_qs(u.query)
            try:
                c = connect(create=False)
                try:
                    return self._json(200, {"requests": list_requests(c, q.get("status", [None])[0])})
                finally:
                    c.close()
            except FileNotFoundError:
                return self._json(200, {"requests": []})
        if u.path == "/api/chat":
            q = parse_qs(u.query)
            try:
                since = int(q.get("since", ["0"])[0])
            except ValueError:
                since = 0
            name = (q.get("name", [""])[0] or "").strip()[:NAME_MAX] or None
            leave = (q.get("leave", [""])[0] or "").strip()[:NAME_MAX] or None
            try:
                return self._json(200, chat_read(since, name, q.get("emoji", [""])[0], leave, q.get("channel", ["crew"])[0]))
            except Exception as e:                       # noqa: BLE001
                log.warning("chat read failed: %s", e)
                return self._json(500, {"error": "chat unavailable"})
        return super().do_GET()

    def do_POST(self):
        u = urlsplit(self.path)
        if u.path == "/api/live" and LIVE:
            # point the listener at Seasave from the page: {"tcp": "10.0.0.22:49161"}
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 <= n <= 4096:
                    raise ValueError("Configuration body must be at most 4096 bytes")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise ValueError("Configuration must be a JSON object")
                LIVE.configure(payload.get("tcp"))
                return self._json(200, LIVE.status())
            except Exception as e:                       # noqa: BLE001
                return self._json(400, {"error": str(e)})
        if u.path == "/api/plan":
            # a KMZ or KML dropped on the map: parsed here, kept by that browser
            # alone (the leg's own plan in db/plan/ is the build's business)
            from .plan import parse
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 8 * 1024 * 1024:
                    raise ValueError("the file must be under 8 MB")
                return self._json(200, {"ok": True, **parse(self.rfile.read(n))})
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("plan upload failed: %s", e)
                return self._json(400, {"error": "that file could not be read as KML or KMZ"})
        if u.path == "/api/alerts/row":
            # the bell: follow (or drop) one operation: {"channel": "email", "to": ..., "key": "CardS-3|CTD-Rosette", "remove": false}
            from .alerts import follow_row, following
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 <= n <= 4096:
                    raise ValueError("Request too large")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise ValueError("Bad request")
                channel, to = str(payload.get("channel", "email")), str(payload.get("to", ""))
                follow_row(channel, to, str(payload.get("key", "")), remove=bool(payload.get("remove")), name=str(payload.get("name", "")))
                return self._json(200, {"ok": True, **following(channel, to)})
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("alert row subscription failed: %s", e)
                return self._json(500, {"error": "could not save the subscription"})
        if u.path == "/api/alerts":
            # subscribe from the page: {"channel": "email", "to": ..., "match": "CardS-3, CTD", "lead_min": 30, "events": [...]}
            from .alerts import subscribe
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 <= n <= 4096:
                    raise ValueError("Request too large")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise ValueError("Bad request")
                sub = subscribe(str(payload.get("channel", "email")), str(payload.get("to", "")), str(payload.get("match", "")),
                                payload.get("lead_min", 30), payload.get("events"), str(payload.get("name", "")))
                return self._json(200, {"ok": True, "to": sub["to"], "match": sub["match"], "lead_min": sub["lead_min"], "events": sub["events"]})
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("alert subscription failed: %s", e)
                return self._json(500, {"error": "could not save the subscription"})
        if u.path == "/api/history/ask":
            # the History tab's historian: a question, answered by the local
            # model from the published wiki pages, with the pages it used
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 8192:
                    raise ValueError("a question is at most 8 KB")
                payload = json.loads(self.rfile.read(n) or b"{}")
                question = str(payload.get("question", "")).strip()[:600]
                if not question:
                    raise ValueError("ask something")
                r = history_ask(Path(self.directory), question, str(payload.get("slug", ""))[:200])
                return self._json(200, r)
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("history ask failed: %s", e)
                return self._json(503, {"error": "the historian is not answering right now; the model may be busy or off"})
        if u.path == "/api/history/requests":
            # the ship's copy of the history database is a pulled snapshot;
            # answers are written on grid, where the research crew works
            return self._json(403, {"error": "the ship's history database is read-only; answer requests on grid with history-db.py answer"})
        if u.path != "/api/chat":
            return self._json(404, {"error": "not found"})
        try:
            n = min(int(self.headers.get("Content-Length", "0")), 4096)
            payload = json.loads(self.rfile.read(n) or b"{}")
            r = chat_post(self._client(), str(payload.get("name", "")), str(payload.get("text", "")), str(payload.get("emoji", "")),
                          channel=str(payload.get("channel", "crew")), slug=str(payload.get("slug", ""))[:200])
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


# ---------------------------------------------------------------- the historian
_ask_lock = threading.Lock()


def history_ask(root: Path, question: str, slug: str = "") -> dict:
    """The historian's room: the wiki pages that bear on the question go in
    front of the model, and the answer comes back with the pages it was given."""
    from .chatbot import complete, excerpt_block, wiki_excerpts
    excerpts = wiki_excerpts(root, question, slug)
    if not excerpts:
        raise ValueError("no history has been published yet")
    system = ("You are the historian aboard the research icebreaker CCGS Amundsen, answering scientists' questions about the "
              "history of the Canadian Arctic Archipelago and Baffin Bay. Answer from the wiki excerpts below, which were "
              "written by the ship's research crew from primary sources; when the excerpts do not cover something, say so "
              "plainly and then give your best general knowledge, marked as such. Be concrete: dates, names, places, "
              "coordinates when the excerpts give them. Use Inuit names for people and places as the excerpts do. Where "
              "the record is disputed or rests on testimony, say whose. Cite the pages you draw on inline by their title in "
              "square brackets, like [The death march]. Plain prose, short paragraphs, no headings, no bullet lists unless "
              "listing dates. At most about 350 words.\n\nWIKI EXCERPTS\n\n" + excerpt_block(excerpts))
    with _ask_lock:
        answer = complete(system, question, max_tokens=700, temperature=0.3, num_ctx=16384, timeout=240)
    return {"answer": answer, "pages": [{"slug": e["slug"], "title": e["title"], "kind": e["kind"]} for e in excerpts]}


def requests_page(rows: list[dict]) -> str:
    """The review page: every request the research crew has filed, open ones
    first, with the answers given so far. Read-only on the ship: the database
    here is a pulled snapshot, and answers are written on grid."""
    esc = html.escape
    order = {"open": 0, "approved": 1, "done": 2, "denied": 3}
    rows = sorted(rows, key=lambda r: (order.get(r["status"], 9), -r["id"]))
    cards = []
    for r in rows:
        size = f" · {r['size_mb']:.0f} MB" if r.get("size_mb") else ""
        link = f'<a href="{esc(r["url"])}" target="_blank" rel="noopener">{esc(r["url"])}</a>' if r.get("url") else ""
        cards.append(
            f'<section class="req {esc(r["status"])}" data-id="{r["id"]}">'
            f'<div class="head"><span class="id">#{r["id"]}</span> <span class="kind">{esc(r["kind"])}</span>'
            f'<span class="topic">{esc(r["topic"])}</span><span class="status">{esc(r["status"])}</span>'
            f'<span class="when">{esc(r["created"][:16].replace("T", " "))}{" · " + esc(r["who"]) if r.get("who") else ""}</span></div>'
            f'<div class="what">{esc(r["what"])}{size}</div>'
            + (f'<div class="why">{esc(r["why"])}</div>' if r.get("why") else "")
            + (f'<div class="url">{link}</div>' if link else "")
            + (f'<div class="dest">to <code>db/history/{esc(r["dest"])}</code></div>' if r.get("dest") else "")
            + (f'<div class="answer">{esc(r["answer"])}</div>' if r.get("answer") else "")
            + f'</section>')
    body = "\n".join(cards) or '<p class="muted">No requests. The crew files them with <code>history-db.py request</code>.</p>'
    n_open = sum(1 for r in rows if r["status"] == "open")
    return f"""<!doctype html><meta charset=utf-8><meta name=viewport content="width=device-width,initial-scale=1">
<title>History research requests</title>
<style>
body{{font:15px/1.45 system-ui,sans-serif;background:#0f1419;color:#e6ecf2;margin:0;padding:20px;max-width:900px;margin:auto}}
h1{{font-size:20px;margin:0 0 4px}} .sub{{color:#8b9bb0;margin-bottom:18px}}
.req{{background:#161d26;border:1px solid #263140;border-radius:10px;padding:12px 14px;margin-bottom:12px}}
.req.open{{border-color:#ffb454}} .req.denied{{opacity:.55}} .req.done{{opacity:.7}}
.head{{display:flex;gap:10px;align-items:baseline;flex-wrap:wrap;font-size:12.5px;color:#8b9bb0}}
.head .id{{font-family:ui-monospace,monospace;color:#e6ecf2}} .head .kind{{color:#5cc8ff;text-transform:uppercase;letter-spacing:.5px}}
.head .status{{margin-left:auto;font-weight:600;color:#ffb454}} .req.done .status,.req.approved .status{{color:#7ee787}} .req.denied .status{{color:#ff7b72}}
.what{{font-size:16px;font-weight:600;margin:6px 0 2px}} .why{{color:#c9d3de}} .url,.dest{{font-size:13px;word-break:break-all;margin-top:4px}}
a{{color:#5cc8ff;text-decoration:none}} code{{font-family:ui-monospace,monospace;font-size:12.5px}}
.acts{{display:flex;gap:8px;margin-top:8px}} button{{background:#1c2632;color:#e6ecf2;border:1px solid #263140;border-radius:7px;padding:5px 12px;font:inherit;cursor:pointer}}
button:hover{{border-color:#5cc8ff}} button.no:hover{{border-color:#ff7b72}} button.re{{margin-left:auto;opacity:.7}}
.answer{{margin-top:8px;padding:8px 10px;background:#0f1419;border-left:3px solid #7ee787;border-radius:6px;font-size:14px}}
.muted{{color:#8b9bb0}} .toast{{position:fixed;bottom:16px;left:50%;transform:translateX(-50%);background:#263140;padding:8px 14px;border-radius:8px}}
</style>
<h1>History research requests</h1>
<div class=sub>{n_open} waiting · what the research crew cannot do alone: downloads over 50 MB, API keys, judgement calls. This is the ship's read-only view of the last pull; answers are written on grid with <code>history-db.py answer --id N --status approved --answer "…"</code>.</div>
{body}"""


def serve(root: Path, port: int, bind: str) -> None:
    global CREW, LIVE, INTRANET, ROOT
    ROOT = root
    from .config import INTRANET_BASE
    INTRANET = IntranetLive(INTRANET_BASE.rstrip("/") + "/live.html")
    from .chatbot import Crew
    from .live import DEFAULT_TCP, LiveCTD
    try:
        LIVE = LiveCTD(DEFAULT_TCP)
    except Exception as e:                  # noqa: BLE001 — a bad source setting must not stop the site
        log.warning("live CTD listener not started: %s", e)
        LIVE = LiveCTD("")
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
