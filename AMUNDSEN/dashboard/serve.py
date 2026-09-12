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
# the rooms, the messages, identity and the crew's part live in dashboard.chat
from . import chat as CHAT

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


# Raster tile pyramids are hundreds of thousands of small files; they are kept
# on local disk and served from here rather than written to the CIFS share.
TILES_DIR = Path(os.environ.get("UNDERWAY_TILES_DIR", "/data/gis/tiles"))
# the camera timelapses are built by their own job outside the web root
from .config import CAMERA_OUTPUT
from .nature import IMG_DIR as JOURNAL_IMG


class Handler(SimpleHTTPRequestHandler):
    # The basemap is GeoJSON, a megabyte a file. Served as application/geo+json
    # the front proxy leaves it uncompressed (its compression list does not
    # know the type); as application/json it goes out gzipped at a quarter the size.
    extensions_map = {**SimpleHTTPRequestHandler.extensions_map, ".geojson": "application/json", ".pbf": "application/x-protobuf"}

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

    def _bytes(self, code: int, ctype: str, body: bytes, cache: str = "private, max-age=86400") -> None:
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", cache)
        SimpleHTTPRequestHandler.end_headers(self)
        self.wfile.write(body)

    def _client(self) -> str:
        return self.headers.get("X-Forwarded-For", "").split(",")[0].strip() or self.client_address[0]

    def do_GET(self):
        u = urlsplit(self.path)
        if u.path.startswith('/api/ice/'):
            from . import ice_store
            q=parse_qs(u.query)
            try:
                if u.path=='/api/ice/track':
                    start=float(q.get('start',[time.time()-86400])[0]);end=float(q.get('end',[time.time()])[0])
                    if not (0<=start<=end and end-start<=32*86400):raise ValueError('Choose a range of at most 32 days')
                    return self._json(200,ice_store.track(start,end))
                identifier=q.get('id',[''])[0]
                if u.path=='/api/ice/detail':return self._json(200,ice_store.detail(identifier) or {})
                if u.path=='/api/ice/image':return self._bytes(200,'image/jpeg',ice_store.photo_path(identifier,q.get('kind',['roi'])[0]).read_bytes())
                return self._json(404,{'error':'Unknown camera endpoint'})
            except ValueError as e:return self._json(400,{'error':str(e)})
            except (OSError,sqlite3.Error):return self._json(404,{'error':'Camera product unavailable'})
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
        if u.path == "/api/nature/share":
            # one folder of the ship's share for the Nature tab's photo browser: ?path=<relative to the share>;
            # without a path, where the browser opens (the newest leg's Pictures)
            from .photos import listing, start_path
            q = parse_qs(u.query)
            try:
                rel = q.get("path", [None])[0]
                start = start_path(Path(self.directory))
                out = listing(start if rel is None else rel)
                out["start"] = start
                return self._json(200, out)
            except ValueError as e:
                return self._json(404, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("share listing failed: %s", e)
                return self._json(503, {"error": "the share is not reachable right now"})
        if u.path == "/api/nature/share/thumb":
            # a small JPEG of one photograph on the share, for the browser's grid
            from .photos import thumb
            q = parse_qs(u.query)
            try:
                data = thumb(q.get("path", [""])[0])
            except ValueError as e:
                return self._json(404, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.info("thumbnail failed: %s", e)
                return self._json(503, {"error": "the photograph could not be read"})
            return self._bytes(200, "image/jpeg", data)
        if u.path == "/api/nature/import":
            # an import of photographs as it stands (?job=<id>), or the imports so far and the licences offered
            from .photos import LICENCES, job, jobs, public, watches_public
            q = parse_qs(u.query)
            jid = q.get("job", [""])[0]
            if jid:
                j = job(jid)
                return self._json(200, public(j)) if j else self._json(404, {"error": "no such import"})
            return self._json(200, {"jobs": jobs(), "licences": LICENCES, "watches": watches_public()})
        if u.path == "/api/nature/journal":
            # the ship's own observations of nature, newest first, for the Nature tab
            from .nature import entries
            try:
                return self._json(200, {"entries": entries()})
            except Exception as e:                       # noqa: BLE001
                log.warning("journal read failed: %s", e)
                return self._json(500, {"error": "the journal could not be read"})
        if u.path == "/api/history/flags":
            # the artifacts anyone has flagged for review, so every browser
            # shows the same flags; with this browser's chat token and name,
            # which of them are its own and whether it is an admin's
            from .alerts import flagged
            q = parse_qs(u.query)
            return self._json(200, flagged(q.get("token", [""])[0][:64], q.get("name", [""])[0][:60]))
        if u.path == "/api/chat":
            q = parse_qs(u.query)
            g = lambda k, d="": (q.get(k, [d])[0] or d)             # noqa: E731
            try:
                since = int(g("since", "0"))
            except ValueError:
                since = 0
            try:
                return self._json(200, CHAT.read(since, g("name")[:CHAT.NAME_MAX], g("token")[:64], g("emoji"),
                                                 g("leave") == "1", g("channel", "ship")[:80]))
            except Exception as e:                       # noqa: BLE001
                log.warning("chat read failed: %s", e)
                return self._json(500, {"error": "chat unavailable"})
        return super().do_GET()

    def do_POST(self):
        u = urlsplit(self.path)
        if u.path in ('/api/nature/upload', '/api/nature/upload/file'):
            from . import photo_upload
            try:
                # No cross-site form uploads; custom header also forces a CORS
                # preflight from unrelated websites (we grant no CORS access).
                if self.headers.get('X-Photo-Upload') != '1' or self.headers.get('Sec-Fetch-Site') == 'cross-site':
                    raise ValueError('Use the photo upload form on this site')
                origin = self.headers.get('Origin')
                if origin and urlsplit(origin).netloc != self.headers.get('Host'):
                    raise ValueError('Upload origin does not match this site')
                if self.headers.get('Transfer-Encoding'):
                    raise ValueError('Upload requires a content length')
                n = int(self.headers.get('Content-Length', '0'))
                self.connection.settimeout(90)
                if u.path.endswith('/file'):
                    q = parse_qs(u.query)
                    result = photo_upload.receive(self.headers.get('X-Upload-ID', ''), int(q.get('index', ['-1'])[0]), self.rfile, n)
                else:
                    if not 0 < n <= 128 * 1024:
                        raise ValueError('Invalid upload batch size')
                    result = photo_upload.create(json.loads(self.rfile.read(n)))
                return self._json(200, result)
            except (ValueError, UnicodeDecodeError) as e:
                self.close_connection = True
                return self._json(400, {'error': str(e)})
            except OSError:
                self.close_connection = True
                return self._json(503, {'error': 'Could not finish writing to the share. Keep this page open and retry.'})
            except Exception:
                self.close_connection = True
                log.exception('photo upload failed')
                return self._json(500, {'error': 'Upload failed; keep this page open and retry.'})
        if u.path == "/api/feedback":
            from .feedback import submit
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 32768:
                    raise ValueError("Feedback request must be at most 32768 bytes")
                payload = json.loads(self.rfile.read(n))
                return self._json(200, submit(payload))
            except (ValueError, UnicodeDecodeError) as e:
                return self._json(400, {"error": str(e)})
            except Exception:
                log.exception("feedback save failed")
                return self._json(500, {"error": "Could not save feedback. Please try again."})
        if u.path == "/api/live" and LIVE:
            # point the listener at Seasave from the page: {"tcp": "10.0.0.22:49161,49162"}, the ports tried in turn
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
        if u.path == "/api/nature/journal":
            # one line of the ship's journal, from the Nature tab's form: the observation's
            # fields in the CLI's vocabulary, with an optional photograph as a data URL
            from .nature import Refused, append
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 14 * 1024 * 1024:
                    raise Refused("Request too large")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise Refused("Bad request")
                who = str(payload.get("name", ""))[:60]
                return self._json(200, {"ok": True, "entry": append(payload, who)})
            except (Refused, ValueError) as e:
                return self._json(400, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("journal write failed: %s", e)
                return self._json(500, {"error": "the journal could not be written"})
        if u.path == "/api/nature/import":
            # a folder of photographs from the share into the journal: {"folder": <path>, "name", "org", "email", "licence",
            # "clock", "watch": true to keep importing from it}; the import runs on in a thread and the page follows it by its job id
            from .photos import start
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 256 * 1024:
                    raise ValueError("Request too large")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise ValueError("Bad request")
                return self._json(200, {"ok": True, "job": start(Path(self.directory), payload, str(payload.get("name", ""))[:60])})
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("photo import failed to start: %s", e)
                return self._json(500, {"error": "the import could not be started"})
        if u.path == "/api/nature/watch":
            # stop watching a folder: {"path": <the folder>, "stop": true}
            from .photos import watch_remove, watches_public
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 4096:
                    raise ValueError("Request too large")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict) or not payload.get("stop"):
                    raise ValueError("Bad request")
                gone = watch_remove(str(payload.get("path", ""))[:400])
                return self._json(200, {"ok": True, "stopped": gone, "watches": watches_public()})
            except ValueError as e:
                return self._json(400, {"error": str(e)})
        if u.path == "/api/history/flag":
            # the flag on an artifact's card: {"id": ..., "on": true, "token": <chat token>, "name": ...,
            # "title": ..., "page": "artifact/...", "note": ...}; the alerts timer tells the keeper
            from .alerts import TooMany, set_flag
            try:
                n = int(self.headers.get("Content-Length", "0"))
                if not 0 < n <= 4096:
                    raise ValueError("Request too large")
                payload = json.loads(self.rfile.read(n) or b"{}")
                if not isinstance(payload, dict):
                    raise ValueError("Bad request")
                r = set_flag(str(payload.get("id", "")), bool(payload.get("on")), str(payload.get("token", ""))[:64],
                             str(payload.get("name", ""))[:60], str(payload.get("title", "")), str(payload.get("page", "")),
                             str(payload.get("note", "")))
                return self._json(200, {"ok": True, **r})
            except ValueError as e:
                return self._json(400, {"error": str(e)})
            except PermissionError as e:
                return self._json(403, {"error": str(e)})
            except TooMany as e:
                return self._json(429, {"error": str(e)})
            except Exception as e:                       # noqa: BLE001
                log.warning("history flag failed: %s", e)
                return self._json(500, {"error": "could not save the flag"})
        if u.path == "/api/history/requests":
            # the ship's copy of the history database is a pulled snapshot;
            # answers are written on grid, where the research crew works
            return self._json(403, {"error": "the ship's history database is read-only; answer requests on grid with history-db.py answer"})
        if u.path in ("/api/chat", "/api/chat/clear", "/api/chat/release"):
            try:
                n = min(int(self.headers.get("Content-Length", "0")), 4096)
                payload = json.loads(self.rfile.read(n) or b"{}")
                name, token = str(payload.get("name", ""))[:CHAT.NAME_MAX], str(payload.get("token", ""))[:64]
                channel = str(payload.get("channel", "ship"))[:80]
                if u.path == "/api/chat/clear":
                    r = CHAT.clear(channel, name, token)
                elif u.path == "/api/chat/release":
                    c = CHAT.conn()
                    try:
                        r = {"ok": CHAT.release(c, name, token)}
                    finally:
                        c.close()
                else:
                    r = CHAT.post(self._client(), name, str(payload.get("text", "")), str(payload.get("emoji", "")),
                                  channel, token, str(payload.get("slug", ""))[:200])
            except Exception as e:                           # noqa: BLE001
                log.warning("chat post failed: %s", e)
                r = {"error": "bad request"}
            return self._json(200 if r.get("ok") else 400, r)
        return self._json(404, {"error": "not found"})

    def translate_path(self, path):
        p = unquote(urlsplit(path).path)
        for prefix, base in (("/static/tiles/", TILES_DIR), ("/camera/", CAMERA_OUTPUT), ("/journal/", JOURNAL_IMG)):   # read at call time: tests swap the roots
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
        elif p.startswith(("/static/geo/", "/static/tiles/")) or p.endswith(("plotly.min.js", "maplibre-gl.js", "maplibre-gl.css")):
            self.send_header("Cache-Control", "public, max-age=604800")   # big, rarely change, versioned URL
        else:
            self.send_header("Cache-Control", "no-cache")
        super().end_headers()


# ---------------------------------------------------------------- the historian
_ask_lock = threading.Lock()


def history_ask(root: Path, question: str, slug: str = "") -> dict:
    """The historian's room: the wiki pages that bear on the question go in
    front of the model, and the answer comes back with the pages it was given."""
    from .chatbot import _num, complete, excerpt_block, history_lines, places_named, wiki_excerpts
    from .chatbot import wiki_pages
    if not wiki_pages(root):
        raise ValueError("no history has been published yet")
    excerpts = wiki_excerpts(root, question, slug)          # may be empty: then the ship context is all there is
    # the ship's own situation: where it is, what the history holds nearby,
    # and any named place the question mentions, with its position
    ship = []
    try:
        m = json.loads((root / "data" / "manifest.json").read_text())
        lat = m.get("latest", {}).get("lat"); lon = m.get("latest", {}).get("lon")
        end = (m.get("data_range", {}).get("end") or "")[:16].replace("T", " ")
        if lat is not None:
            ship.append(f"The ship is now at {_num(lat, 3)}, {_num(lon, 3)} (as of {end} UTC); the date today is "
                        f"{datetime.now(timezone.utc).strftime('%Y-%m-%d')}.")
        ship += history_lines(root, lat, lon)
    except Exception:                                # noqa: BLE001
        pass
    named = places_named(root, question)
    if named:
        ship.append("Named places the question mentions, from the ship's gazetteer: " + "; ".join(
            f"{p['name']}" + (f" ({', '.join(x for x in (p.get('inuktitut'), p.get('historic')) if x and x != p['name'])})" if (p.get('inuktitut') or p.get('historic')) else "")
            + (f" at {p['lat']:.3f}, {p['lon']:.3f}" if p.get("lat") is not None else "") + (f", {p['kind']}" if p.get("kind") else "")
            + (f": {p['note'][:200]}" if p.get("note") else "") for p in named) + ".")
    system = ("You are the historian aboard the research icebreaker CCGS Amundsen, answering scientists' questions about the "
              "history of the Canadian Arctic Archipelago and Baffin Bay. Answer from the wiki excerpts below, which were "
              "written by the ship's research crew from primary sources; when nothing you have covers something, say so "
              "plainly, as yourself, and then give your best general knowledge, marked as such. Speak of the sources by "
              "name, never of 'the wiki' or 'the excerpts' as if they were a person. Be concrete: dates, names, places, "
              "coordinates when the excerpts give them. Use Inuit names for people and places as the excerpts do. Where "
              "the record is disputed or rests on testimony, say whose. Cite the pages you draw on inline by their title in "
              "square brackets, like [The death march]. Plain prose, short paragraphs, no headings, no bullet lists unless "
              "listing dates. At most about 350 words.\n\nSHIP\n" + ("\n".join(ship) or "The ship's position is not known to this build.")
              + "\n\nWIKI EXCERPTS\n\n" + (excerpt_block(excerpts) or "(no page in the wiki bears on this question)"))
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
    global CREW, LIVE, INTRANET
    from .config import INTRANET_BASE
    INTRANET = IntranetLive(INTRANET_BASE.rstrip("/") + "/live.html")
    from .chatbot import Crew
    from .live import DEFAULT_TCP, LiveCTD
    try:
        LIVE = LiveCTD(DEFAULT_TCP)
    except Exception as e:                  # noqa: BLE001 — a bad source setting must not stop the site
        log.warning("live CTD listener not started: %s", e)
        LIVE = LiveCTD("")
    CREW = Crew(root)
    CHAT.CREW = CREW
    CHAT.ROOT = root
    CREW.start()
    from .photos import start_watcher
    start_watcher(root)                     # the watched folders of the share, looked at every ten minutes
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
