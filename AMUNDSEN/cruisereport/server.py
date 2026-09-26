"""The report page and its JSON API, on the standard library's HTTP server.

    python -m cruisereport serve --port 8044      (from the AMUNDSEN directory)

Routes (all JSON unless noted):

    GET  /report.html                      the page
    GET  /api/legs                         legs with an event log
    GET  /api/leg?leg=<leg>                instruments, operations, rosette teams, columns
    POST /api/logsheet?leg=<leg>           raw file body, X-Filename header: parse and guess
    POST /api/logsheet/match               {id, sheet, roles, leg, groups}: rows matched to operations
    POST /api/conditions                   {report}: narrative preview and word count
    POST /api/table                        {report, index}: one table, formatted, first rows
    POST /api/figure                       {report, spec}: {images: [PNG data URLs]}
    POST /api/docx                         {report}: the .docx
    POST /api/digitize?rotate=<deg>        raw photo body, X-Filename: queue a logbook page
    GET  /api/digitized/<id>               a transcription;  /image the photo
    POST /api/digitized/<id>/edit          {table, row, col, text}: a participant's correction
    GET  /api/digitized/<id>/<table>.tsv   one table as TSV
    GET  /api/digitized.xlsx?ids=a,b       every table as a sheet, confidence as fill
    POST /api/digitized/<id>/logsheet      {table, fill_down}: use a table as a logsheet
    POST /api/digitized/<id>/setmatch      {table, row, op}: match a row by hand (op null clears)
    POST /api/digitized/<id>/again         queue the stored photo again
    POST /api/digitized/<id>/match         {table, fill_down, leg, groups}: row -> operation, now
    GET  /api/drafts                       saved drafts
    GET  /api/draft/<name>                 one draft
    PUT  /api/draft/<name>                 save a draft

It reads the shares and the underway stores and writes only under
``STATE_DIR``. There is no authentication: it is meant for the ship's LAN,
like the underway dashboard it will sit beside.
"""

from __future__ import annotations

import base64
import json
import logging
import re
import traceback
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, quote, urlparse

import pandas as pd

from . import activities, digitize, eventlog, logsheets, report, rosette, tables, underway_panels
from .config import STATE_DIR, WORD_LIMIT

log = logging.getLogger(__name__)
STATIC = Path(__file__).resolve().parent / "static"
MAX_UPLOAD = 25 * 1024 * 1024
PREVIEW_ROWS = 60


def _drafts() -> Path:
    d = STATE_DIR / "drafts"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _slug(name: str) -> str:
    s = re.sub(r"[^\w\-]+", "-", name.strip()).strip("-")[:80]
    if not s:
        raise ValueError("a draft needs a name")
    return s


def leg_info(leg: str) -> dict:
    ops = [o.summary() for o in eventlog.operations(leg) if o.group != "void"]
    counts: dict[str, int] = {}
    for o in ops:
        counts[o["group"]] = counts.get(o["group"], 0) + 1
    order = activities.DISPLAY_ORDER
    return {
        "leg": leg,
        "groups": [{"id": g, "label": activities.LABELS[g], "count": counts[g]}
                   for g in order if g in counts],
        "operations": ops,
        "teams": rosette.teams(leg),
        "columns": tables.catalog(leg, [], []),
        "figures": [{"kind": k, "caption": v} for k, v in report.FIGURE_CAPTIONS.items()],
        "underway_panels": underway_panels.catalog(),
        "underway_default": underway_panels.DEFAULT,
        "word_limit": WORD_LIMIT,
    }


class Handler(SimpleHTTPRequestHandler):
    server_version = "cruisereport/0.1"

    def __init__(self, *a, **kw):
        super().__init__(*a, directory=str(STATIC), **kw)

    def end_headers(self):
        # The page's own files change with every deploy: browsers revalidate
        # them (a 304 when unchanged) instead of running a stale script.
        if not any(h.startswith(b"Cache-Control") for h in getattr(self, "_headers_buffer", [])):
            self.send_header("Cache-Control", "no-cache")
        super().end_headers()

    def log_message(self, fmt, *args):
        log.info("%s %s", self.address_string(), fmt % args)

    def _send(self, code: int, ctype: str, body: bytes, headers: dict | None = None):
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        for k, v in (headers or {}).items():
            self.send_header(k, v)
        self.end_headers()
        self.wfile.write(body)

    def _json(self, code: int, payload) -> None:
        self._send(code, "application/json; charset=utf-8",
                   json.dumps(payload, default=str, allow_nan=False).encode())

    def _body(self) -> bytes:
        n = int(self.headers.get("Content-Length") or 0)
        if n > MAX_UPLOAD:
            raise ValueError(f"upload larger than {MAX_UPLOAD // 2**20} MB")
        return self.rfile.read(n) if n else b""

    def _obj(self) -> dict:
        return json.loads(self._body() or b"{}")

    def _guard(self, fn):
        try:
            fn()
        except (BrokenPipeError, ConnectionResetError):
            pass                                    # the browser left (reload, closed tab)
        except (ValueError, KeyError, FileNotFoundError) as e:
            self._json(400, {"error": str(e)})
        except Exception as e:                      # the page shows it; the log keeps the trace
            log.error("%s\n%s", e, traceback.format_exc())
            self._json(500, {"error": f"{type(e).__name__}: {e}"})

    # --- GET -----------------------------------------------------------------
    def do_GET(self):
        u = urlparse(self.path)
        q = {k: v[0] for k, v in parse_qs(u.query).items()}
        if u.path in ("/", "/report", "/report.html"):
            self.path = "/report.html"
            return super().do_GET()
        if u.path == "/api/legs":
            return self._guard(lambda: self._json(200, {"legs": eventlog.legs()}))
        if u.path == "/api/leg":
            return self._guard(lambda: self._json(200, _clean(leg_info(q["leg"]))))
        if u.path == "/api/drafts":
            return self._guard(lambda: self._json(200, {"drafts": sorted(
                ({"name": p.stem, "mtime": p.stat().st_mtime} for p in _drafts().glob("*.json")),
                key=lambda d: -d["mtime"])}))
        m = re.fullmatch(r"/api/digitized/([0-9a-f]{12})", u.path)
        if m:
            return self._guard(lambda: self._json(200, digitize.load(m[1])))
        m = re.fullmatch(r"/api/digitized/([0-9a-f]{12})/image", u.path)
        if m:
            return self._guard(lambda: self._send(200, "image/jpeg", digitize.image(m[1]),
                                                  {"Cache-Control": "private, max-age=86400"}))
        m = re.fullmatch(r"/api/digitized/([0-9a-f]{12})/(\d+)\.tsv", u.path)
        if m:
            def tsv():
                doc = digitize.load(m[1])
                name = re.sub(r"[^\w\-]+", "_", doc["tables"][int(m[2])].get("title") or doc["name"])
                self._send(200, "text/tab-separated-values; charset=utf-8",
                           digitize.tsv(m[1], int(m[2])).encode(),
                           {"Content-Disposition": f'attachment; filename="{name}.tsv"'})
            return self._guard(tsv)
        if u.path == "/api/digitized.xlsx":
            ids = [i for i in q.get("ids", "").split(",") if re.fullmatch(r"[0-9a-f]{12}", i)]
            return self._guard(lambda: self._send(
                200, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                digitize.xlsx(ids), {"Content-Disposition": 'attachment; filename="logbook_transcription.xlsx"'}))
        m = re.fullmatch(r"/api/draft/([\w\-]+)", u.path)
        if m:
            return self._guard(lambda: self._json(
                200, json.loads((_drafts() / f"{_slug(m[1])}.json").read_text())))
        return super().do_GET()

    # --- POST / PUT ----------------------------------------------------------
    def do_POST(self):
        u = urlparse(self.path)
        q = {k: v[0] for k, v in parse_qs(u.query).items()}
        routes = {
            "/api/logsheet": lambda: self._upload(q),
            "/api/logsheet/match": self._match,
            "/api/conditions": self._conditions,
            "/api/table": self._table,
            "/api/figure": self._figure,
            "/api/docx": self._docx,
            "/api/digitize": lambda: self._digitize(q),
        }
        m = re.fullmatch(r"/api/digitized/([0-9a-f]{12})/(edit|logsheet|match|again|setmatch)", u.path)
        if m:
            fn = {"edit": self._dig_edit, "logsheet": self._dig_logsheet, "match": self._dig_match,
                  "again": self._dig_again, "setmatch": self._dig_setmatch}[m[2]]
            return self._guard(lambda: fn(m[1]))
        fn = routes.get(u.path)
        if fn is None:
            return self._json(404, {"error": "no such route"})
        self._guard(fn)

    def do_PUT(self):
        m = re.fullmatch(r"/api/draft/([\w\-]+)", urlparse(self.path).path)
        if not m:
            return self._json(404, {"error": "no such route"})

        def save():
            doc = self._obj()
            name = _slug(m[1])
            (_drafts() / f"{name}.json").write_text(json.dumps(doc, indent=1))
            self._json(200, {"saved": name})
        self._guard(save)

    def _digitize(self, q):
        name = self.headers.get("X-Filename") or "logbook.jpg"
        jpeg = digitize.prepare(self._body(), int(q.get("rotate", 0) or 0))
        self._json(200, digitize.submit(name, jpeg))

    def _dig_setmatch(self, ident):
        """Match one row by hand (op null clears it); logsheets from the table follow."""
        a = self._obj()
        k = int(a["table"])
        digitize.set_match(ident, k, int(a["row"]), a.get("op") or None)

        def frame(fill_down):
            cols, rows = digitize.rows_for_logsheet(ident, k, fill_down)
            return pd.DataFrame(rows, columns=cols)
        self._json(200, {"linked": logsheets.refresh_digitized(ident, k, frame)})

    def _dig_again(self, ident):
        """Queue the stored photo again; its tables (corrections too) are replaced when done."""
        self._json(200, digitize.requeue(ident))

    def _dig_edit(self, ident):
        """Save a correction, then rewrite the logsheets made from that table."""
        a = self._obj()
        k = int(a["table"])
        doc = digitize.edit(ident, k, int(a["row"]), int(a["col"]), str(a["text"]))
        def frame(fill_down):
            cols, rows = digitize.rows_for_logsheet(ident, k, fill_down)
            return pd.DataFrame(rows, columns=cols)
        self._json(200, {**doc, "linked": logsheets.refresh_digitized(ident, k, frame)})

    def _dig_match(self, ident):
        """Which operation each row of a digitized table matches, as it stands now."""
        a = self._obj()
        k = int(a.get("table", 0))
        cols, rows = digitize.rows_for_logsheet(ident, k, bool(a.get("fill_down", True)))
        roles = logsheets.guess_roles(pd.DataFrame([{c: v for c, v in r.items() if c != logsheets.HAND_COLUMN}
                                                    for r in rows]))
        matched = logsheets.match(rows, roles, a["leg"], a.get("groups"))
        ops = {o.key: o.summary() for o in eventlog.operations(a["leg"])}
        self._json(200, {"roles": roles, "rows": [
            {"op": r["_op"], "how": r["_how"],
             "label": " ".join(x for x in (ops.get(r["_op"], {}).get("station"),
                                           ops.get(r["_op"], {}).get("label")) if x) or None}
            for r in matched]})

    def _dig_logsheet(self, ident):
        a = self._obj()
        doc = digitize.load(ident)
        k = int(a.get("table", 0))
        cols, rows = digitize.rows_for_logsheet(ident, k, bool(a.get("fill_down", True)))
        df = pd.DataFrame(rows, columns=cols)
        title = doc["tables"][k].get("title") or f"table {k + 1}"
        name = f"{Path(doc['name']).stem} · {title}"
        meta = logsheets.save_frames(name, {"transcribed": df},
                                     {"digitized": ident, "table": k, "fill_down": bool(a.get("fill_down", True))})
        self._json(200, {"id": meta["id"], "name": meta["name"], "sheets": {
            s: {"columns": d["columns"], "roles": d["roles"], "n": len(d["rows"])}
            for s, d in meta["sheets"].items()}})

    def _upload(self, q):
        name = self.headers.get("X-Filename") or "upload.xlsx"
        meta = logsheets.save_upload(name, self._body())
        out = {"id": meta["id"], "name": meta["name"], "sheets": {
            s: {"columns": d["columns"], "roles": d["roles"], "n": len(d["rows"])}
            for s, d in meta["sheets"].items()}}
        self._json(200, out)

    def _match(self):
        a = self._obj()
        m = logsheets.matched(a["id"], a["sheet"], a.get("roles") or {}, a["leg"], a.get("groups"))
        m["rows"] = [{"op": r["_op"], "how": r["_how"],
                      "cells": {k: r.get(k) for k in m["columns"]}} for r in m["rows"]]
        m["role_options"] = logsheets.ROLES
        self._json(200, _clean(m))

    def _conditions(self):
        rep = self._obj()["report"]
        c = report.content({**rep, "tables": [], "figures": []})
        self._json(200, {"narratives": c["narratives"], "words": c["words"],
                         "word_limit": WORD_LIMIT})

    def _table(self):
        a = self._obj()
        rep, spec = a["report"], a["report"]["tables"][int(a["index"])]
        sel = rep.get("selection", {})
        logs = report._logs(rep)
        tab = tables.build(rep["leg"], spec, sel.get("ops", []), sel.get("teams", []), logs,
                           report.logged_bottles(rep, logs))
        body = [[tables.fmt(v, c) for v, c in zip(r, tab["columns"])]
                for r in tab["body"][:PREVIEW_ROWS]]
        self._json(200, {"columns": tab["columns"], "body": body, "total": len(tab["body"])})

    def _figure(self):
        a = self._obj()
        pngs = report.figure(a["report"], a["spec"])
        self._json(200, {"images": ["data:image/png;base64," + base64.b64encode(p).decode()
                                    for p in pngs]})

    def _docx(self):
        rep = self._obj()["report"]
        data = report.docx_bytes(rep)
        team = re.sub(r"[^\w\-]+", "_", (rep.get("team") or "TeamX").strip()) or "TeamX"
        fname = f"Cruise Report_{team}.docx"
        self._send(200, "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                   data, {"Content-Disposition":
                          f"attachment; filename=\"{fname}\"; filename*=UTF-8''{quote(fname)}"})


def _clean(obj):
    """NaN and infinity are not JSON; send them as null."""
    if isinstance(obj, float):
        return obj if obj == obj and abs(obj) != float("inf") else None
    if isinstance(obj, dict):
        return {k: _clean(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_clean(v) for v in obj]
    return obj


def serve(port: int = 8044, bind: str = "0.0.0.0") -> None:
    digitize.start()
    httpd = ThreadingHTTPServer((bind, port), Handler)
    log.info("cruise report on http://%s:%d/report.html", bind, port)
    httpd.serve_forever()

