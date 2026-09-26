"""Logbook photographs to tables, through a vision model on OpenRouter.

A participant photographs a paper logbook page; the model returns every
table on it as an importable table (one row per record, ditto marks and
arrows expanded, multi-line records joined), each cell with the model's
qualitative log-likelihood that it is right (log10 odds, from 3 "near
certain" to -1 "a guess"), covering both its reading and its placement, plus any notes
written outside the tables. The layout may change; the values may not: each
is copied as written, never corrected or inferred, and an illegible one is the
model's best reading with a low confidence.

The key is ``OPENROUTER_REPORT_KEY``, from the environment or, failing that,
from ``~/.config/underway/underway.env`` (only that one variable is read).
"""

from __future__ import annotations

import base64
import io
import json
import os
import re
import time
import urllib.error
import urllib.request
from pathlib import Path

from PIL import Image, ImageOps

API = "https://openrouter.ai/api/v1/chat/completions"
ENV_FILE = Path.home() / ".config/underway/underway.env"
# Gemini 3.8 Flash reads handwritten logbook pages (a dense beta-counter log,
# a hand-ruled eDNA log, pages upside down) nearly cell for cell, and sizes
# its own reasoning to the page: about 1.5k tokens (US$0.02) for a simple
# page, 20k (US$0.09) for a dense one. Capped reasoning cuts dense pages
# short, so none is set. Its safety filter sometimes stops a page partway;
# such a page goes to the fallback, which is cheaper and less exact.
MODEL = os.environ.get("CRUISE_DIGITIZE_MODEL", "google/gemini-3.8-flash")
FALLBACK = os.environ.get("CRUISE_DIGITIZE_FALLBACK", "openai/gpt-6-luna-pro")
EFFORT = os.environ.get("CRUISE_DIGITIZE_EFFORT") or None     # the model's reasoning effort
MAX_SIDE = 2400          # px on the long side: handwriting stays legible, tokens stay bounded

PROMPT = """This is a photograph of a logbook page. Turn it into tables that can be imported into a
spreadsheet or database as they are. The goal is a clean, importable table, not a line-by-line
copy of the page, so reformat freely where that makes the data importable:

- One header row, then one row per record, one value per cell.
- A record written across two or more lines becomes one row (add columns as needed).
- A cell holding two different fields (a value and its uncertainty in separate places, a date
  and a time) may be split into two columns; values that belong together stay together.
- Ditto marks, arrows and lines drawn down a column mean "same as above": write the repeated
  value in each row it covers.
- Notes written beside or between rows go in a notes column on the row they refer to.
- Crossed-out entries are left out.
- Put units in the header when they are written once for a column.
- Separate tables on the page (different headings or columns) are separate tables.
- The page may be rotated or photographed at an angle; read it in its written orientation.
- Text that belongs to no table (title, page number, signatures, general remarks) goes in "notes".

What must not change: the characters of each value. Copy numbers, codes, names and units exactly
as written; do not correct, round, complete or infer them. An empty cell is "". An illegible
value gets your best reading and a low confidence.

"c" is your estimate of how likely the cell is exactly right for that row and column (both the
reading and the placement; a value carried down from a ditto mark or arrow counts as placed),
as a qualitative log-likelihood, one step per factor of ten in the odds that it is right:
  near-certain  wrong less than 1 time in 1000 (clear print, unambiguous)
  very-likely   wrong about 1 time in 100 (clear handwriting)
  likely        wrong about 1 time in 10 (legible but a character could be another; placement inferred)
  even          even odds: a real doubt between two readings or two places
  guess         more likely wrong than right: a guess at an illegible value
Judge each cell on its own evidence; do not give a whole table one level.
"""

# Confidence is a qualitative log-likelihood: log10 of the odds a cell is right.
LEVELS = {3: "near certain", 2: "very likely", 1: "likely", 0: "even odds", -1: "a guess"}
# Sent as words: structured output handles string enums, not negative integer ones.
WORDS = {"near-certain": 3, "very-likely": 2, "likely": 1, "even": 0, "guess": -1}
LEVEL = {"type": "string", "enum": list(WORDS)}
EDITED = 3          # a participant's correction is taken as certain

SCHEMA = {
    "name": "logbook",
    "strict": True,
    "schema": {
        "type": "object",
        "additionalProperties": False,
        "required": ["tables", "notes"],
        "properties": {
            "tables": {"type": "array", "items": {
                "type": "object", "additionalProperties": False,
                "required": ["title", "columns", "rows"],
                "properties": {
                    "title": {"type": "string"},
                    "columns": {"type": "array", "items": {"type": "string"}},
                    "rows": {"type": "array", "items": {"type": "array", "items": {
                        "type": "object", "additionalProperties": False,
                        "required": ["t", "c"],
                        "properties": {"t": {"type": "string"}, "c": LEVEL}}}},
                }}},
            "notes": {"type": "array", "items": {
                "type": "object", "additionalProperties": False, "required": ["t", "c"],
                "properties": {"t": {"type": "string"}, "c": LEVEL}}},
        },
    },
}


class Refused(RuntimeError):
    """The model stopped before finishing (a content filter, or out of tokens)."""


def api_key() -> str:
    key = os.environ.get("OPENROUTER_REPORT_KEY")
    if key:
        return key.strip()
    if ENV_FILE.is_file():
        for line in ENV_FILE.read_text().splitlines():
            m = re.match(r"^\s*(?:export\s+)?OPENROUTER_REPORT_KEY\s*=\s*(.*)$", line)
            if m:
                return m[1].strip().strip("'\"")
    raise RuntimeError("OPENROUTER_REPORT_KEY is not set")


def prepare(raw: bytes, rotate: int = 0) -> bytes:
    """Upright per EXIF, turned by ``rotate`` degrees clockwise, bounded, as JPEG."""
    im = ImageOps.exif_transpose(Image.open(io.BytesIO(raw))).convert("RGB")
    if rotate % 360:
        im = im.rotate(-rotate, expand=True)
    im.thumbnail((MAX_SIDE, MAX_SIDE), Image.LANCZOS)
    buf = io.BytesIO()
    im.save(buf, "JPEG", quality=88)
    return buf.getvalue()


def transcribe(jpeg: bytes, model: str = MODEL, timeout: int = 600,
               effort: str | None = EFFORT, reasoning: dict | None = None) -> dict:
    """{'tables', 'notes', 'model', 'usage', 'seconds'} for one prepared image."""
    body = {
        "model": model,
        "messages": [{"role": "user", "content": [
            {"type": "text", "text": PROMPT},
            {"type": "image_url", "image_url": {
                "url": "data:image/jpeg;base64," + base64.b64encode(jpeg).decode()}},
        ]}],
        "response_format": {"type": "json_schema", "json_schema": SCHEMA},
        "temperature": 0,
        "usage": {"include": True},
    }
    if reasoning is not None:
        body["reasoning"] = reasoning
    elif effort:
        body["reasoning"] = {"effort": effort}
    req = urllib.request.Request(API, data=json.dumps(body).encode(), headers={
        "Authorization": f"Bearer {api_key()}", "Content-Type": "application/json",
        "X-Title": "Amundsen cruise report builder"})
    t0 = time.monotonic()
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            out = json.loads(r.read())
    except urllib.error.HTTPError as e:
        detail = e.read().decode(errors="replace")[:400]
        raise RuntimeError(f"OpenRouter {e.code}: {detail}") from None
    if "error" in out:
        raise RuntimeError(f"OpenRouter: {out['error']}")
    choice = out["choices"][0]
    if choice.get("finish_reason") in ("content_filter", "length"):
        raise Refused(f"{model} stopped: {choice.get('native_finish_reason') or choice.get('finish_reason')}")
    text = choice["message"]["content"] or ""
    text = re.sub(r"^```(?:json)?\s*|\s*```$", "", text.strip())
    data = json.loads(text)
    cells = [c for t in data.get("tables", []) for r in t.get("rows", []) for c in r]
    if not any(str(c.get("t", "")).strip() for c in cells):
        raise Refused(f"{model} returned no cell text")
    data["model"] = out.get("model", model)
    data["usage"] = out.get("usage", {})
    data["seconds"] = round(time.monotonic() - t0, 1)
    return data


def transcribe_page(jpeg: bytes) -> dict:
    """The main model, then the fallback if it refuses or fails; says which answered."""
    try:
        return transcribe(jpeg, MODEL)
    except (Refused, RuntimeError, ValueError) as first:
        data = transcribe(jpeg, FALLBACK)
        data["fallback_reason"] = str(first)[:200]
        return data


# --- stored digitizations ----------------------------------------------------------
#
# Each photo is kept under STATE_DIR/digitized/<id>.jpg with its answer in
# <id>.json. A participant's correction sets the cell's text, marks it edited
# and sets its confidence to 100: it is then the participant's reading, not
# the model's.

import colorsys  # noqa: E402
import uuid  # noqa: E402

from openpyxl import Workbook  # noqa: E402
from openpyxl.styles import Alignment, Font, PatternFill  # noqa: E402
from openpyxl.utils import get_column_letter  # noqa: E402

from .config import STATE_DIR  # noqa: E402
from .logsheets import HAND_COLUMN  # noqa: E402

DITTO = re.compile(r'^\s*(["“”″〃=|↓⇓⬇]+|do\.?|same|ditto|\^+|v)\s*$', re.I)


def _dir() -> Path:
    d = STATE_DIR / "digitized"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _path(ident: str, ext: str) -> Path:
    if not re.fullmatch(r"[0-9a-f]{12}", ident):
        raise ValueError("bad digitization id")
    return _dir() / f"{ident}.{ext}"


def save(name: str, jpeg: bytes, result: dict, ident: str | None = None) -> dict:
    """Store a transcription; with ``ident``, replace that page's (same photo)."""
    if ident is None:
        ident = uuid.uuid4().hex[:12]
        _path(ident, "jpg").write_bytes(jpeg)
    else:
        _path(ident, "json")                      # validates the id
    doc = {"id": ident, "name": re.sub(r"[^\w.\- ]+", "_", Path(name).name)[:120],
           "created": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()), **result}
    _levels(doc)
    for t in doc["tables"]:
        width = max([len(t["columns"])] + [len(r) for r in t["rows"]])
        t["columns"] = (t["columns"] + [""] * width)[:width]
        t["rows"] = [(r + [{"t": "", "c": EDITED}] * width)[:width] for r in t["rows"]]
    _levels(doc)
    _path(ident, "json").write_text(json.dumps(doc, ensure_ascii=False))
    return doc


def level(c) -> int:
    """A confidence as a level (log10 odds): the model's word, a stored level, or a
    percentage from an earlier transcription."""
    if c is None:
        return EDITED
    if isinstance(c, str):
        if c in WORDS:
            return WORDS[c]
        try:
            c = int(c)
        except ValueError:
            return 0
    c = int(c)
    if -1 <= c <= 3:
        return c
    return 3 if c >= 97 else 2 if c >= 88 else 1 if c >= 70 else 0 if c >= 45 else -1


def _levels(doc: dict) -> dict:
    """Every cell as {"t": text, "c": level}, whatever the model left out."""
    for t in doc.get("tables", []):
        t.setdefault("columns", [])
        t["rows"] = [[c if isinstance(c, dict) else {"t": str(c)} for c in r] for r in t.get("rows", [])]
        for r in t["rows"]:
            for c in r:
                c["t"] = str(c.get("t") or "")
                c["c"] = level(c.get("c"))
    for n in doc.get("notes", []):
        n["c"] = level(n.get("c"))
    return doc


def load(ident: str) -> dict:
    return _levels(json.loads(_path(ident, "json").read_text()))


def image(ident: str) -> bytes:
    return _path(ident, "jpg").read_bytes()


def edit(ident: str, table: int, row: int, col: int, text: str) -> dict:
    """A participant's correction of one cell (row -1 is the header)."""
    doc = load(ident)
    t = doc["tables"][table]
    if row < 0:
        t["columns"][col] = text
    else:
        t["rows"][row][col] = {"t": text, "c": EDITED, "edited": True}
    _path(ident, "json").write_text(json.dumps(doc, ensure_ascii=False))
    return doc


HUES = {3: 120, 2: 90, 1: 58, 0: 30, -1: 0}


def set_match(ident: str, table: int, row: int, op_key: str | None) -> dict:
    """A participant's own match for one row (None clears it)."""
    doc = load(ident)
    manual = doc["tables"][table].setdefault("manual", {})
    if op_key:
        manual[str(row)] = op_key
    else:
        manual.pop(str(row), None)
    _path(ident, "json").write_text(json.dumps(doc, ensure_ascii=False))
    return doc


def colour(c: int | None, scheme: str = "light") -> str:
    """The level's fill, green (near certain) through yellow (likely) to red (a guess)."""
    hue = HUES[level(c)]
    light = 0.84 if scheme == "light" else 0.30
    sat = 0.75 if scheme == "light" else 0.45
    r, g, b = colorsys.hls_to_rgb(hue / 360, light, sat)
    return f"{round(r * 255):02X}{round(g * 255):02X}{round(b * 255):02X}"


def tsv(ident: str, table: int) -> str:
    t = load(ident)["tables"][table]
    clean = lambda s: re.sub(r"[\t\r\n]+", " ", s or "")  # noqa: E731
    lines = ["\t".join(clean(c) for c in t["columns"])]
    lines += ["\t".join(clean(c["t"]) for c in r) for r in t["rows"]]
    return "\n".join(lines) + "\n"


def _sheet_title(title: str, used: set[str]) -> str:
    base = re.sub(r"[\[\]:*?/\\]", " ", title or "Table").strip()[:28] or "Table"
    name, n = base, 2
    while name.lower() in used:
        name, n = f"{base[:25]} {n}", n + 1
    used.add(name.lower())
    return name


def xlsx(idents: list[str]) -> bytes:
    """Every table of the given pages as its own sheet, confidence as cell fill;
    a Notes sheet with the text outside the tables and a Legend."""
    wb = Workbook()
    wb.remove(wb.active)
    used: set[str] = set()
    notes = []
    bold = Font(bold=True)
    for ident in idents:
        doc = load(ident)
        for k, t in enumerate(doc["tables"]):
            ws = wb.create_sheet(_sheet_title(t.get("title") or f"{doc['name']} {k + 1}", used))
            ws.append(t["columns"])
            for cell in ws[1]:
                cell.font = bold
            for r in t["rows"]:
                ws.append([c["t"] for c in r])
                for j, c in enumerate(r, start=1):
                    ws.cell(ws.max_row, j).fill = PatternFill("solid", fgColor=colour(c["c"]))
            for j in range(1, len(t["columns"]) + 1):
                width = max(len(str(ws.cell(i, j).value or "")) for i in range(1, ws.max_row + 1))
                ws.column_dimensions[get_column_letter(j)].width = min(40, max(8, width + 2))
            ws.freeze_panes = "A2"
        notes += [(doc["name"], n["t"], n["c"]) for n in doc.get("notes", [])]
    ws = wb.create_sheet(_sheet_title("Notes", used))
    ws.append(["Page", "Note"])
    for cell in ws[1]:
        cell.font = bold
    for page, text, c in notes:
        ws.append([page, text])
        ws.cell(ws.max_row, 2).fill = PatternFill("solid", fgColor=colour(c))
        ws.cell(ws.max_row, 2).alignment = Alignment(wrap_text=True)
    ws.column_dimensions["A"].width, ws.column_dimensions["B"].width = 30, 90
    ws = wb.create_sheet(_sheet_title("Legend", used))
    ws.append(["Cell colour = the model's estimate that the cell is exactly right (reading and "
               "placement), as log10 odds"])
    ws.append(["Level", "Meaning", "Wrong about"])
    for c, wrong in zip(LEVELS, ("< 1 in 1000", "1 in 100", "1 in 10", "1 in 2", "more often than not")):
        ws.append([c, LEVELS[c], wrong])
        for j in (1, 2, 3):
            ws.cell(ws.max_row, j).fill = PatternFill("solid", fgColor=colour(c))
    ws.append(["Cells a participant corrected are shown as level 3 (near certain)."])
    buf = io.BytesIO()
    wb.save(buf)
    return buf.getvalue()


def rows_for_logsheet(ident: str, table: int, fill_down: bool) -> tuple[list[str], list[dict]]:
    """A table as logsheet columns and rows.

    With ``fill_down``, a ditto mark or arrow in any column takes the value
    above, and so do blank identifying cells (event label, station, cast,
    date) in a row that continues the record above, one whose identifying
    cells are all blank or ditto: logbooks draw a line down those columns
    rather than repeat them. A row that names any identifier of its own
    ("CONTROL") starts afresh. Nothing is filled otherwise.
    """
    from .logsheets import NAME_HINTS

    t = load(ident)["tables"][table]
    cols = [c or f"column {j + 1}" for j, c in enumerate(t["columns"])]
    seen: dict[str, int] = {}
    uniq = []
    for c in cols:
        seen[c] = seen.get(c, 0) + 1
        uniq.append(c if seen[c] == 1 else f"{c} ({seen[c]})")
    ident_rx = [re.compile(p, re.I) for role, p in NAME_HINTS
                if role in ("label", "station", "cast", "datetime", "date")]
    carry = [any(rx.search(c) for rx in ident_rx) for c in uniq]
    manual = t.get("manual") or {}
    out, prev = [], [""] * len(uniq)
    for i, r in enumerate(t["rows"]):
        continues = all(not r[j]["t"].strip() or DITTO.match(r[j]["t"])
                        for j in range(len(uniq)) if carry[j])
        vals = []
        for j, c in enumerate(r):
            v = c["t"]
            if fill_down and (DITTO.match(v) or (carry[j] and continues and not v.strip())):
                v = prev[j]
            vals.append(v)
        prev = vals
        row = dict(zip(uniq, vals))
        if manual:
            row[HAND_COLUMN] = manual.get(str(i), "")
        out.append(row)
    if manual:
        uniq = uniq + [HAND_COLUMN]
    return uniq, out
