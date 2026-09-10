"""The Nature tab's ship side: the journal.

The natural half of the history layer (subjects and observations) is written
on grid and published with the rest of the layer; the ship reads it. What
the ship writes is a journal: an append-only file of observation rows in the
CLI's own vocabulary, one JSON object per line, under ``db/history/_journal/``
in the ship's clone of the project, with the photographs beside it in
``_journal/img/``. The journal is rsynced up to grid (``history-sync.sh push-journal``, into
``_journal/incoming/``), where ``history-db.py ingest --journal`` turns each line into rows through the same
writers as everything else; that ingest is idempotent on ``id``, so a
corrected line with the same id replaces the row.

Most lines come in by the photo import (``photos.py``): a photograph from
the share, placed by its own time and position and captioned by the model,
becomes a line through ``append`` like any other; the form on the tab writes
one line by hand.

Ids carry the ``amundsen-<date>-<nnn>`` prefix, so they never collide with
research ids. A line carries only the writer's fields (``db/history/JOURNAL.md``
on grid is the contract); who wrote it goes to ``journal.log`` beside it.
Nothing here writes to the database or the repository.
"""

from __future__ import annotations

import base64
import fcntl
import json
import logging
import re
from datetime import datetime, timezone
from pathlib import Path

from .history import HISTORY_DIR

log = logging.getLogger(__name__)

JOURNAL_DIR = HISTORY_DIR / "_journal"
IMG_DIR = JOURNAL_DIR / "img"
FILE = JOURNAL_DIR / "journal.jsonl"
# the writer's vocabulary on grid (arctic_history.METHODS); db/history/JOURNAL.md there is the contract
METHODS = {"sighting", "hunt", "specimen", "transect", "aerial-survey", "camera", "acoustic", "edna", "catch-record", "testimony", "instrument", "sounding", "dredge", "trawl", "net", "trap", "core", "sample", "station-record", "survey", "satellite", "chart"}
ORIGINS = {"ship", "crew"}
IMAGE_TYPES = {"image/jpeg": ".jpg", "image/png": ".png", "image/webp": ".webp"}
IMAGE_MAX = 10 * 1024 * 1024
TEXT = {"count": 60, "unit": 30, "qualifier": 120, "instrument": 80, "observer": 80, "vessel": 60, "place": 120,
        "detail": 2000, "confidence": 20, "stage": 30, "sex": 20, "behaviour": 120, "licence": 40, "topic": 60, "bibkey": 60}
DATE_RX = re.compile(r"^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}(:\d{2})?Z?)?$")
ID_RX = re.compile(r"^amundsen-\d{4}-\d{2}-\d{2}-\d{3}$")
LINES_PER_DAY = 500


class Refused(ValueError):
    """A line the journal will not take; the message says what is missing."""


def _clean(entry: dict) -> dict:
    """The line as the CLI reads it: the fields it knows, checked the way
    the form asks for them. Grid's writer validates the rest (the subject
    against the published list, the bibkey, the rules of the brief)."""
    if not isinstance(entry, dict):
        raise Refused("an observation is a JSON object")
    out = {"kind": "observation"}
    subject = str(entry.get("subject", "")).strip()
    if not subject or len(subject) > 120:
        raise Refused("a subject is needed: what was seen, measured, sounded or collected")
    out["subject"] = subject
    date = str(entry.get("date", "")).strip()
    if not DATE_RX.match(date):
        raise Refused("the date is written 2026-09-11T14:22Z (UTC), or 2026-09-11 for a day")
    out["date"] = date
    try:
        lat, lon = float(entry.get("lat")), float(entry.get("lon"))
    except (TypeError, ValueError):
        raise Refused("a position is needed: latitude and longitude in decimal degrees") from None
    if not (-90 <= lat <= 90 and -180 <= lon <= 180):
        raise Refused("the position is off the globe")
    out["lat"], out["lon"] = round(lat, 5), round(lon, 5)
    for k, n in TEXT.items():
        v = str(entry.get(k) or "").strip()
        if len(v) > n:
            raise Refused(f"{k} is over {n} characters")
        if v:
            out[k] = v
    if "detail" not in out:
        raise Refused("a detail is needed: the sentence that says what was seen, naming the observer")
    for k in ("value", "depth", "height"):
        v = entry.get(k)
        if v is None or v == "":
            continue
        try:
            out[k] = float(v)
        except (TypeError, ValueError):
            raise Refused(f"{k} is a number") from None
    if "value" in out and "unit" not in out:
        raise Refused("a value carries its unit, as written")
    method = str(entry.get("method") or "sighting").strip()
    if method not in METHODS:
        raise Refused(f"the method is one of {', '.join(sorted(METHODS))}")
    out["method"] = method
    origin = str(entry.get("origin") or "ship").strip()
    out["origin"] = origin if origin in ORIGINS else "ship"
    out["sensitive"] = 1 if entry.get("sensitive") in (1, True, "1", "true") else 0
    for k in ("event_id", "artifact_id"):
        v = str(entry.get(k) or "").strip()[:80]
        if v:
            out[k] = v
    return out


def _image(data: str, id_: str) -> str:
    """A data URL from the form, saved beside the journal; the path the line carries."""
    m = re.match(r"^data:(image/[a-z]+);base64,(.+)$", str(data or ""), re.S)
    if not m or m.group(1) not in IMAGE_TYPES:
        raise Refused("a photograph is a JPEG, PNG or WebP")
    try:
        raw = base64.b64decode(m.group(2), validate=True)
    except Exception:                                   # noqa: BLE001
        raise Refused("the photograph did not decode") from None
    if len(raw) > IMAGE_MAX:
        raise Refused("the photograph is over 10 MB")
    IMG_DIR.mkdir(parents=True, exist_ok=True)
    name = f"{id_}{IMAGE_TYPES[m.group(1)]}"
    (IMG_DIR / name).write_bytes(raw)
    return f"_journal/img/{name}"


def _lines() -> list[dict]:
    if not FILE.is_file():
        return []
    out = []
    for line in FILE.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            out.append(json.loads(line))
        except json.JSONDecodeError:
            log.warning("a journal line did not parse: %.80s", line)
    return out


def entries(limit: int = 500) -> list[dict]:
    """The journal as the tab reads it: one entry per id (the last line with
    that id stands, as it does on grid), newest first."""
    by_id: dict[str, dict] = {}
    for e in _lines():
        if e.get("id"):
            by_id[e["id"]] = e
    rows = sorted(by_id.values(), key=lambda e: (str(e.get("date", "")), e.get("id", "")), reverse=True)
    return rows[:limit]


def next_id(day: str, lines: list[dict]) -> str:
    n = 1 + sum(1 for e in lines if str(e.get("id", "")).startswith(f"amundsen-{day}-"))
    return f"amundsen-{day}-{n:03d}"


def append(entry: dict, who: str = "") -> dict:
    """Write one line. A line with an existing journal id is a correction and
    replaces that row on the next ingest; a new line gets the day's next id.
    The photograph, if any, is saved first and the line points at it."""
    row = _clean(entry)
    JOURNAL_DIR.mkdir(parents=True, exist_ok=True)
    with open(JOURNAL_DIR / ".lock", "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        lines = _lines()
        day = row["date"][:10]
        today = datetime.now(timezone.utc).strftime("%Y-%m-%d")
        logf = JOURNAL_DIR / "journal.log"
        if logf.is_file() and sum(1 for l in logf.read_text().splitlines() if l.startswith(today)) >= LINES_PER_DAY:
            raise Refused("the journal has taken enough lines for one day; tell the keeper")
        id_ = str(entry.get("id") or "").strip()
        if id_ and not (ID_RX.match(id_) and any(e.get("id") == id_ for e in lines)):
            raise Refused("only a line already in the journal can be corrected")
        if not id_:
            id_ = next_id(day, lines)
        row = {"kind": "observation", "id": id_, **{k: v for k, v in row.items() if k != "kind"}}
        if entry.get("image"):
            row["artifact_file"] = _image(entry["image"], id_)
        else:
            # a correction without a new photograph keeps the one the line has
            before = next((e for e in reversed(lines) if e.get("id") == id_ and e.get("artifact_file")), None)
            if before:
                row["artifact_file"] = before["artifact_file"]
        with open(FILE, "a", encoding="utf-8") as f:
            f.write(json.dumps(row, ensure_ascii=False) + "\n")
        with open(JOURNAL_DIR / "journal.log", "a", encoding="utf-8") as f:
            f.write(f"{datetime.now(timezone.utc).isoformat(timespec='seconds')} {id_} {who[:60] or '-'}\n")
    return row
