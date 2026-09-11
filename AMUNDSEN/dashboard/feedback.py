"""Durable dashboard feedback, separate from artifact review flags.

Read submissions on the server with ``python -m dashboard.feedback``.
"""
from __future__ import annotations

import json
import sqlite3
from contextlib import closing
from datetime import datetime, timezone

from .config import DB_DIR


def connect():
    DB_DIR.mkdir(parents=True, exist_ok=True)
    con = sqlite3.connect(DB_DIR / "feedback.sqlite", timeout=10)
    con.row_factory = sqlite3.Row
    con.execute("""CREATE TABLE IF NOT EXISTS feedback (
        id TEXT PRIMARY KEY, created_utc TEXT NOT NULL, message TEXT NOT NULL,
        name TEXT NOT NULL, context TEXT NOT NULL)""")
    return con


def submit(payload):
    if not isinstance(payload, dict):
        raise ValueError("Feedback must be a JSON object")
    import uuid
    try:
        ident = str(uuid.UUID(payload.get("id", "")))
    except (ValueError, TypeError, AttributeError):
        raise ValueError("Feedback needs a valid submission ID") from None
    message = payload.get("message", "")
    name = payload.get("name", "")
    context = payload.get("context", {})
    if not isinstance(message, str) or not 1 <= len(message.strip()) <= 5000:
        raise ValueError("Please enter feedback of up to 5000 characters")
    if not isinstance(name, str) or len(name) > 100:
        raise ValueError("Name must be at most 100 characters")
    if not isinstance(context, dict) or len(json.dumps(context)) > 12000:
        raise ValueError("Page context is too large")
    with closing(connect()) as con, con:
        # A retry after a lost response must not create a second submission.
        con.execute("INSERT OR IGNORE INTO feedback VALUES (?, ?, ?, ?, ?)",
                    (ident, datetime.now(timezone.utc).isoformat(), message.strip(),
                     name.strip(), json.dumps(context, ensure_ascii=False)))
    return {"ok": True, "id": ident}


if __name__ == "__main__":
    with closing(connect()) as con:
        rows = [dict(row) for row in con.execute("SELECT * FROM feedback ORDER BY created_utc")]
    for row in rows:
        row["context"] = json.loads(row["context"])
    print(json.dumps(rows, ensure_ascii=False, indent=2))
