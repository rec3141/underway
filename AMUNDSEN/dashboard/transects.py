"""Transects shared by every browser on the ship: a section the Casts tab
saved (its profiles in order, the variable, the stations), kept outside the
web root in ``UNDERWAY_DB_DIR/transects.sqlite`` and listed by
``GET /api/transects``. A transect a browser saved before sharing existed
stays in that browser until its owner shares it.
"""
from __future__ import annotations

import json
import re
import sqlite3
from contextlib import closing
from datetime import datetime, timezone

from .config import DB_DIR

ID = re.compile(r"^trs:[0-9a-f]{8,64}$")
FIELDS = ("id", "kind", "cast", "label", "members", "variable", "legs", "leg", "station", "stations", "time", "time_end", "track")


def connect():
    DB_DIR.mkdir(parents=True, exist_ok=True)
    con = sqlite3.connect(DB_DIR / "transects.sqlite", timeout=10)
    con.row_factory = sqlite3.Row
    con.execute("""CREATE TABLE IF NOT EXISTS transects (
        id TEXT PRIMARY KEY, created_utc TEXT NOT NULL, name TEXT NOT NULL, body TEXT NOT NULL)""")
    return con


def _strings(value, what, n, each):
    if not isinstance(value, list) or len(value) > n or not all(isinstance(s, str) and len(s) <= each for s in value):
        raise ValueError(f"A transect's {what} must be a list of at most {n} short strings")
    return value


def validate(transect) -> dict:
    """The transect as the Casts tab builds it, with only its known fields, or ValueError."""
    if not isinstance(transect, dict) or not ID.match(str(transect.get("id", ""))):
        raise ValueError("A transect needs an id like trs:<hex>")
    label = transect.get("label", "")
    if not isinstance(label, str) or not 1 <= len(label.strip()) <= 100:
        raise ValueError("A transect needs a name of up to 100 characters")
    members = _strings(transect.get("members", []), "members", 2000, 80)
    if not members:
        raise ValueError("A transect needs at least one profile")
    track = transect.get("track", [])
    if not isinstance(track, list) or len(track) > 2000 or not all(
            isinstance(p, list) and len(p) == 2 and all(v is None or isinstance(v, (int, float)) for v in p) for p in track):
        raise ValueError("A transect's track must be a list of [lat, lon] pairs")
    out = {"id": transect["id"], "kind": "TRS", "cast": "", "label": label.strip(), "members": members, "track": track,
           "variable": str(transect.get("variable", ""))[:60], "legs": _strings(transect.get("legs", []), "legs", 50, 40),
           "stations": _strings(transect.get("stations", []), "stations", 2000, 80),
           "station": str(transect.get("station", ""))[:4000], "time": str(transect.get("time") or "")[:40] or None,
           "time_end": str(transect.get("time_end") or "")[:40] or None}
    out["leg"] = str(transect.get("leg") or (out["legs"][0] if out["legs"] else ""))[:40] or None
    return out


def save(transect, name="") -> dict:
    """Store (or replace) a shared transect; the stored record is returned."""
    body = validate(transect)
    body["shared"] = True
    body["by"] = str(name or "")[:60].strip()
    with closing(connect()) as con, con:
        con.execute("INSERT OR REPLACE INTO transects VALUES (?, ?, ?, ?)",
                    (body["id"], datetime.now(timezone.utc).isoformat(timespec="seconds"), body["by"], json.dumps(body, ensure_ascii=False)))
    return body


def remove(ident) -> bool:
    if not ID.match(str(ident or "")):
        raise ValueError("Not a transect id")
    with closing(connect()) as con, con:
        return con.execute("DELETE FROM transects WHERE id = ?", (ident,)).rowcount > 0


def listing() -> list:
    """Every shared transect, oldest first."""
    with closing(connect()) as con:
        return [json.loads(r["body"]) for r in con.execute("SELECT body FROM transects ORDER BY created_utc, id")]


if __name__ == "__main__":
    print(json.dumps(listing(), ensure_ascii=False, indent=2))
