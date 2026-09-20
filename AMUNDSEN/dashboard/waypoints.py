"""Waypoints anyone has marked on the map, kept outside the web root in
``UNDERWAY_DB_DIR/waypoints.sqlite`` and served at ``/api/waypoints``.

A waypoint is a named position: what a person dropped on the map and chose to
keep. Every browser lists them beside the logged stations, so a place worth
returning to is on everyone's map.
"""
from __future__ import annotations

import json
import re
import sqlite3
import uuid
from contextlib import closing
from datetime import datetime, timezone

from .config import DB_DIR

ID = re.compile(r"^wp:[0-9a-f]{8,64}$")


def connect():
    DB_DIR.mkdir(parents=True, exist_ok=True)
    con = sqlite3.connect(DB_DIR / "waypoints.sqlite", timeout=10)
    con.row_factory = sqlite3.Row
    con.execute("""CREATE TABLE IF NOT EXISTS waypoints (
        id TEXT PRIMARY KEY, created_utc TEXT NOT NULL, name TEXT NOT NULL,
        lat REAL NOT NULL, lon REAL NOT NULL, by_whom TEXT NOT NULL, note TEXT NOT NULL DEFAULT '')""")
    return con


def validate(waypoint) -> dict:
    """The waypoint as the map offers it, with only its known fields, or ValueError."""
    if not isinstance(waypoint, dict):
        raise ValueError("A waypoint must be a JSON object")
    ident = str(waypoint.get("id") or f"wp:{uuid.uuid4().hex}")
    if not ID.match(ident):
        raise ValueError("A waypoint needs an id like wp:<hex>")
    name = waypoint.get("name", "")
    if not isinstance(name, str) or not 1 <= len(name.strip()) <= 80:
        raise ValueError("A waypoint needs a name of up to 80 characters")
    try:
        lat, lon = float(waypoint["lat"]), float(waypoint["lon"])
    except (KeyError, TypeError, ValueError):
        raise ValueError("A waypoint needs a position") from None
    if not (-90 <= lat <= 90 and -180 <= lon <= 180):
        raise ValueError("A waypoint's position is out of range")
    note = waypoint.get("note", "")
    if not isinstance(note, str) or len(note) > 500:
        raise ValueError("A waypoint's note must be at most 500 characters")
    return {"id": ident, "name": name.strip(), "lat": round(lat, 6), "lon": round(lon, 6), "note": note.strip(),
            "time": _utc(waypoint.get("time"))}


def _utc(value):
    """A time the map sends, as a UTC stamp to the second; None when it is not one."""
    if not isinstance(value, str) or not value:
        return None
    try:
        when = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None
    when = when.replace(tzinfo=timezone.utc) if when.tzinfo is None else when.astimezone(timezone.utc)
    return when.isoformat(timespec="seconds")


def save(waypoint, who="") -> dict:
    """Store (or replace) a waypoint; the stored record is returned.

    A waypoint is stamped with the moment it was marked on the map, which the
    map sends; renaming it later leaves that stamp alone.
    """
    body = validate(waypoint)
    body["by"] = str(who or "")[:60].strip()
    marked = body.pop("time", None)
    with closing(connect()) as con, con:
        kept = con.execute("SELECT created_utc FROM waypoints WHERE id = ?", (body["id"],)).fetchone()
        body["created_utc"] = kept["created_utc"] if kept else (marked or datetime.now(timezone.utc).isoformat(timespec="seconds"))
        con.execute("INSERT OR REPLACE INTO waypoints VALUES (?, ?, ?, ?, ?, ?, ?)",
                    (body["id"], body["created_utc"], body["name"], body["lat"], body["lon"], body["by"], body["note"]))
    return body


def remove(ident) -> bool:
    if not ID.match(str(ident or "")):
        raise ValueError("Not a waypoint id")
    with closing(connect()) as con, con:
        return con.execute("DELETE FROM waypoints WHERE id = ?", (ident,)).rowcount > 0


def listing() -> list:
    """Every waypoint, oldest first."""
    with closing(connect()) as con:
        return [{"id": r["id"], "created_utc": r["created_utc"], "name": r["name"], "lat": r["lat"],
                 "lon": r["lon"], "by": r["by_whom"], "note": r["note"]}
                for r in con.execute("SELECT * FROM waypoints ORDER BY created_utc, id")]


if __name__ == "__main__":
    print(json.dumps(listing(), ensure_ascii=False, indent=2))
