"""The ship chat: rooms, direct messages, presence, and the AI crew's part in it.

One SQLite log holds every message with a ``channel``:

* ``ship``       the public room: people, and a crew member only when @mentioned
* ``crew``       the AI crew's room: any message there is answered, and the crew
                 speak unprompted while someone has it open
* ``ada``        the Library, Ada's room: every message is a question to the librarian,
                 answered at length from the History wiki with the pages read
* ``dm:a|b``     a direct message between two names, either of which may be a
                 crew member (``@ada``, ``@doc``, ``@capn``, ``@polly``); a room
                 with a crew member is private and the member speaks unprompted
                 while it is open

Identity is a name plus a device token: the first device to use a name owns it
until it releases it, and a direct message is delivered only to polls that
present the owner's token. No passwords; enough for a ship's company.

The crew see the last five kilobytes of the room they are speaking in, never
another room. Clearing a room with only a crew member in it deletes it on the
server, which is the crew member's memory of it; clearing a shared room hides
it on the device alone.
"""

from __future__ import annotations

import json
import logging
import os
import re
import secrets
import sqlite3
import threading
import time
from pathlib import Path

log = logging.getLogger(__name__)

CHAT_DB = Path(os.environ.get("UNDERWAY_CHAT_DB", "/data/underway/chat/chat.sqlite"))
CHAT_KEEP = 4000            # messages kept, all rooms together
CHAT_PAGE = 100             # messages sent to a fresh page of a room
CONTEXT_BYTES = 5000        # what a crew member sees of the room it speaks in
NAME_MAX, TEXT_MAX = 24, 500
PRESENCE_S = 45             # a poll this recent means the page is open
FIXED = {"ship": "Ship", "crew": "Crew", "ada": "Library"}

_lock = threading.Lock()
_online: dict[str, dict] = {}           # name -> {"t", "room", "emoji"}: who has which room open
_last_post: dict[str, float] = {}       # address -> last post, a light rate limit
_typing: dict[str, set] = {}            # channel -> handles composing there
CREW = None                             # the model-driven crew, once the server is up (chatbot.Crew)
ROOT: Path | None = None                # the web root, for Ada's wiki


# ---------------------------------------------------------------- storage
def conn() -> sqlite3.Connection:
    CHAT_DB.parent.mkdir(parents=True, exist_ok=True)
    c = sqlite3.connect(CHAT_DB, timeout=5)
    c.execute("CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY, t REAL NOT NULL, addr TEXT, name TEXT NOT NULL, "
              "text TEXT NOT NULL, emoji TEXT, channel TEXT NOT NULL DEFAULT 'ship', meta TEXT)")
    cols = {r[1] for r in c.execute("PRAGMA table_info(messages)")}
    for col, ddl in (("emoji", "TEXT"), ("channel", "TEXT NOT NULL DEFAULT 'ship'"), ("meta", "TEXT")):
        if col not in cols:
            c.execute(f"ALTER TABLE messages ADD COLUMN {col} {ddl}")
    c.execute("CREATE INDEX IF NOT EXISTS messages_channel ON messages (channel, id)")
    c.execute("CREATE TABLE IF NOT EXISTS names (name TEXT PRIMARY KEY, token TEXT NOT NULL, first_seen REAL, last_seen REAL)")
    c.execute("CREATE TABLE IF NOT EXISTS schema (version INTEGER)")
    v = c.execute("SELECT version FROM schema").fetchone()
    if not v:
        # the rooms were once 'crew' (the public one) and 'historian'
        c.execute("UPDATE messages SET channel='ship' WHERE channel='crew'")
        c.execute("UPDATE messages SET channel='ada' WHERE channel='historian'")
        c.execute("INSERT INTO schema (version) VALUES (2)")
    c.commit()
    return c


def clean_emoji(e: str) -> str:
    e = (e or "").strip()
    return e[:8] if e and "<" not in e else ""


def clean_name(n: str) -> str:
    return " ".join((n or "").split())[:NAME_MAX]


# ---------------------------------------------------------------- rooms
def bots() -> dict:
    """handle -> persona, for the crew members that can be in a room."""
    if not (CREW and CREW.enabled):
        return {}
    from .chatbot import PERSONAS
    return PERSONAS


def is_bot(name: str) -> bool:
    return name.startswith("@") and name[1:].lower() in bots()


def dm_channel(a: str, b: str) -> str:
    return "dm:" + "|".join(sorted((a.strip().lower(), b.strip().lower())))


def participants(channel: str) -> list[str]:
    return channel[3:].split("|") if channel.startswith("dm:") else []


def valid_channel(channel: str, name: str) -> bool:
    if channel in FIXED:
        return True
    if not channel.startswith("dm:"):
        return False
    p = participants(channel)
    return len(p) == 2 and name.lower() in p and all(x and len(x) <= NAME_MAX + 1 for x in p) and not all(x.startswith("@") for x in p)


def room_title(channel: str, me: str) -> str:
    if channel in FIXED:
        return FIXED[channel]
    other = [p for p in participants(channel) if p != me.lower()]
    if not other:
        return "Me"
    o = other[0]
    if o.startswith("@"):
        b = bots().get(o[1:])
        return b["name"] if b else o
    return o


def bots_in(channel: str) -> list[str]:
    """The crew handles that belong to a room: all four in the crew room, Ada
    in hers, the one named in a direct message, none in the public room."""
    if channel == "crew":
        return list(bots())
    if channel == "ada":
        return ["ada"] if "ada" in bots() else []
    return [p[1:] for p in participants(channel) if p.startswith("@") and p[1:] in bots()]


def open_rooms() -> list[tuple[str, str]]:
    """(channel, name) for every room a person has open right now."""
    now = time.time()
    with _lock:
        return [(v["room"], n) for n, v in _online.items() if now - v["t"] <= PRESENCE_S and v.get("room")]


# ---------------------------------------------------------------- identity
def claim(c: sqlite3.Connection, name: str, token: str) -> str:
    """'' when the name is this device's, else why not."""
    if not name:
        return ""
    if not token:
        return "this browser has no chat token; reload the page"
    now = time.time()
    row = c.execute("SELECT token FROM names WHERE name = ?", (name,)).fetchone()
    if row and row[0] != token:
        return "that name is in use on another device; pick another, or release it there"
    if row:
        c.execute("UPDATE names SET last_seen = ? WHERE name = ?", (now, name))
    else:
        c.execute("INSERT INTO names (name, token, first_seen, last_seen) VALUES (?, ?, ?, ?)", (name, token, now, now))
    c.commit()
    return ""


def release(c: sqlite3.Connection, name: str, token: str) -> bool:
    cur = c.execute("DELETE FROM names WHERE name = ? AND token = ?", (name, token))
    c.commit()
    return cur.rowcount > 0


# ---------------------------------------------------------------- reading
def _row(i, t, n, x, e, meta) -> dict:
    m = {"id": i, "t": t, "name": n, "text": x, "emoji": e or ""}
    if meta:
        try:
            m["meta"] = json.loads(meta)
        except ValueError:
            pass
    return m


def read(since: int, name: str, token: str, emoji: str = "", leave: bool = False, channel: str = "ship") -> dict:
    name = clean_name(name)
    now = time.time()
    with _lock:
        c = conn()
        try:
            error = claim(c, name, token)
            if name and not leave and not error:
                _online[name] = {"t": now, "room": channel, "emoji": clean_emoji(emoji)}
            if leave:
                _online.pop(name, None)
            for k in [k for k, v in _online.items() if now - v["t"] > PRESENCE_S]:
                del _online[k]
            online = [{"name": n, "emoji": v.get("emoji", ""), "room": v.get("room", "")} for n, v in sorted(_online.items())]
            if error or not valid_channel(channel, name):
                rows, latest_rows = [], []
            else:
                if since > 0:
                    rows = c.execute("SELECT id, t, name, text, emoji, meta FROM messages WHERE channel = ? AND id > ? ORDER BY id",
                                     (channel, since)).fetchall()
                else:
                    rows = c.execute("SELECT id, t, name, text, emoji, meta FROM messages WHERE channel = ? ORDER BY id DESC LIMIT ?",
                                     (channel, CHAT_PAGE)).fetchall()[::-1]
                latest_rows = c.execute("SELECT channel, MAX(id) FROM messages GROUP BY channel").fetchall()
        finally:
            c.close()
    latest = dict(latest_rows)
    me = name.lower()
    rooms = [{"channel": ch, "title": title, "kind": "room", "latest": latest.get(ch, 0)} for ch, title in FIXED.items()]
    for ch, mid in sorted(latest.items(), key=lambda x: -x[1]):
        if ch.startswith("dm:") and me and me in participants(ch):
            rooms.append({"channel": ch, "title": room_title(ch, name), "kind": "dm", "latest": mid})
    from .chatbot import model_status
    st = model_status() if bots() else {"online": False, "model": "", "why": "the crew are off"}
    return {"messages": [_row(*r) for r in rows], "online": online, "channel": channel, "rooms": rooms,
            "typing": sorted(_typing.get(channel, set())), "error": error,
            "crew": [{"handle": h, "name": p["name"], "emoji": p["emoji"], "beat": p["beat"]} for h, p in bots().items()],
            "room_bots": bots_in(channel), "model": st["model"] if st["online"] else st.get("why", ""),
            "model_online": bool(st["online"]), "now": now}


def context(channel: str, limit: int = CONTEXT_BYTES) -> list[dict]:
    """The newest messages of one room, up to about ``limit`` bytes of text,
    oldest first: what a crew member speaking there gets to see."""
    c = conn()
    try:
        out, size = [], 0
        for r in c.execute("SELECT id, t, name, text, emoji, meta FROM messages WHERE channel = ? ORDER BY id DESC LIMIT 400", (channel,)):
            size += len(r[3].encode("utf-8")) + len(r[2]) + 4
            if size > limit and out:
                break
            out.append(_row(*r))
        return out[::-1]
    finally:
        c.close()


# ---------------------------------------------------------------- writing
def post(addr: str, name: str, text: str, emoji: str = "", channel: str = "ship", token: str = "",
         slug: str = "", bot: bool = False, meta: dict | None = None) -> dict:
    name = clean_name(name) or "anon"
    text = text.strip()[:TEXT_MAX if not bot else 2600]
    emoji = clean_emoji(emoji)
    if not text:
        return {"error": "empty"}
    if not bot and not valid_channel(channel, name):
        return {"error": "no such room"}
    now = time.time()
    with _lock:
        c = conn()
        try:
            if not bot:
                if now - _last_post.get(addr, 0) < 1.0:
                    return {"error": "slow down"}
                err = claim(c, name, token)
                if err:
                    return {"error": err}
                _last_post[addr] = now
                _online[name] = {"t": now, "room": channel, "emoji": emoji or _online.get(name, {}).get("emoji", "")}
            cur = c.execute("INSERT INTO messages (t, addr, name, text, emoji, channel, meta) VALUES (?, ?, ?, ?, ?, ?, ?)",
                            (now, addr, name, text, emoji, channel, json.dumps(meta) if meta else None))
            c.execute("DELETE FROM messages WHERE id <= (SELECT MAX(id) FROM messages) - ?", (CHAT_KEEP,))
            c.commit()
            mid = cur.lastrowid
        finally:
            c.close()
    if not bot and CREW:
        CREW.on_message(name, text, channel, slug)
    return {"ok": True, "id": mid, "t": now}


def clear(channel: str, name: str, token: str) -> dict:
    """Empty a room. A room whose only other member is a crew member is
    deleted on the server, which forgets it; any other room is the device's
    to hide, so nothing is deleted for anyone else."""
    name = clean_name(name)
    if not valid_channel(channel, name):
        return {"error": "no such room"}
    others = [p for p in participants(channel) if p != name.lower()]
    with _lock:
        c = conn()
        try:
            if claim(c, name, token):
                return {"error": "not your name"}
            if channel.startswith("dm:") and others and all(o.startswith("@") for o in others):
                n = c.execute("DELETE FROM messages WHERE channel = ?", (channel,)).rowcount
                c.commit()
                return {"ok": True, "deleted": n}
            return {"ok": True, "deleted": 0}
        finally:
            c.close()


def typing(channel: str, handle: str, on: bool) -> None:
    with _lock:
        s = _typing.setdefault(channel, set())
        (s.add if on else s.discard)(handle)


# ---------------------------------------------------------------- citations
_CITE_RX = re.compile(r"\[([^\[\]\n]{3,140})\](?!\()")


def _norm(s: str) -> str:
    return re.sub(r"[^a-z0-9]+", " ", s.lower()).strip()


def link_citations(text: str, pages: list[dict]) -> str:
    """A page the answer cites as [Its Title], exactly or nearly, becomes a
    Markdown link to that page; anything in brackets that is no page is left."""
    if not pages:
        return text
    titles = [(p, _norm(p["title"])) for p in pages if p.get("title")]

    def sub(m):
        raw = m.group(1)
        q = _norm(raw)
        if not q:
            return m.group(0)
        for p, t in titles:
            if q == t:
                return f"[{p['title']}](#history/{p['slug']})"
        for p, t in titles:
            if len(q) >= 8 and (q in t or t in q):
                return f"[{p['title']}](#history/{p['slug']})"
        # a slug cited as such
        for p, _ in titles:
            if raw.strip() in (p["slug"], p["slug"].split("/")[-1]):
                return f"[{p['title']}](#history/{p['slug']})"
        return m.group(0)
    return _CITE_RX.sub(sub, text)


def new_token() -> str:
    return secrets.token_urlsafe(18)
