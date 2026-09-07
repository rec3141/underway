"""Alerts for scheduled operations, by Telegram and by email.

People subscribe from the Schedule tab (email) or by messaging the Telegram
bot; each subscription names what to hear about (keywords matched against
station and operation, or everything), how far ahead ("starting in 30 min")
and which events: upcoming, started (In progress), finished (Completed or
Canceled), moved (a start time changed). A subscription can also follow
single operations (the bell beside a row, or a ``t.me/<bot>?start=<row>``
link) or every operation of one kind (a key ``op:<operation>``, the second
bell): each such entry carries its own lead time and events, by default a
15-minute heads-up and every change. Subscriptions live in
``db/alerts.json``; what has been sent, the last version of every row seen
and the Telegram update offset live in ``db/alerts_state.json``.

``run()`` (the ``alerts`` command, underway-alerts.timer every couple of
minutes) reads the current schedule (``db/schedule.json``), answers Telegram
commands, works out what is due for each subscription and sends one message
per subscription per run. Nothing is sent twice for the same row and event.

An operations alert goes to the dashboard's keeper (``UNDERWAY_OPS_EMAIL``,
else the address in the R scheduler's ``gmail_creds``, which may also carry
the mail for it; and the Telegram chat ``TELEGRAM_ID``) when the ACSD
FULL_CSV record has not grown for ``STALE_MIN`` minutes, once per episode,
with a note when it recovers.

Telegram needs the bot token in ``UNDERWAY_TELEGRAM_TOKEN`` (or
``TELEGRAM_KEY``, as in ``~/.config/underway/underway.env``) or in
``~/.config/underway/telegram.json`` (``{"token": ...}``). Email needs an SMTP
account in ``~/.config/underway/smtp.json`` (host, port, user, password,
from, ssl, and reply_to for the address replies should go to). Without one
the corresponding channel is off and said so on the page.
"""

from __future__ import annotations

import json
import logging
import os
import re
import secrets
import smtplib
import time
from datetime import datetime, timedelta, timezone
from email.message import EmailMessage
from pathlib import Path
from zoneinfo import ZoneInfo

from .config import DB_DIR, LOCAL_TZ

log = logging.getLogger(__name__)
CONF_DIR = Path("~/.config/underway").expanduser()
TELEGRAM_TOKEN = next((os.environ[k] for k in ("UNDERWAY_TELEGRAM_TOKEN", "TELEGRAM_KEY", "TELEGRAM_BOT_TOKEN") if os.environ.get(k)), "")
EVENTS = ("upcoming", "started", "finished", "moved")
DEFAULT_EVENTS = ("upcoming", "started", "moved")
DEFAULT_LEAD_MIN = 30
ROW_LEAD_MIN = 15               # a single followed operation: a quarter hour ahead …
ROW_EVENTS = EVENTS             # … and every change to it
MOVED_MIN = 15                  # a start that shifts by less is not worth a message
STATUS_STARTED = ("in progress",)
STATUS_FINISHED = ("completed", "canceled", "cancelled")
TZ = ZoneInfo(LOCAL_TZ)
TIMEOUT = 15
STALE_MIN = 30                  # the FULL_CSV normally grows every ten minutes
WEBROOT = Path(os.environ.get("UNDERWAY_WEBROOT", "/data/underway/www"))
OPS_EMAIL = os.environ.get("UNDERWAY_OPS_EMAIL", "")
OPS_TELEGRAM = os.environ.get("TELEGRAM_ID", "")


# ---------------------------------------------------------------- storage

def _subs_path() -> Path:
    return DB_DIR / "alerts.json"


def _state_path() -> Path:
    return DB_DIR / "alerts_state.json"


def load_subs() -> list[dict]:
    p = _subs_path()
    return json.loads(p.read_text()) if p.is_file() else []


def save_subs(subs: list[dict]) -> None:
    DB_DIR.mkdir(parents=True, exist_ok=True)
    _subs_path().write_text(json.dumps(subs, indent=1))


def load_state() -> dict:
    p = _state_path()
    st = json.loads(p.read_text()) if p.is_file() else {}
    st.setdefault("sent", {}); st.setdefault("rows", {}); st.setdefault("telegram_offset", 0)
    return st


def save_state(st: dict) -> None:
    DB_DIR.mkdir(parents=True, exist_ok=True)
    _state_path().write_text(json.dumps(st))


# ---------------------------------------------------------------- subscriptions

def _clean_match(match) -> str:
    words = [w.strip() for w in str(match or "").replace(";", ",").split(",")]
    return ", ".join(w for w in words if w)[:200]


def _clean_events(events) -> list[str]:
    if isinstance(events, str):
        events = events.split(",")
    out = [e.strip().lower() for e in (events or []) if str(e).strip().lower() in EVENTS]
    return out or list(DEFAULT_EVENTS)


def _check_address(channel: str, to: str) -> str:
    if channel not in ("email", "telegram"):
        raise ValueError("channel must be email or telegram")
    to = str(to or "").strip()
    if channel == "email" and not re.fullmatch(r"[^@\s]+@[^@\s]+\.[^@\s]+", to):
        raise ValueError("that does not look like an email address")
    if channel == "telegram" and not re.fullmatch(r"-?\d{1,20}", to):
        raise ValueError("bad Telegram chat id")
    return to


def _lead(lead_min) -> int:
    try:
        return max(5, min(24 * 60, int(lead_min)))
    except (TypeError, ValueError):
        raise ValueError("lead time must be minutes") from None


def _find(subs: list[dict], channel: str, to: str) -> dict | None:
    return next((s for s in subs if s["channel"] == channel and s["to"].lower() == to.lower()), None)


def _new(channel: str, to: str) -> dict:
    return {"id": secrets.token_hex(12), "channel": channel, "to": to, "created_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
            "all": False, "match": "", "lead_min": DEFAULT_LEAD_MIN, "events": list(DEFAULT_EVENTS), "name": "", "rows": {}}


def subscribe(channel: str, to: str, match: str = "", lead_min=DEFAULT_LEAD_MIN, events=None, name: str = "") -> dict:
    """Add (or update, same channel and address) a general subscription:
    everything, or the operations matching ``match``; returns it. Rows the
    address already follows stay."""
    to = _check_address(channel, to)
    lead = _lead(lead_min)
    subs = load_subs()
    sub = _find(subs, channel, to)
    if sub is None:
        sub = _new(channel, to); subs.append(sub)
    match = _clean_match(match)
    sub.update(match=match, all=not match, lead_min=lead, events=_clean_events(events), name=str(name or "").strip()[:40] or sub.get("name", ""))
    save_subs(subs)
    return sub


def follow_row(channel: str, to: str, key: str, remove: bool = False, lead_min=ROW_LEAD_MIN, events=ROW_EVENTS, name: str = "") -> dict | None:
    """Follow (or stop following) one operation by its row key; a subscription
    left following nothing is removed. Returns the subscription, or None."""
    to = _check_address(channel, to)
    key = str(key or "").strip()[:120]
    if not key:
        raise ValueError("no operation given")
    if key.startswith("op:"):                          # a kind: transits are one kind whatever the destination
        key = "op:" + kind_of(key[3:])
    subs = load_subs()
    sub = _find(subs, channel, to)
    if remove:
        if sub is None:
            return None
        sub.setdefault("rows", {}).pop(key, None)
        if not sub["rows"] and not sub.get("all") and not sub.get("match"):
            subs.remove(sub); save_subs(subs)
            return None
    else:
        if sub is None:
            sub = _new(channel, to); subs.append(sub)
        if name and not sub.get("name"):
            sub["name"] = str(name).strip()[:40]
        sub.setdefault("rows", {})[key] = {"lead_min": _lead(lead_min), "events": _clean_events(events)}
    save_subs(subs)
    return sub


def following(channel: str, to: str) -> dict:
    """What an address follows, for the page's bells: row keys and whether
    it hears about everything or a keyword match."""
    try:
        to = _check_address(channel, to)
    except ValueError:
        return {"rows": [], "all": False, "match": ""}
    sub = _find(load_subs(), channel, to)
    if sub is None:
        return {"rows": [], "all": False, "match": ""}
    return {"rows": sorted(sub.get("rows", {})), "all": bool(sub.get("all")), "match": sub.get("match", "")}


def unsubscribe(token: str) -> dict | None:
    subs = load_subs()
    gone = next((s for s in subs if s["id"] == token), None)
    if gone:
        save_subs([s for s in subs if s is not gone])
    return gone


def kind_of(operation: str) -> str:
    """The kind an ``op:`` entry names: transits and steaming are one kind
    ("Transit"), whatever their destination; anything else is its own name."""
    op = (operation or "").strip()
    return "Transit" if re.search(r"transit|steam", op, re.I) else op


def matches(sub: dict, row: dict) -> bool:
    """Whether a general subscription covers the row (followed rows are
    checked separately, with their own settings)."""
    words = [w.strip().lower() for w in (sub.get("match") or "").split(",") if w.strip()]
    if words:
        hay = f"{row.get('station') or ''} {row.get('operation') or ''}".lower()
        return any(w in hay for w in words)
    return bool(sub.get("all", True))


# ---------------------------------------------------------------- what is due

def _rows() -> list[dict]:
    """The current schedule rows with UTC instants and keys."""
    from .calendar import _instants, row_key
    p = DB_DIR / "schedule.json"
    if not p.is_file():
        return []
    out = []
    for r in json.loads(p.read_text()).get("rows", []):
        r = dict(r, **_instants(r))
        r["key"] = row_key(r)
        if r.get("start_utc"):
            out.append(r)
    return out


def _local(iso: str) -> str:
    return datetime.fromisoformat(iso).astimezone(TZ).strftime("%H:%M")


def _when(r: dict) -> str:
    return f"{_local(r['start_utc'])}–{_local(r['end_utc'])} {datetime.fromisoformat(r['start_utc']).astimezone(TZ).strftime('%Z')}"


def _name(r: dict) -> str:
    return f"{r.get('station') or ''} — {r.get('operation') or ''}".strip(" —")


def due_events(rows: list[dict], state: dict, now: datetime) -> list[tuple[str, dict, str]]:
    """(event, row, text) for everything that happened since the last run,
    judged against the last version of each row in ``state['rows']``;
    updates that record."""
    out = []
    seen = state["rows"]
    for r in rows:
        prev = seen.get(r["key"])
        status = (r.get("status") or "").strip().lower()
        start = datetime.fromisoformat(r["start_utc"])
        if status not in STATUS_STARTED + STATUS_FINISHED and now < start <= now + timedelta(hours=24):
            mins = int((start - now).total_seconds() // 60)
            out.append(("upcoming", r, f"Starting in {mins} min: {_name(r)} ({_when(r)})" + (f" — {r['comment']}" if r.get("comment") else "")))
        if prev is not None:
            pstat = (prev.get("status") or "").strip().lower()
            if status in STATUS_STARTED and pstat not in STATUS_STARTED:
                out.append(("started", r, f"Now in progress: {_name(r)} ({_when(r)})"))
            if status in STATUS_FINISHED and pstat not in STATUS_FINISHED:
                out.append(("finished", r, f"{'Canceled' if status.startswith('cancel') else 'Completed'}: {_name(r)}"))
            if prev.get("start_utc") and prev["start_utc"] != r["start_utc"] and status not in STATUS_FINISHED:
                shift = (start - datetime.fromisoformat(prev["start_utc"])).total_seconds() / 60
                if abs(shift) >= MOVED_MIN:
                    out.append(("moved", r, f"Moved {'later' if shift > 0 else 'earlier'} by {abs(shift):.0f} min: {_name(r)}, now {_when(r)}"))
        seen[r["key"]] = {"status": r.get("status") or "", "start_utc": r["start_utc"]}
    return out


def messages_for(subs: list[dict], events: list[tuple[str, dict, str]], state: dict, now: datetime) -> list[tuple[dict, list[str]]]:
    """Per subscription, the lines it should get now (each row × event once;
    an upcoming alert only inside the subscription's lead time)."""
    out = []
    sent = state["sent"]
    for sub in subs:
        lines = []
        mine = sent.setdefault(sub["id"], {})
        for ev, r, text in events:
            rows_ = sub.get("rows") or {}
            followed = rows_.get(r["key"]) or rows_.get("op:" + kind_of(r.get("operation")))
            if followed is not None:                     # a followed row: its own lead time and events
                wanted, lead = followed.get("events", ROW_EVENTS), followed.get("lead_min", ROW_LEAD_MIN)
            elif matches(sub, r):
                wanted, lead = sub.get("events", DEFAULT_EVENTS), sub.get("lead_min", DEFAULT_LEAD_MIN)
            else:
                continue
            if ev not in wanted:
                continue
            if ev == "upcoming":
                start = datetime.fromisoformat(r["start_utc"])
                if (start - now).total_seconds() > lead * 60:
                    continue
            key = f"{r['key']}|{ev}" + (f"|{r['start_utc']}" if ev == "moved" else "")
            if key in mine:
                continue
            mine[key] = now.isoformat(timespec="seconds")
            lines.append(text)
        # forget rows that are long gone so the record stays small
        for k in [k for k, t in mine.items() if (now - datetime.fromisoformat(t)).days > 14]:
            del mine[k]
        if lines:
            out.append((sub, lines))
    return out


# ---------------------------------------------------------------- channels

def telegram_token() -> str:
    if TELEGRAM_TOKEN:
        return TELEGRAM_TOKEN
    p = CONF_DIR / "telegram.json"
    try:
        return json.loads(p.read_text()).get("token", "") if p.is_file() else ""
    except (OSError, ValueError):
        return ""


def smtp_config() -> dict | None:
    """host, port, user, password, from, ssl — from ~/.config/underway/smtp.json."""
    p = CONF_DIR / "smtp.json"
    if not p.is_file():
        return None
    try:
        c = json.loads(p.read_text())
        return c if c.get("host") and c.get("user") else None
    except (OSError, ValueError):
        return None


def gmail_creds() -> dict | None:
    """The R scheduler's Gmail settings (an R list serialised by jsonlite):
    the keeper's own account, used only to write to the keeper."""
    p = CONF_DIR / "gmail_creds"
    if not p.is_file():
        return None
    try:
        d = json.loads(p.read_text())
        names = d["attributes"]["names"]["value"]
        vals = [(v["value"][0] if isinstance(v.get("value"), list) and v["value"] else v.get("value")) for v in d["value"]]
        c = dict(zip(names, vals))
        return {"host": c["host"], "port": int(c.get("port") or 465), "user": c["user"], "password": c["password"],
                "from": c["user"], "ssl": bool(c.get("use_ssl", True))}
    except (OSError, ValueError, KeyError, TypeError, IndexError):
        return None


def ops_targets() -> tuple[str, dict | None, str]:
    """(email address, its SMTP settings, Telegram chat) for operations alerts."""
    cfg = smtp_config() or gmail_creds()
    to = OPS_EMAIL or (cfg or {}).get("user", "") if not OPS_EMAIL else OPS_EMAIL
    return to, cfg, OPS_TELEGRAM


def record_age(now: datetime) -> tuple[float | None, str]:
    """Minutes since the FULL_CSV last grew, and its last time, from the built manifest."""
    p = WEBROOT / "data" / "manifest.json"
    try:
        src = json.loads(p.read_text()).get("sources") or {}
        last = src.get("full_csv")
        if not last:
            return None, ""
        return (now - datetime.fromisoformat(last)).total_seconds() / 60, last
    except (OSError, ValueError):
        return None, ""


def ops_check(state: dict, now: datetime, tg=None, email=None) -> list[str]:
    """Say once when the record goes stale, and once when it recovers.
    Returns the messages sent."""
    email = email or send_email
    age, last = record_age(now)
    ops = state.setdefault("ops", {})
    sent = []
    if age is None:
        return sent
    stale_since = ops.get("stale_since")
    if age > STALE_MIN and not stale_since:
        ops["stale_since"] = now.isoformat(timespec="seconds")
        text = (f"FULL_CSV stale: the ACSD record last grew at {_local(last)} {TZ.tzname(now)} ({age:.0f} min ago). "
                f"The dashboard runs on the TSG tail meanwhile; check logging on the acquisition PC and the share.")
        sent.append(text)
    elif age <= STALE_MIN and stale_since:
        ops["stale_since"] = None
        text = f"FULL_CSV recovered: the ACSD record is growing again (last row {_local(last)} {TZ.tzname(now)}), stale since {_local(stale_since)}."
        sent.append(text)
    for text in sent:
        to, cfg, chat = ops_targets()
        try:
            if to and cfg:
                email(cfg, to, "Underway dashboard: " + text.split(":")[0], text)
            if chat and tg is not None:
                tg.send(chat, "⚠️ " + text)
            log.warning("alerts: ops: %s", text)
        except Exception as e:                  # noqa: BLE001
            log.warning("alerts: ops message failed: %s", e)
    return sent


class Telegram:
    def __init__(self, token: str):
        import requests
        self.rq = requests
        self.base = f"https://api.telegram.org/bot{token}"
        self.username = None

    def call(self, method: str, **params):
        r = self.rq.post(f"{self.base}/{method}", json=params, timeout=TIMEOUT)
        j = r.json()
        if not j.get("ok"):
            raise RuntimeError(j.get("description") or r.text[:200])
        return j["result"]

    def me(self) -> str:
        if self.username is None:
            self.username = self.call("getMe").get("username", "")
        return self.username

    def updates(self, offset: int) -> list[dict]:
        return self.call("getUpdates", offset=offset, timeout=0, allowed_updates=["message"])

    def send(self, chat_id: str, text: str) -> None:
        self.call("sendMessage", chat_id=chat_id, text=text, disable_web_page_preview=True)


def send_email(cfg: dict, to: str, subject: str, body: str) -> None:
    m = EmailMessage()
    m["From"] = cfg.get("from") or cfg["user"]
    m["To"] = to
    if cfg.get("reply_to"):
        m["Reply-To"] = cfg["reply_to"]
    m["Subject"] = subject
    m.set_content(body)
    port = int(cfg.get("port") or (465 if cfg.get("ssl", True) else 587))
    if cfg.get("ssl", True) and port != 587:
        with smtplib.SMTP_SSL(cfg["host"], port, timeout=TIMEOUT) as s:
            s.login(cfg["user"], cfg["password"]); s.send_message(m)
    else:
        with smtplib.SMTP(cfg["host"], port, timeout=TIMEOUT) as s:
            s.starttls(); s.login(cfg["user"], cfg["password"]); s.send_message(m)


HELP = ("Amundsen schedule alerts.\n"
        "/all — everything on the schedule\n"
        "(a bell on the dashboard follows one operation: 15 min heads-up and every change)\n"
        "/only CardS-3, CTD — only operations whose station or name contains one of these\n"
        "/lead 30 — warn this many minutes ahead\n"
        "/events upcoming,started,finished,moved — which changes to hear about\n"
        "/status — what you are subscribed to\n"
        "/stop — no more alerts")


def handle_telegram(tg: Telegram, state: dict) -> int:
    """Answer commands; returns how many were handled."""
    n = 0
    for u in tg.updates(state.get("telegram_offset", 0)):
        state["telegram_offset"] = u["update_id"] + 1
        msg = u.get("message") or {}
        chat = str((msg.get("chat") or {}).get("id") or "")
        text = (msg.get("text") or "").strip()
        if not chat or not text:
            continue
        n += 1
        who = " ".join(x for x in ((msg.get("from") or {}).get("first_name"), (msg.get("from") or {}).get("last_name")) if x)
        cmd, _, arg = text.partition(" ")
        cmd = cmd.lower().split("@")[0]
        subs = load_subs()
        mine = next((s for s in subs if s["channel"] == "telegram" and s["to"] == chat), None)
        try:
            if cmd == "/start" and arg.strip():          # the dashboard's bell: t.me/<bot>?start=<row key>
                key = decode_row(arg.strip())
                follow_row("telegram", chat, key, name=who)
                key = "op:" + kind_of(key[3:]) if key.startswith("op:") else key
                what = ("every transit" if key == "op:Transit" else f"every {key[3:]}") if key.startswith("op:") else key.replace("|", " — ")
                reply = f"Following {what}: a heads-up 15 min ahead and every change.\n/status shows everything you follow, /stop ends it all."
            elif cmd in ("/start", "/all"):
                subscribe("telegram", chat, "", (mine or {}).get("lead_min", DEFAULT_LEAD_MIN), (mine or {}).get("events"), who)
                reply = "Subscribed to every scheduled operation.\n\n" + HELP
            elif cmd == "/only":
                subscribe("telegram", chat, arg, (mine or {}).get("lead_min", DEFAULT_LEAD_MIN), (mine or {}).get("events"), who)
                reply = f"Only operations matching: {_clean_match(arg) or 'everything'}"
            elif cmd == "/lead":
                subscribe("telegram", chat, (mine or {}).get("match", ""), arg or DEFAULT_LEAD_MIN, (mine or {}).get("events"), who)
                reply = f"Warning {max(5, min(24 * 60, int(arg or DEFAULT_LEAD_MIN)))} min ahead."
            elif cmd == "/events":
                subscribe("telegram", chat, (mine or {}).get("match", ""), (mine or {}).get("lead_min", DEFAULT_LEAD_MIN), arg, who)
                reply = f"Events: {', '.join(_clean_events(arg))}"
            elif cmd == "/stop":
                if mine:
                    unsubscribe(mine["id"])
                reply = "Unsubscribed. /start to come back."
            elif cmd == "/status":
                if not mine:
                    reply = "Not subscribed. /start to subscribe."
                else:
                    general = f"{mine.get('match') or ('everything' if mine.get('all') else 'no general subscription')} · {mine.get('lead_min')} min ahead · {', '.join(mine.get('events', []))}"
                    rows = "\n".join(f"• {('every transit' if k == 'op:Transit' else 'every ' + k[3:]) if k.startswith('op:') else k.replace('|', ' — ')} ({v.get('lead_min')} min ahead)" for k, v in (mine.get("rows") or {}).items())
                    reply = f"Subscribed: {general}" + (f"\nFollowing:\n{rows}" if rows else "")
            else:
                reply = HELP
        except ValueError as e:
            reply = f"Sorry: {e}"
        try:
            tg.send(chat, reply)
        except Exception as e:                  # noqa: BLE001
            log.warning("alerts: telegram reply to %s failed: %s", chat, e)
    return n


def encode_row(key: str) -> str:
    """A row key as a Telegram /start payload (base64url, at most 64 chars)."""
    import base64
    return base64.urlsafe_b64encode(key.encode()).decode().rstrip("=")[:64]


def decode_row(payload: str) -> str:
    import base64
    try:
        return base64.urlsafe_b64decode(payload + "=" * (-len(payload) % 4)).decode()
    except Exception:                       # noqa: BLE001 — not ours: keep as typed
        return payload


def info() -> dict:
    """What the page tells people: the bot's name and whether email works."""
    st = load_state()
    return {"telegram_bot": st.get("telegram_username") or ("" if not telegram_token() else None), "email": smtp_config() is not None}


def run(now: datetime | None = None, tg: Telegram | None = None, email=send_email) -> dict:
    """One pass: Telegram commands, then everything due. Never raises."""
    now = now or datetime.now(timezone.utc)
    state = load_state()
    token = telegram_token()
    if tg is None and token:
        try:
            tg = Telegram(token)
        except Exception as e:                  # noqa: BLE001
            log.info("alerts: telegram off (%s)", e)
    handled = 0
    if tg is not None:
        try:
            state["telegram_username"] = tg.me()
            handled = handle_telegram(tg, state)
        except Exception as e:                  # noqa: BLE001
            log.warning("alerts: telegram updates failed: %s", e)
    ops_check(state, now, tg=tg, email=email)
    subs = load_subs()
    events = due_events(_rows(), state, now)
    sent = failed = 0
    cfg = smtp_config()
    for sub, lines in messages_for(subs, events, state, now):
        body = "\n".join("• " + l for l in lines)
        try:
            if sub["channel"] == "telegram":
                if tg is None:
                    raise RuntimeError("telegram not configured")
                tg.send(sub["to"], "🔔 Amundsen schedule\n" + body)
            else:
                if cfg is None:
                    raise RuntimeError("email not configured")
                email(cfg, sub["to"], f"Amundsen schedule: {lines[0][:60]}",
                      body.replace("• ", "- ") + f"\n\nShip time. Unsubscribe: http://underway.local:8042/api/alerts/unsubscribe?token={sub['id']}")
            sent += 1
        except Exception as e:                  # noqa: BLE001
            failed += 1
            log.warning("alerts: %s to %s failed: %s", sub["channel"], sub["to"], e)
    state["last_run"] = now.isoformat(timespec="seconds")
    save_state(state)
    log.info("alerts: %d subscriptions, %d events, %d messages sent, %d failed, %d telegram commands", len(subs), len(events), sent, failed, handled)
    return {"subscriptions": len(subs), "events": len(events), "sent": sent, "failed": failed, "commands": handled}
