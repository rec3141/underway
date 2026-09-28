"""The settings page: keys and account settings filled in on the site.

``/settings`` shows the integrations' settings in a table, behind a password,
so the people running the appliance never have to edit a file over SSH. What
it writes is the two files everything else already reads from
``CONFIG_DIR``:

- ``underway.env``, the secrets (mode 0600), and ``site.env``, the plain
  settings: ``KEY=value`` lines, the format systemd's ``EnvironmentFile``, the
  supervisor and bash ``source`` share. Comments, order and keys the page does
  not know are kept; a key it does not find is appended.
- ``gcal-sa.json``, the Google service account's key (mode 0600).

After a save the page touches ``CONFIG_DIR/.reload``; the supervisor
(dashboard.supervise) then restarts the servers so they start with the new
values. Scheduled jobs read the files afresh on each run.

The admin password is set only from the command line
(``python -m dashboard set-admin-password``): the page never lets the first
visitor claim it, since anyone on the ship's Wi-Fi can reach the page. It is
kept as a scrypt hash in ``CONFIG_DIR/admin-password``. A login is an
HMAC-signed cookie (key in ``CONFIG_DIR/.session-key``) good for 12 hours, and
every form carries a CSRF token derived from it. Secrets never go back to the
browser: the page shows only whether one is set and its last four characters.
"""

from __future__ import annotations

import base64
import hashlib
import hmac
import json
import logging
import os
import re
import secrets
import threading
import time
from dataclasses import dataclass
from email.parser import BytesParser
from email.policy import HTTP as HTTP_POLICY
from http.server import SimpleHTTPRequestHandler
from pathlib import Path
from urllib.parse import parse_qs, urlsplit

from .config import CONFIG_DIR

log = logging.getLogger(__name__)

SECRETS_FILE = "underway.env"
SITE_FILE = "site.env"
GCAL_FILE = "gcal-sa.json"
PASSWORD_FILE = "admin-password"
KEY_FILE = ".session-key"
RELOAD_FILE = ".reload"

COOKIE = "uw_settings"
SESSION_SECONDS = 12 * 3600
MAX_FORM = 64 * 1024
MIN_PASSWORD = 8
FREE_TRIES = 5                  # wrong passwords from one address before it has to wait
MAX_WAIT = 15 * 60


# ---------------------------------------------------------------- the fields

@dataclass(frozen=True)
class Field:
    key: str
    label: str
    group: str
    help: str
    file: str = SECRETS_FILE
    secret: bool = False
    default: str = ""
    kind: str = "text"          # text, int, email, choice, tz
    choices: tuple[tuple[str, str], ...] = ()


GROUPS = ("Ship shares", "Telegram", "Email alerts", "Satellite", "Google Calendar", "AI models", "Cameras", "General")

FIELDS: tuple[Field, ...] = (
    Field("SHIP_SMB_HOST", "Share server address", "Ship shares",
          "The ship's file server (the NAS). Leave blank for the usual one.", SITE_FILE, default="10.0.0.10"),
    Field("SHIP_SMB_DATA", "Data share name", "Ship shares",
          "The share with the instrument files. Leave blank for the usual one.", SITE_FILE, default="Data"),
    Field("SHIP_SMB_SHARE", "Science share name", "Ship shares",
          "The share with the legs' folders and pictures. Leave blank for the usual one.", SITE_FILE, default="Share"),
    Field("SHIP_SMB_USER", "Share user name", "Ship shares",
          "The user name you would type to open the shares from a ship computer.", SITE_FILE),
    Field("SHIP_SMB_PASSWORD", "Share password", "Ship shares",
          "The password that goes with that user name.", secret=True),

    Field("TELEGRAM_KEY", "Bot token", "Telegram",
          "From @BotFather in Telegram: send it /newbot and paste the token it gives you.", secret=True),
    Field("TELEGRAM_ID", "Your chat id", "Telegram",
          "Where problem alerts go. Send /start to @userinfobot in Telegram to see your number."),

    Field("SMTP_HOST", "Mail server", "Email alerts",
          "The outgoing mail server. For Gmail: smtp.gmail.com"),
    Field("SMTP_PORT", "Mail server port", "Email alerts",
          "465 for most accounts (587 if the secure connection below is set to STARTTLS).",
          default="465", kind="int"),
    Field("SMTP_USER", "Mail account", "Email alerts",
          "The email address that sends the alerts.", kind="email"),
    Field("SMTP_PASSWORD", "Mail password", "Email alerts",
          "For Gmail, an app password (Google account, Security, App passwords), not your usual one.", secret=True),
    Field("SMTP_FROM", "Send as", "Email alerts",
          "The From address people see. Leave blank to use the mail account.", kind="email"),
    Field("SMTP_SSL", "Secure connection", "Email alerts",
          "Leave on SSL unless your mail provider says STARTTLS on port 587.",
          default="1", kind="choice", choices=(("1", "SSL (port 465)"), ("0", "STARTTLS (port 587)"))),
    Field("SMTP_REPLY_TO", "Replies go to", "Email alerts",
          "Where replies to an alert go. Leave blank for the mail account.", kind="email"),
    Field("UNDERWAY_OPS_EMAIL", "Problem alerts to", "Email alerts",
          "Who is emailed when the data stop arriving. Leave blank for the mail account.", kind="email"),

    Field("COPERNICUS_ID", "Client id", "Satellite",
          "From your Copernicus Data Space account: User settings, OAuth clients, create one."),
    Field("COPERNICUS_SECRET", "Client secret", "Satellite",
          "Shown once when you create the OAuth client; paste it here.", secret=True),

    Field("UNDERWAY_CAMERA_SOURCE", "Camera pictures folder", "Cameras",
          "Where the 360 cameras' daily folders are. Leave blank for the usual place.", SITE_FILE,
          default="/mnt/ship/Data/Camera_360"),
    Field("UNDERWAY_CAMERA_INTERVAL", "Seconds between frames", "Cameras",
          "How far apart the timelapse frames are, in seconds.", SITE_FILE, default="120", kind="int"),
    Field("UNDERWAY_CAMERA_LAYOUT", "Timelapse layout", "Cameras",
          "Portrait stacks the three cameras for phones; mosaic puts them side by side.", SITE_FILE,
          default="portrait", kind="choice", choices=(("portrait", "Portrait (phones)"), ("mosaic", "Mosaic (wide)"))),
    Field("UNDERWAY_CAMERA_WIDTH", "Timelapse width", "Cameras",
          "In pixels. Leave blank for the usual size.", SITE_FILE, kind="int"),
    Field("UNDERWAY_CAMERA_SYNC_LEG", "Current leg", "Cameras",
          "The leg's name, like 2026_LEG_04. Change it at the start of each leg.", SITE_FILE),
    Field("UNDERWAY_CAMERA_SHARE", "Copy timelapses to", "Cameras",
          "The leg's folder for timelapses on the science share. Change it with the leg.", SITE_FILE),

    Field("TZ", "Time zone", "General",
          "The ship's clocks, like America/Toronto or UTC. Leave blank for UTC.", SITE_FILE, kind="tz"),
    Field("UNDERWAY_CTD_TCP", "SeaSave address", "General",
          "Only if the live CTD cast does not show up: the SeaSave computer and ports, like "
          "10.0.0.22:49161. Leave blank to search the usual ports.", SITE_FILE),
)


def _ai_fields() -> tuple[Field, ...]:
    """A key and a model for each part that uses a language model (llm.USES), after
    the shared key those parts fall back on. A model box shows the default, and a
    model left at the default is not written, so a later default still applies."""
    from .llm import OWN_KEY_ONLY, SHARED_KEY, USES
    out = [Field(SHARED_KEY, "Shared OpenRouter key", "AI models",
                 "From openrouter.ai (Keys). Every part below whose own key is empty uses this one, "
                 "except the ice camera. "
                 "Without any key those parts are off.", secret=True)]
    for u in USES:
        out.append(Field(u.key_var, f"{u.label}: key", "AI models",
                         u.help if u.name in OWN_KEY_ONLY else f"{u.help} Leave empty to use the shared key.",
                         secret=True))
        out.append(Field(u.model_var, f"{u.label}: model", "AI models",
                         "The OpenRouter model name. Leave it as it is unless you have a reason to change it.",
                         SITE_FILE, default=u.default, kind="model-images" if u.images else "model-text"))
    return tuple(out)


FIELDS = FIELDS + _ai_fields()
BY_KEY = {f.key: f for f in FIELDS}

# site.env is sourced by the shell tools, so its values must mean the same
# thing to bash unquoted: no spaces, quotes, $ or other shell syntax
_SITE_SAFE = re.compile(r"[A-Za-z0-9_./:,@%+=-]*")
_EMAIL = re.compile(r"[^@\s]+@[^@\s]+\.[^@\s]+")


def _dir() -> Path:
    return Path(CONFIG_DIR)


def check_value(field: Field, value: str) -> str:
    """The value as it will be written, or ValueError in words for the page."""
    if any(c in value for c in "\r\n\0") or any(ord(c) < 32 for c in value):
        raise ValueError(f"{field.label}: must be on one line")
    value = value.strip()
    if not value:
        return value
    if field.file == SITE_FILE and not _SITE_SAFE.fullmatch(value):
        raise ValueError(f"{field.label}: spaces, quotes and symbols like $ ; & are not allowed here")
    if field.kind == "int" and not (value.isdigit() and 0 < int(value) < 100000):
        raise ValueError(f"{field.label}: must be a whole number")
    if field.kind == "email" and not _EMAIL.fullmatch(value):
        raise ValueError(f"{field.label}: does not look like an email address")
    if field.kind == "choice" and value not in dict(field.choices):
        raise ValueError(f"{field.label}: pick one of the choices")
    if field.kind == "tz":
        from zoneinfo import ZoneInfo
        try:
            ZoneInfo(value)
        except (ValueError, KeyError, OSError):
            raise ValueError(f"{field.label}: not a time zone name (try America/Toronto or UTC)") from None
    return value


# ---------------------------------------------------------------- KEY=value files

_LINE = re.compile(r"\s*([A-Za-z_][A-Za-z0-9_]*)\s*=(.*)")


def read_env(path: Path) -> dict[str, str]:
    """The file's KEY=value pairs, read as the supervisor reads them (the last one wins)."""
    out: dict[str, str] = {}
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return out
    for line in text.splitlines():
        if line.lstrip().startswith("#"):
            continue
        m = _LINE.match(line)
        if m:
            out[m.group(1)] = m.group(2).strip()
    return out


def _atomic_write(path: Path, data: bytes, mode: int) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(f".{path.name}.{secrets.token_hex(4)}.tmp")
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_EXCL, mode)
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
            f.flush()
            os.fsync(f.fileno())
        os.chmod(tmp, mode)                 # the umask may have taken bits off
        os.replace(tmp, path)
    except BaseException:
        tmp.unlink(missing_ok=True)
        raise


def update_env(path: Path, changes: dict[str, str | None], mode: int) -> None:
    """Set (a string) or remove (None) keys in a KEY=value file.

    Every other line stays as it is. A key's first line takes the new value and
    any later lines for it go, so the file says one thing; a key not in the
    file is appended."""
    for k, v in changes.items():
        if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", k):
            raise ValueError(f"bad setting name {k!r}")
        if v is not None and any(c in v for c in "\r\n\0"):
            raise ValueError(f"{k}: must be on one line")
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except FileNotFoundError:
        lines = []
    out, done = [], set()
    for line in lines:
        m = None if line.lstrip().startswith("#") else _LINE.match(line)
        k = m.group(1) if m else None
        if k not in changes:
            out.append(line)
            continue
        if k not in done and changes[k] is not None:
            out.append(f"{k}={changes[k]}")
        done.add(k)
    added = [f"{k}={v}" for k, v in changes.items() if k not in done and v is not None]
    if added and out and out[-1].strip():
        out.append("")
    out += added
    _atomic_write(path, ("\n".join(out) + "\n").encode(), mode)


def current() -> dict[str, str]:
    """Every field's saved value ('' when unset), read from the files now."""
    files = {name: read_env(_dir() / name) for name in (SECRETS_FILE, SITE_FILE)}
    return {f.key: files[f.file].get(f.key, "") for f in FIELDS}


def save(form: dict[str, str], clear: set[str]) -> dict[str, dict[str, str | None]]:
    """Validate and write the page's form: ``form`` maps keys to what was typed,
    ``clear`` the secrets ticked to be removed. An empty secret is left as it
    is; an empty plain setting is removed, so its default applies. Returns the
    changes made per file; raises ValueError listing every problem and writes
    nothing when there is one."""
    now = current()
    changes: dict[str, dict[str, str | None]] = {SECRETS_FILE: {}, SITE_FILE: {}}
    problems = []
    for f in FIELDS:
        if f.secret and f.key in clear:
            if now[f.key]:
                changes[f.file][f.key] = None
            continue
        if f.key not in form:
            continue
        try:
            v = check_value(f, form[f.key])
        except ValueError as e:
            problems.append(str(e))
            continue
        if f.secret and not v:
            continue
        if f.kind.startswith("model") and v == f.default:
            v = ""
        if v != now[f.key]:
            changes[f.file][f.key] = v or None
    if problems:
        raise ValueError("\n".join(problems))
    for name, ch in changes.items():
        if ch:
            update_env(_dir() / name, ch, 0o600 if name == SECRETS_FILE else 0o644)
    return {k: v for k, v in changes.items() if v}


def mask(value: str) -> str:
    """What the page shows of a secret."""
    if not value:
        return "not set"
    return f"set (ends …{value[-4:]})" if len(value) >= 12 else "set"


def request_reload() -> None:
    """Tell the supervisor to restart the servers with the new settings."""
    p = _dir() / RELOAD_FILE
    p.touch()
    os.utime(p)


# ---------------------------------------------------------------- the Google key

def check_gcal_key(data: bytes) -> dict:
    try:
        j = json.loads(data.decode("utf-8"))
    except (UnicodeDecodeError, ValueError):
        raise ValueError("That file is not a Google key: it is not JSON. Download the key again as JSON.") from None
    if not isinstance(j, dict) or j.get("type") != "service_account":
        raise ValueError("That JSON file is not a service account key (its type is not service_account).")
    for k in ("client_email", "private_key"):
        if not isinstance(j.get(k), str) or not j[k]:
            raise ValueError(f"That key has no {k}; download a new key for the service account.")
    return j


def save_gcal_key(data: bytes) -> str:
    j = check_gcal_key(data)
    _atomic_write(_dir() / GCAL_FILE, data, 0o600)
    return j["client_email"]


def gcal_account() -> str:
    """The saved key's service account, '' without one."""
    try:
        j = json.loads((_dir() / GCAL_FILE).read_text(encoding="utf-8"))
        return str(j.get("client_email") or "set")
    except FileNotFoundError:
        return ""
    except (OSError, ValueError, AttributeError):
        return "unreadable"


def status(v: dict[str, str]) -> list[tuple[str, bool, str]]:
    """(integration, on, plain words) for each integration, from the saved values."""
    out = []
    if v["SHIP_SMB_USER"]:
        host = v["SHIP_SMB_HOST"] or BY_KEY["SHIP_SMB_HOST"].default
        out.append(("Ship shares", True, f"on: signs in to {host} as {v['SHIP_SMB_USER']}"
                    + ("" if v["SHIP_SMB_PASSWORD"] else ", with no password")))
    else:
        out.append(("Ship shares", False, "off: add the share user name and password "
                    "(not needed if this computer already has the shares)"))
    missing = [w for k, w in (("TELEGRAM_KEY", "the bot token"), ("TELEGRAM_ID", "your chat id")) if not v[k]]
    out.append(("Telegram", not missing, "on" if not missing else "off: add " + " and ".join(missing)))
    missing = [w for k, w in (("SMTP_HOST", "the mail server"), ("SMTP_USER", "the mail account"),
                              ("SMTP_PASSWORD", "its password")) if not v[k]]
    out.append(("Email alerts", not missing, "on" if not missing else "off: add " + ", ".join(missing)))
    missing = [w for k, w in (("COPERNICUS_ID", "the client id"), ("COPERNICUS_SECRET", "the client secret")) if not v[k]]
    out.append(("Satellite pictures", not missing, "on" if not missing else "off: add " + " and ".join(missing)))
    from .llm import OWN_KEY_ONLY, SHARED_KEY, USES
    on = [u.label for u in USES if v[u.key_var] or (v[SHARED_KEY] and u.name not in OWN_KEY_ONLY)]
    out.append(("AI models", bool(on), "off: add an OpenRouter key" if not on else
                "on: " + ", ".join(on) + ("" if len(on) == len(USES) else
                                          "; off: " + ", ".join(u.label for u in USES if u.label not in on))))
    acct = gcal_account()
    out.append(("Google Calendar", bool(acct) and acct != "unreadable",
                f"on: {acct}" if acct and acct != "unreadable" else
                "off: the saved key cannot be read; upload it again" if acct else "off: upload the key file"))
    return out


# ---------------------------------------------------------------- the password

def password_set() -> bool:
    return (_dir() / PASSWORD_FILE).is_file()


def hash_password(password: str, n: int = 2 ** 14, r: int = 8, p: int = 1) -> str:
    salt = secrets.token_bytes(16)
    h = hashlib.scrypt(password.encode(), salt=salt, n=n, r=r, p=p, dklen=32)
    b64 = lambda b: base64.b64encode(b).decode()      # noqa: E731
    return f"scrypt${n}${r}${p}${b64(salt)}${b64(h)}"


def check_hash(password: str, stored: str) -> bool:
    try:
        algo, n, r, p, salt, h = stored.strip().split("$")
        if algo != "scrypt":
            return False
        want = base64.b64decode(h)
        got = hashlib.scrypt(password.encode(), salt=base64.b64decode(salt), n=int(n), r=int(r), p=int(p),
                             dklen=len(want))
    except (ValueError, TypeError):
        return False
    return hmac.compare_digest(got, want)


def set_password(password: str) -> None:
    if len(password) < MIN_PASSWORD:
        raise ValueError(f"the password must be at least {MIN_PASSWORD} characters")
    if any(ord(c) < 32 for c in password):
        raise ValueError("the password must be on one line")
    _atomic_write(_dir() / PASSWORD_FILE, (hash_password(password) + "\n").encode(), 0o600)


def verify_password(password: str) -> bool:
    try:
        stored = (_dir() / PASSWORD_FILE).read_text(encoding="utf-8")
    except OSError:
        return False
    return check_hash(password, stored)


WORDS = (
    "anchor arctic aurora baleen beluga berg bight bosun bow bowhead breeze buoy cabin cape capstan cargo channel "
    "chart cliff coast compass coral crest current cutter delta dinghy dock dolphin drift dune eddy eider fathom "
    "fjord floe fog fulmar galley gale glacier gull gyre harbour harpoon haze helm hull inlet island jetty kayak "
    "keel kelp knot krill lagoon lantern ledge lichen mast moss narwhal north oar orca otter pack paddle pier "
    "plankton polar puffin quay radar reef ridge rope rudder sail salmon seal shoal shore signal skiff sledge sonar "
    "sound spray squall star stern storm strait swell tern thaw tide tundra walrus wave whale winch wind"
).split()


def friendly_password() -> str:
    """Four words and two digits, easy to read out over a radio: about 30 bits."""
    return "-".join(secrets.choice(WORDS) for _ in range(4)) + f"-{secrets.randbelow(90) + 10}"


# ---------------------------------------------------------------- sessions

def _key() -> bytes:
    p = _dir() / KEY_FILE
    try:
        return bytes.fromhex(p.read_text().strip())
    except (FileNotFoundError, ValueError):
        pass
    p.parent.mkdir(parents=True, exist_ok=True)
    try:
        fd = os.open(p, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(fd, "w") as f:
            f.write(secrets.token_hex(32) + "\n")
    except FileExistsError:
        pass                                # another request made it first
    return bytes.fromhex(p.read_text().strip())


def _mac(msg: str) -> str:
    return hmac.new(_key(), msg.encode(), hashlib.sha256).hexdigest()


def _password_tag() -> str:
    """Changes with the password, so a new password ends every older login."""
    try:
        return hashlib.sha256((_dir() / PASSWORD_FILE).read_bytes()).hexdigest()[:16]
    except OSError:
        return "none"


def make_session(now: float | None = None) -> str:
    exp = int((now or time.time()) + SESSION_SECONDS)
    body = f"{exp}.{secrets.token_hex(16)}.{_password_tag()}"
    return f"{body}.{_mac(body)}"


def check_session(value: str, now: float | None = None) -> str | None:
    """The session id of a valid, unexpired login cookie; None otherwise."""
    try:
        exp, sid, tag, mac = value.split(".")
        good = hmac.compare_digest(mac, _mac(f"{exp}.{sid}.{tag}"))
        if not good or int(exp) < (now or time.time()) or not hmac.compare_digest(tag, _password_tag()):
            return None
    except (ValueError, AttributeError):
        return None
    return None if sid in _logged_out else sid


def csrf_token(sid: str) -> str:
    return _mac(f"csrf.{sid}")[:32]


def check_csrf(sid: str | None, token: str) -> bool:
    return bool(sid) and hmac.compare_digest(csrf_token(sid), str(token))


_logged_out: set[str] = set()


# ---------------------------------------------------------------- wrong passwords

class Limiter:
    """Wrong passwords per address: FREE_TRIES, then a wait that doubles with
    each further miss, from 30 seconds up to a quarter hour."""

    def __init__(self) -> None:
        self.lock = threading.Lock()
        self.misses: dict[str, tuple[int, float]] = {}

    def wait(self, who: str, now: float | None = None) -> int:
        with self.lock:
            n, until = self.misses.get(who, (0, 0.0))
        return max(0, int(until - (now or time.time()) + 0.999))

    def fail(self, who: str, now: float | None = None) -> None:
        now = now or time.time()
        with self.lock:
            n = self.misses.get(who, (0, 0.0))[0] + 1
            until = now + min(MAX_WAIT, 30 * 2 ** (n - FREE_TRIES)) if n >= FREE_TRIES else 0.0
            self.misses[who] = (n, until)
            if len(self.misses) > 10000:
                self.misses.clear()

    def ok(self, who: str) -> None:
        with self.lock:
            self.misses.pop(who, None)


LIMITER = Limiter()


# ---------------------------------------------------------------- the page

_flash: dict[str, tuple[str, str]] = {}         # session -> (ok|bad, message), shown once

LOGIN_MESSAGES = {
    "wrong": ("bad", "That password is not right."),
    "wait": ("bad", "Too many wrong passwords. Wait a few minutes and try again."),
    "out": ("ok", "You are logged out."),
    "expired": ("bad", "Your login ran out or the page was open too long. Log in again."),
}


def _env():
    from jinja2 import Environment, FileSystemLoader
    return Environment(loader=FileSystemLoader(str(Path(__file__).parent / "templates")), autoescape=True)


def render(sid: str | None, message: tuple[str, str] | None = None, typed: dict[str, str] | None = None,
           errors: list[str] | None = None) -> str:
    ctx = {"password_set": password_set(), "logged_in": bool(sid), "message": message, "errors": errors or []}
    if sid:
        v = current()
        typed = typed or {}
        groups = []
        for g in GROUPS:
            rows = []
            for f in (f for f in FIELDS if f.group == g):
                rows.append({"f": f, "value": "" if f.secret else typed.get(f.key, v[f.key]),
                             "shown": mask(v[f.key]) if f.secret else ""})
            groups.append((g, rows))
        ctx.update(groups=groups, status=status(v), csrf=csrf_token(sid), gcal=gcal_account(),
                   config_dir=str(_dir()), models={"images": openrouter_models(True), "text": openrouter_models(False)})
    return _env().get_template("settings.html.j2").render(**ctx)


def _client(h) -> str:
    """The address wrong passwords are counted against: the proxy's word for it
    when the request came through the local proxy, the peer's otherwise."""
    peer = h.client_address[0]
    fwd = [x.strip() for x in h.headers.get("X-Forwarded-For", "").split(",") if x.strip()]
    return fwd[-1] if fwd and peer in ("127.0.0.1", "::1") else peer


def _send(h, code: int, body: str = "", location: str = "", cookie: str = "") -> None:
    data = body.encode()
    h.send_response(code)
    if location:
        h.send_header("Location", location)
    if cookie:
        h.send_header("Set-Cookie", cookie)
    h.send_header("Content-Type", "text/html; charset=utf-8")
    h.send_header("Content-Length", str(len(data)))
    h.send_header("Cache-Control", "no-store")
    h.send_header("X-Frame-Options", "DENY")
    h.send_header("Content-Security-Policy", "frame-ancestors 'none'")
    h.send_header("Referrer-Policy", "no-referrer")
    SimpleHTTPRequestHandler.end_headers(h)
    if h.command != "HEAD":
        h.wfile.write(data)


def _cookie(value: str, max_age: int = SESSION_SECONDS) -> str:
    return f"{COOKIE}={value}; Path=/; HttpOnly; SameSite=Strict; Max-Age={max_age}"


def _session(h) -> str | None:
    for part in h.headers.get("Cookie", "").split(";"):
        k, _, v = part.strip().partition("=")
        if k == COOKIE:
            return check_session(v)
    return None


def _form(h) -> tuple[dict[str, str], dict[str, bytes]]:
    """The POSTed fields (url-encoded or multipart), and any uploaded files."""
    if h.headers.get("Transfer-Encoding"):
        raise ValueError("the form needs a content length")
    n = int(h.headers.get("Content-Length") or 0)
    if not 0 <= n <= MAX_FORM:
        raise ValueError("that is too much to send at once")
    h.connection.settimeout(30)
    raw = h.rfile.read(n)
    ctype = h.headers.get("Content-Type", "")
    fields, files = {}, {}
    if ctype.startswith("multipart/form-data"):
        msg = BytesParser(policy=HTTP_POLICY).parsebytes(b"Content-Type: " + ctype.encode() + b"\r\n\r\n" + raw)
        for part in msg.iter_parts():
            name = part.get_param("name", header="content-disposition")
            payload = part.get_payload(decode=True) or b""
            if part.get_filename() is not None:
                files[name] = payload
            else:
                fields[name] = payload.decode("utf-8", "replace")
    else:
        for k, vs in parse_qs(raw.decode("utf-8", "replace"), keep_blank_values=True).items():
            fields[k] = vs[-1]
    return fields, files


def _same_site(h) -> bool:
    if h.headers.get("Sec-Fetch-Site") == "cross-site":
        return False
    origin = h.headers.get("Origin")
    return not origin or urlsplit(origin).netloc == h.headers.get("Host")


def handle_get(h, path: str) -> None:
    if path != "/settings":
        # the page's links are relative, so it must be read at /settings itself
        return _send(h, 303, location="../settings" if path.startswith("/settings/") else "settings")
    sid = _session(h)
    q = parse_qs(urlsplit(h.path).query)
    message = _flash.pop(sid, None) if sid else LOGIN_MESSAGES.get(q.get("m", [""])[0])
    _send(h, 200, render(sid, message))


def handle_post(h, path: str) -> None:
    back = "settings" if path == "/settings" else "../settings"
    try:
        if not _same_site(h):
            raise PermissionError
        fields, files = _form(h)
    except (ValueError, PermissionError):
        return _send(h, 400, "<p>That request could not be read. Go back to the settings page and try again.</p>")
    sid = _session(h)

    if path == "/settings/login":
        who = _client(h)
        if not password_set():
            return _send(h, 303, location=back)
        if LIMITER.wait(who):
            return _send(h, 303, location=back + "?m=wait")
        if not verify_password(fields.get("password", "")):
            LIMITER.fail(who)
            log.info("settings: wrong password from %s", who)
            return _send(h, 303, location=back + "?m=wrong")
        LIMITER.ok(who)
        return _send(h, 303, location=back, cookie=_cookie(make_session()))

    if not check_csrf(sid, fields.get("csrf", "")):
        return _send(h, 303, location=back + "?m=expired", cookie=_cookie("", 0) if not sid else "")

    if path == "/settings/logout":
        _logged_out.add(sid)
        return _send(h, 303, location=back + "?m=out", cookie=_cookie("", 0))

    if path == "/settings":
        typed = {f.key: fields[f"f_{f.key}"] for f in FIELDS if f"f_{f.key}" in fields}
        clear = {f.key for f in FIELDS if f.secret and fields.get(f"clear_{f.key}")}
        try:
            changed = save(typed, clear)
        except ValueError as e:
            return _send(h, 400, render(sid, ("bad", "Nothing was saved. Please fix:"), typed, str(e).split("\n")))
        except OSError as e:
            log.warning("settings: could not write: %s", e)
            return _send(h, 500, render(sid, ("bad", f"Could not write the settings file: {e.strerror}."), typed))
        n = sum(len(c) for c in changed.values())
        if n:
            _flash[sid] = ("ok", f"Saved {n} setting{'s' if n != 1 else ''}. The dashboard restarts in a few "
                                 "seconds to use them; reload this page if it does not answer at once.")
            log.info("settings: saved %s", ", ".join(k for c in changed.values() for k in c))
        else:
            _flash[sid] = ("ok", "Nothing had changed.")
        _send(h, 303, location=back)
        if n:
            request_reload()                # after the response: the supervisor restarts this server
        return

    if path == "/settings/gcal-key":
        try:
            if fields.get("remove"):
                (_dir() / GCAL_FILE).unlink(missing_ok=True)
                _flash[sid] = ("ok", "The Google Calendar key is removed.")
            else:
                data = files.get("key") or b""
                if not data:
                    raise ValueError("Choose the key file first (the .json file Google gave you).")
                _flash[sid] = ("ok", f"Google Calendar key saved, for {save_gcal_key(data)}.")
        except ValueError as e:
            _flash[sid] = ("bad", str(e))
        except OSError as e:
            _flash[sid] = ("bad", f"Could not write the key file: {e.strerror}.")
        return _send(h, 303, location=back)

    if path == "/settings/password":
        new = fields.get("new", "")
        if not verify_password(fields.get("current", "")):
            _flash[sid] = ("bad", "The current password is not right; the password is unchanged.")
        elif new != fields.get("again", ""):
            _flash[sid] = ("bad", "The two new passwords are not the same; the password is unchanged.")
        else:
            try:
                set_password(new)
            except ValueError as e:
                _flash[sid] = ("bad", f"Not changed: {e}.")
            else:
                fresh = make_session()      # the new password ended the old login
                _flash[check_session(fresh)] = ("ok", "The password is changed.")
                return _send(h, 303, location=back, cookie=_cookie(fresh))
        return _send(h, 303, location=back)

    if path in ("/settings/test-telegram", "/settings/test-email", "/settings/test-ai"):
        _flash[sid] = send_test(path.rsplit("-", 1)[1])
        return _send(h, 303, location=back)

    _send(h, 404, "<p>Not found.</p>")


MODELS_FILE = ".openrouter-models.json"
MODELS_TTL = 86400


def _models_list() -> list[dict]:
    """OpenRouter's public model list, cached for a day beside the settings; [] offline."""
    import urllib.request
    p = _dir() / MODELS_FILE
    try:
        if time.time() - p.stat().st_mtime < MODELS_TTL:
            return json.loads(p.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        pass
    try:
        with urllib.request.urlopen("https://openrouter.ai/api/v1/models", timeout=6) as r:
            data = [{"id": m["id"], "in": m.get("architecture", {}).get("input_modalities", [])}
                    for m in json.load(r).get("data", [])]
        _atomic_write(p, json.dumps(data).encode(), 0o644)
        return data
    except Exception:                       # noqa: BLE001 — no list only means no suggestions
        try:
            return json.loads(p.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            return []


def openrouter_models(images: bool) -> list[str]:
    """The model names to suggest: those that read images, for the parts that send them."""
    return sorted(m["id"] for m in _models_list() if not images or "image" in m["in"])


def check_ai() -> tuple[str, str]:
    """Ask OpenRouter about each saved key (which costs nothing) and check each model name."""
    import urllib.error
    import urllib.request
    from .llm import OWN_KEY_ONLY, SHARED_KEY, USES
    v = current()
    keys: dict[str, list[str]] = {}
    for u in USES:
        k = v[u.key_var] or ("" if u.name in OWN_KEY_ONLY else v[SHARED_KEY])
        if k:
            keys.setdefault(k, []).append(u.label)
    if not keys:
        return "bad", "Save an OpenRouter key first."
    known = {m["id"]: m["in"] for m in _models_list()}
    lines, bad = [], False
    for k, labels in keys.items():
        req = urllib.request.Request("https://openrouter.ai/api/v1/key", headers={"Authorization": f"Bearer {k}"})
        try:
            with urllib.request.urlopen(req, timeout=10) as r:
                d = json.load(r).get("data", {})
            left = d.get("limit_remaining")
            lines.append(f"key {mask(k)} works ({', '.join(labels)})"
                         + (f", {left:.2f} credit left" if isinstance(left, (int, float)) else ""))
        except urllib.error.HTTPError as e:
            bad = True
            lines.append(f"key {mask(k)} was refused ({e.code}) for {', '.join(labels)}")
        except OSError as e:
            bad = True
            lines.append(f"could not reach OpenRouter: {getattr(e, 'reason', e)}")
            break
    for u in USES:
        m = v[u.model_var] or u.default
        if known and m not in known:
            bad = True
            lines.append(f"{u.label}: OpenRouter has no model called {m}")
        elif known and u.images and "image" not in known[m]:
            bad = True
            lines.append(f"{u.label}: {m} does not read images, and this part sends them")
    return ("bad" if bad else "ok"), "; ".join(lines) + "."


def send_test(channel: str) -> tuple[str, str]:
    """Send a test message with the saved settings (not the running servers',
    which may predate the save)."""
    if channel == "ai":
        return check_ai()
    v = current()
    text = "Test message from the underway dashboard's settings page. Alerts will arrive like this."
    try:
        if channel == "telegram":
            if not (v["TELEGRAM_KEY"] and v["TELEGRAM_ID"]):
                return "bad", "Save the bot token and your chat id first."
            from .alerts import Telegram
            Telegram(v["TELEGRAM_KEY"]).send(v["TELEGRAM_ID"], text)
            return "ok", "Test message sent to Telegram. Check your chat."
        if not (v["SMTP_HOST"] and v["SMTP_USER"]):
            return "bad", "Save the mail server, mail account and password first."
        from .alerts import send_email
        cfg = {"host": v["SMTP_HOST"], "port": v["SMTP_PORT"], "user": v["SMTP_USER"], "password": v["SMTP_PASSWORD"],
               "from": v["SMTP_FROM"], "ssl": v["SMTP_SSL"].lower() not in ("0", "false", "no"),
               "reply_to": v["SMTP_REPLY_TO"]}
        to = v["UNDERWAY_OPS_EMAIL"] or v["SMTP_USER"]
        send_email(cfg, to, "Underway dashboard: test message", text)
        return "ok", f"Test email sent to {to}."
    except Exception as e:                  # noqa: BLE001 — the provider's own words are the useful part
        return "bad", f"The test did not go through: {str(e)[:300]}"
