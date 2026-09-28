"""The /settings page: its files, password, logins and routes."""

import functools
import http.client
import json
import re
import stat
import threading
import time
from urllib.parse import urlencode

import pytest

from dashboard import settings as S
from dashboard.serve import Handler, ThreadingHTTPServer


@pytest.fixture(autouse=True)
def conf(tmp_path, monkeypatch):
    d = tmp_path / "conf"
    d.mkdir()
    monkeypatch.setattr(S, "CONFIG_DIR", d)
    monkeypatch.setattr(S, "LIMITER", S.Limiter())
    monkeypatch.setattr(S, "_models_list", lambda: [{"id": "google/gemma-4-26b-a4b-it", "in": ["image", "text"]},
                                                    {"id": "some/text-only", "in": ["text"]}])
    S._flash.clear()
    S._logged_out.clear()
    return d


# ---------------------------------------------------------------- files

def test_update_keeps_comments_order_and_unknown_keys(conf):
    p = conf / "underway.env"
    p.write_text("# Telegram\nTELEGRAM_KEY=old\n\n# mine\nOTHER=thing with spaces\nTELEGRAM_KEY=dupe\nSMTP_PORT=465\n")
    S.update_env(p, {"TELEGRAM_KEY": "new", "SMTP_PORT": None, "COPERNICUS_ID": "abc"}, 0o600)
    assert p.read_text() == "# Telegram\nTELEGRAM_KEY=new\n\n# mine\nOTHER=thing with spaces\n\nCOPERNICUS_ID=abc\n"
    assert stat.S_IMODE(p.stat().st_mode) == 0o600
    assert S.read_env(p) == {"TELEGRAM_KEY": "new", "OTHER": "thing with spaces", "COPERNICUS_ID": "abc"}


def test_update_creates_the_file(conf):
    p = conf / "site.env"
    S.update_env(p, {"TZ": "UTC"}, 0o644)
    assert p.read_text() == "TZ=UTC\n"
    assert not list(conf.glob(".*.tmp"))


def test_newlines_are_refused(conf):
    with pytest.raises(ValueError):
        S.update_env(conf / "underway.env", {"TELEGRAM_KEY": "a\nEVIL=1"}, 0o600)
    with pytest.raises(ValueError):
        S.check_value(S.BY_KEY["TELEGRAM_KEY"], "a\rb")
    with pytest.raises(ValueError, match="nothing|one line"):
        S.save({"SMTP_PASSWORD": "x\ny"}, set())
    assert not (conf / "underway.env").exists()


def test_site_values_must_survive_bash(conf):
    with pytest.raises(ValueError, match="spaces"):
        S.check_value(S.BY_KEY["UNDERWAY_CAMERA_SHARE"], "/mnt/a b")
    with pytest.raises(ValueError):
        S.check_value(S.BY_KEY["SHIP_SMB_USER"], "x;rm")
    # secrets are never sourced by bash: anything on one line goes
    assert S.check_value(S.BY_KEY["SMTP_PASSWORD"], " ab cd $x ") == "ab cd $x"


def test_field_kinds_are_checked():
    with pytest.raises(ValueError):
        S.check_value(S.BY_KEY["SMTP_PORT"], "46a")
    with pytest.raises(ValueError):
        S.check_value(S.BY_KEY["SMTP_SSL"], "2")
    with pytest.raises(ValueError):
        S.check_value(S.BY_KEY["TZ"], "Mars/Olympus")
    assert S.check_value(S.BY_KEY["TZ"], "America/Toronto") == "America/Toronto"


def test_save_splits_files_and_handles_secrets(conf):
    (conf / "underway.env").write_text("# keep me\nSMTP_PASSWORD=hunter2hunter2\nTELEGRAM_KEY=tok\n")
    changed = S.save({"SMTP_PASSWORD": "", "TELEGRAM_ID": "42", "SHIP_SMB_USER": "science", "TZ": ""},
                     clear={"TELEGRAM_KEY"})
    assert changed == {"underway.env": {"TELEGRAM_KEY": None, "TELEGRAM_ID": "42"},
                       "site.env": {"SHIP_SMB_USER": "science"}}
    assert (conf / "underway.env").read_text() == "# keep me\nSMTP_PASSWORD=hunter2hunter2\n\nTELEGRAM_ID=42\n"
    assert (conf / "site.env").read_text() == "SHIP_SMB_USER=science\n"
    assert S.save({"TELEGRAM_ID": "42"}, set()) == {}


def test_mask_never_shows_the_secret():
    assert S.mask("") == "not set"
    assert S.mask("short") == "set"
    assert S.mask("123456:ABCdefGHIjkl") == "set (ends …Ijkl)"


def test_gcal_key_checked(conf):
    with pytest.raises(ValueError):
        S.save_gcal_key(b"not json")
    with pytest.raises(ValueError):
        S.save_gcal_key(json.dumps({"type": "authorized_user"}).encode())
    with pytest.raises(ValueError):
        S.save_gcal_key(json.dumps({"type": "service_account", "client_email": "a@b"}).encode())
    good = json.dumps({"type": "service_account", "client_email": "cal@p.iam.gserviceaccount.com",
                       "private_key": "-----BEGIN PRIVATE KEY-----\n"}).encode()
    assert S.save_gcal_key(good) == "cal@p.iam.gserviceaccount.com"
    assert stat.S_IMODE((conf / "gcal-sa.json").stat().st_mode) == 0o600
    assert S.gcal_account() == "cal@p.iam.gserviceaccount.com"


# ---------------------------------------------------------------- password and sessions

def test_password_hash_and_verify(conf):
    h = S.hash_password("correct horse")
    assert h.startswith("scrypt$") and "correct" not in h
    assert S.check_hash("correct horse", h)
    assert not S.check_hash("correct hors", h)
    assert not S.check_hash("x", "garbage")
    assert not S.password_set()
    with pytest.raises(ValueError):
        S.set_password("short")
    S.set_password("correct horse")
    assert S.verify_password("correct horse") and not S.verify_password("wrong one")
    assert stat.S_IMODE((conf / "admin-password").stat().st_mode) == 0o600


def test_friendly_password():
    pw = S.friendly_password()
    assert re.fullmatch(r"([a-z]+-){4}\d\d", pw) and len(pw) >= S.MIN_PASSWORD


def test_session_sign_verify_expire(conf):
    S.set_password("correct horse")
    c = S.make_session(now=1000.0)
    sid = S.check_session(c, now=1001.0)
    assert sid
    assert S.check_session(c, now=1000.0 + S.SESSION_SECONDS + 1) is None
    exp, s, tag, mac = c.split(".")
    assert S.check_session(f"{int(exp) + 999}.{s}.{tag}.{mac}", now=1001.0) is None
    assert S.check_session("nonsense", now=1001.0) is None
    assert stat.S_IMODE((conf / ".session-key").stat().st_mode) == 0o600
    S.set_password("another password")          # a new password ends older logins
    assert S.check_session(c, now=1001.0) is None


def test_csrf(conf):
    assert S.check_csrf("abc", S.csrf_token("abc"))
    assert not S.check_csrf("abc", S.csrf_token("abd"))
    assert not S.check_csrf(None, "")
    assert not S.check_csrf("abc", "")


def test_limiter_backs_off():
    lim = S.Limiter()
    for _ in range(S.FREE_TRIES - 1):
        lim.fail("1.2.3.4", now=100.0)
    assert lim.wait("1.2.3.4", now=100.0) == 0
    lim.fail("1.2.3.4", now=100.0)
    assert lim.wait("1.2.3.4", now=100.0) == 30
    lim.fail("1.2.3.4", now=100.0)
    assert lim.wait("1.2.3.4", now=100.0) == 60
    assert lim.wait("5.6.7.8", now=100.0) == 0
    lim.ok("1.2.3.4")
    assert lim.wait("1.2.3.4", now=100.0) == 0


# ---------------------------------------------------------------- the routes

class Client:
    def __init__(self, port):
        self.port, self.cookie = port, ""

    def req(self, method, path, body=None, headers=None):
        h = dict(headers or {})
        if self.cookie:
            h["Cookie"] = self.cookie
        if isinstance(body, dict):
            body = urlencode(body).encode()
            h.setdefault("Content-Type", "application/x-www-form-urlencoded")
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=10)
        try:
            conn.request(method, path, body=body, headers=h)
            r = conn.getresponse()
            data = r.read().decode()
            set_cookie = r.getheader("Set-Cookie") or ""
            if set_cookie.startswith(S.COOKIE + "="):
                v = set_cookie.split(";")[0]
                self.cookie = "" if v == S.COOKIE + "=" else v
            return r.status, r.getheader("Location"), data
        finally:
            conn.close()

    def page(self):
        return self.req("GET", "/settings")[2]

    def csrf(self):
        return re.search(r'name="csrf" value="([0-9a-f]+)"', self.page()).group(1)

    def login(self, pw="correct horse"):
        return self.req("POST", "/settings/login", {"password": pw})


@pytest.fixture
def client(tmp_path):
    web = tmp_path / "web"
    web.mkdir()
    server = ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=str(web)))
    t = threading.Thread(target=server.serve_forever, daemon=True)
    t.start()
    yield Client(server.server_port)
    server.shutdown()
    server.server_close()


def test_no_password_page_says_how(client, conf):
    page = client.page()
    assert "set-admin-password" in page and 'name="password"' not in page
    code, loc, _ = client.login("anything")
    assert code == 303 and not client.cookie
    assert not (conf / "admin-password").exists()


def test_login_save_logout(client, conf):
    S.set_password("correct horse")
    (conf / "underway.env").write_text("# secrets\nTELEGRAM_KEY=123456:SECRETSECRETabcd\n")
    assert 'name="password"' in client.page()
    code, loc, _ = client.login("wrong")
    assert (code, loc) == (303, "../settings?m=wrong") and not client.cookie
    code, loc, _ = client.login()
    assert (code, loc) == (303, "../settings") and client.cookie
    page = client.page()
    assert "SECRETSECRET" not in page and "set (ends …abcd)" in page
    assert "Telegram:</b> off: add your chat id" in page
    assert 'href="static/style.css"' in page                 # relative: the site may sit under /underway/
    assert not re.search(r"#[0-9a-fA-F]{3,8}\b", page.split("<style>")[1].split("</style>")[0])   # theme tokens only
    csrf = client.csrf()
    code, loc, _ = client.req("POST", "/settings", {"csrf": csrf, "f_TELEGRAM_KEY": "", "f_TELEGRAM_ID": "777",
                                                    "f_SHIP_SMB_USER": "science", "f_TZ": "UTC"})
    assert (code, loc) == (303, "settings")
    assert (conf / "underway.env").read_text() == "# secrets\nTELEGRAM_KEY=123456:SECRETSECRETabcd\n\nTELEGRAM_ID=777\n"
    assert (conf / "site.env").read_text() == "SHIP_SMB_USER=science\nTZ=UTC\n"
    time.sleep(0.05)
    assert (conf / ".reload").exists()
    page = client.page()
    assert "Saved 3 settings" in page and "Telegram:</b> on" in page
    # a bad value: nothing written, the page says why and keeps what was typed
    code, _, page = client.req("POST", "/settings", {"csrf": csrf, "f_SMTP_PORT": "abc", "f_TZ": "Europe/Paris"})
    assert code == 400 and "must be a whole number" in page and 'value="Europe/Paris"' in page
    assert "TZ=UTC" in (conf / "site.env").read_text()
    code, loc, _ = client.req("POST", "/settings/logout", {"csrf": csrf})
    assert (code, loc) == (303, "../settings?m=out") and not client.cookie
    assert "You are logged out" in client.req("GET", "/settings?m=out")[2]


def test_post_without_csrf_is_refused(client, conf):
    S.set_password("correct horse")
    client.login()
    code, loc, _ = client.req("POST", "/settings", {"f_TELEGRAM_ID": "666"})
    assert code == 303 and "expired" in loc
    code, _, _ = client.req("POST", "/settings", {"csrf": client.csrf(), "f_TELEGRAM_ID": "666"},
                            headers={"Origin": "http://evil.example"})
    assert code == 400
    assert not (conf / "underway.env").exists()


def test_logged_out_cannot_save(client, conf):
    S.set_password("correct horse")
    code, loc, _ = client.req("POST", "/settings", {"csrf": "0" * 32, "f_TELEGRAM_ID": "666"})
    assert code == 303 and not (conf / "underway.env").exists()


def test_login_rate_limit(client, conf):
    S.set_password("correct horse")
    for _ in range(S.FREE_TRIES):
        client.login("wrong")
    code, loc, _ = client.login()               # even the right one must wait now
    assert loc.endswith("m=wait") and not client.cookie


def test_gcal_upload_and_password_change(client, conf):
    S.set_password("correct horse")
    client.login()
    csrf = client.csrf()
    key = json.dumps({"type": "service_account", "client_email": "cal@x.iam.gserviceaccount.com",
                      "private_key": "k"}).encode()
    b = "----b0undary"
    body = (f"--{b}\r\nContent-Disposition: form-data; name=\"csrf\"\r\n\r\n{csrf}\r\n"
            f"--{b}\r\nContent-Disposition: form-data; name=\"key\"; filename=\"k.json\"\r\n"
            f"Content-Type: application/json\r\n\r\n").encode() + key + f"\r\n--{b}--\r\n".encode()
    code, loc, _ = client.req("POST", "/settings/gcal-key", body,
                              {"Content-Type": f"multipart/form-data; boundary={b}"})
    assert (code, loc) == (303, "../settings")
    assert (conf / "gcal-sa.json").read_bytes() == key
    assert "Google Calendar key saved, for cal@x.iam.gserviceaccount.com" in client.page()

    old = client.cookie
    client.req("POST", "/settings/password", {"csrf": csrf, "current": "nope", "new": "a new pass", "again": "a new pass"})
    assert "current password is not right" in client.page()
    client.req("POST", "/settings/password", {"csrf": csrf, "current": "correct horse", "new": "a new pass",
                                              "again": "a new pass"})
    assert client.cookie != old and "The password is changed" in client.page()
    assert S.verify_password("a new pass")
    client.cookie = old                         # the login from before the change no longer counts
    assert 'name="password"' in client.page()
