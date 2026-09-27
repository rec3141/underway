"""Run the whole dashboard in one process tree: the container's init.

``python -m dashboard supervise`` does what the systemd units in ``deploy/`` do
on a workstation install, for a machine that runs only Docker:

- keeps the long-running servers up (the page server, the cruise report
  builder, the game, Caddy on port 80, and the Telegram bot while it has a token),
  restarting any that exits;
- runs the periodic jobs on the timers' schedules (the build every minute,
  alerts, calendar, satellite, cameras, ice charts), never two runs of one job
  at once;
- mounts the ship's SMB shares itself from the settings, unless something is
  already mounted or bind-mounted there.

Settings come from two files in ``UNDERWAY_CONFIG``: ``site.env`` (plain
settings) and ``underway.env`` (secrets), both written by the /settings page.
Each job reads them afresh when it starts; touching ``UNDERWAY_CONFIG/.reload``
(the settings page does, after a save) restarts the servers and remounts the
shares so they pick up the new values too. The paths of the installation are
fixed by the image and cannot be overridden from those files.
"""
from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import threading
import time
from dataclasses import dataclass, field
from pathlib import Path

APP = Path(__file__).resolve().parent.parent          # the AMUNDSEN directory
PY = sys.executable

# set by the image; a settings file cannot move the installation
PINNED = ("UNDERWAY_HOME", "UNDERWAY_DB_DIR", "UNDERWAY_CACHE_DIR", "UNDERWAY_WEBROOT", "UNDERWAY_CHAT_DB",
          "UNDERWAY_CONFIG", "UNDERWAY_MIRROR", "UNDERWAY_TILES_DIR", "UNDERWAY_CAMERA_OUTPUT", "UNDERWAY_PORT",
          "CRUISE_STATE_DIR", "UNDERWAY_PYTHON", "UNDERWAY_LOCAL", "PATH", "HOME")

MOUNT_ROOT = Path("/mnt/ship")
CREDS = Path("/run/ship-smb.creds")


def log(name: str, msg: str) -> None:
    print(f"{time.strftime('%Y-%m-%d %H:%M:%S')} [{name}] {msg}", flush=True)


def read_env_file(path: Path) -> dict[str, str]:
    """KEY=value lines, the format systemd and bash share; comments and blanks skipped."""
    out: dict[str, str] = {}
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return out
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        k, v = line.split("=", 1)
        out[k.strip()] = v.strip()
    return out


def config_dir() -> Path:
    return Path(os.environ.get("UNDERWAY_CONFIG", "~/.config/underway")).expanduser()


def environment() -> dict[str, str]:
    """The environment a child starts with: the image's, then site.env, then the secrets."""
    env = dict(os.environ)
    for name in ("site.env", "underway.env"):
        for k, v in read_env_file(config_dir() / name).items():
            if k not in PINNED:
                env[k] = v
    return env


# ---------------------------------------------------------------- the shares

def _mounted(path: Path) -> bool:
    return os.path.ismount(path)


def _bind_provided(path: Path) -> bool:
    """A directory the host supplied (a bind mount, or a copy) rather than one this process mounts."""
    return path.is_dir() and any(path.iterdir()) and not _mounted(path)


_said: set = set()


def _say_once(name: str, msg: str) -> None:
    """Log a standing problem once rather than at every minute's retry."""
    if msg not in _said:
        _said.add(msg)
        log(name, msg)


def smb_signature(env: dict[str, str]) -> tuple:
    return tuple(env.get(k, "") for k in ("SHIP_SMB_HOST", "SHIP_SMB_DATA", "SHIP_SMB_SHARE",
                                           "SHIP_SMB_USER", "SHIP_SMB_PASSWORD"))


def mount_shares(env: dict[str, str], remount: bool = False) -> None:
    """Mount //host/Data and //host/Share at /mnt/ship/{Data,Share} when the settings name a user.

    A share the host already provides (bind-mounted into the container) is left
    alone, so a machine that mounts the shares itself, or a Windows host with
    mapped drives, needs no SMB settings here.
    """
    host = env.get("SHIP_SMB_HOST", "10.0.0.10")
    user = env.get("SHIP_SMB_USER", "")
    shares = {"Data": env.get("SHIP_SMB_DATA", "Data"), "Share": env.get("SHIP_SMB_SHARE", "Share")}
    for local, remote in shares.items():
        target = MOUNT_ROOT / local
        target.mkdir(parents=True, exist_ok=True)
        if _mounted(target) and remount:
            subprocess.run(["umount", "-l", str(target)], check=False)
        if _mounted(target) or _bind_provided(target):
            continue
        if not user:
            _say_once("shares", f"{target} not mounted: add the ship share's user name and password on the settings page")
            continue
        CREDS.write_text(f"username={user}\npassword={env.get('SHIP_SMB_PASSWORD', '')}\n", encoding="utf-8")
        CREDS.chmod(0o600)
        opts = f"credentials={CREDS},vers=3.0,iocharset=utf8,file_mode=0664,dir_mode=0775,uid=0,gid=0"
        r = subprocess.run(["mount", "-t", "cifs", f"//{host}/{remote}", str(target), "-o", opts],
                           capture_output=True, text=True, timeout=60)
        if r.returncode == 0:
            _said.clear()
            log("shares", f"mounted //{host}/{remote} at {target}")
        else:
            _say_once("shares", f"could not mount //{host}/{remote}: {(r.stderr or r.stdout).strip()}")


# ---------------------------------------------------------------- servers and jobs

@dataclass
class Server:
    name: str
    argv: list[str]
    wanted: object = None           # env -> bool; None means always
    proc: subprocess.Popen | None = None
    started: float = 0.0
    failures: int = 0

    def want(self, env: dict[str, str]) -> bool:
        return self.wanted is None or bool(self.wanted(env))


@dataclass
class Job:
    name: str
    argv: list[str]
    period: int                     # seconds
    offset: int = 0                 # seconds past each period boundary (UTC epoch)
    timeout: int = 3600
    first_delay: int = 60           # seconds after start before the first run
    wanted: object = None
    running: bool = field(default=False)
    last_slot: int = -1

    def want(self, env: dict[str, str]) -> bool:
        return self.wanted is None or bool(self.wanted(env))


def _has(*keys):
    return lambda env: all(env.get(k) for k in keys)


def _gcal_key(env):
    return (config_dir() / "gcal-sa.json").is_file()


def _camera_source(env):
    src = env.get("UNDERWAY_CAMERA_SOURCE", "")
    return bool(src) and Path(src).is_dir()


def servers() -> list[Server]:
    port = os.environ.get("UNDERWAY_PORT", "8042")
    home = os.environ.get("UNDERWAY_HOME", "/underway")
    return [
        Server("dashboard", [PY, "-m", "dashboard", "serve", "--root", f"{home}/www", "--port", port]),
        Server("report", [PY, "-m", "cruisereport", "serve", "--bind", "127.0.0.1", "--port", "8044"]),
        Server("game", [PY, "/game/server.py", "--host", "127.0.0.1", "--port", "8050"]),
        Server("caddy", ["caddy", "run", "--config", str(APP / "deploy/container/Caddyfile"), "--adapter", "caddyfile"]),
        Server("telegram", [PY, "-m", "dashboard", "telegram-bot"], wanted=_has("TELEGRAM_KEY")),
    ]


def jobs() -> list[Job]:
    return [
        Job("build", ["bash", str(APP / "update_underway_py.sh")], 60, 0, timeout=6 * 3600, first_delay=5),
        Job("alerts", [PY, "-m", "dashboard", "alerts"], 120, 20, timeout=600),
        Job("uptime", [PY, "-m", "dashboard.uptime"], 60, 40, timeout=120),
        Job("gcal", [PY, "-m", "dashboard", "gcal-push"], 300, 150, timeout=600, wanted=_gcal_key),
        Job("satellite", [PY, "-m", "dashboard", "satellite"], 1800, 420, timeout=3600,
            wanted=_has("COPERNICUS_ID", "COPERNICUS_SECRET")),
        Job("camera", ["bash", str(APP / "tools/camera-job.sh"), "build"], 3600, 0, timeout=3 * 3600,
            first_delay=300, wanted=_camera_source),
        Job("camera-sync", ["bash", str(APP / "tools/camera-job.sh"), "sync"], 86400, 20 * 60, timeout=1800,
            wanted=_has("UNDERWAY_CAMERA_SHARE")),
        Job("ice-charts", [PY, "-m", "dashboard", "ice-charts", "--refresh"], 6 * 3600, 4 * 3600 + 23 * 60,
            timeout=3600, first_delay=360),
    ]


class Supervisor:
    def __init__(self) -> None:
        self.servers = servers()
        self.jobs = jobs()
        self.stopping = False
        self.t0 = time.time()
        self.reload_mtime = self._reload_mtime()
        self.status: dict[str, dict] = {}
        self.lock = threading.Lock()

    def _reload_mtime(self) -> float:
        try:
            return (config_dir() / ".reload").stat().st_mtime
        except OSError:
            return 0.0

    def _pipe(self, name: str, proc: subprocess.Popen) -> None:
        for line in proc.stdout:        # type: ignore[union-attr]
            print(f"[{name}] {line.rstrip()}", flush=True)

    def _spawn(self, name: str, argv: list[str], env: dict[str, str]) -> subprocess.Popen:
        proc = subprocess.Popen(argv, cwd=APP, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                text=True, errors="replace", start_new_session=True)
        threading.Thread(target=self._pipe, args=(name, proc), daemon=True).start()
        return proc

    def _stop(self, proc: subprocess.Popen | None) -> None:
        if proc is None or proc.poll() is not None:
            return
        try:
            os.killpg(proc.pid, signal.SIGTERM)
            proc.wait(timeout=15)
        except (ProcessLookupError, subprocess.TimeoutExpired):
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass

    def tend_servers(self, env: dict[str, str]) -> None:
        for s in self.servers:
            alive = s.proc is not None and s.proc.poll() is None
            if not s.want(env):
                if alive:
                    log(s.name, "stopping: its settings are gone")
                    self._stop(s.proc)
                continue
            if alive:
                if time.time() - s.started > 300:
                    s.failures = 0
                continue
            if s.proc is not None:
                # back off a server that keeps dying: 2, 4, 8 … up to 5 minutes between tries
                wait = min(300, 2 ** s.failures)
                if time.time() - s.started < wait:
                    continue
                log(s.name, f"exited with {s.proc.returncode}; restarting")
                s.failures += 1
            s.proc = self._spawn(s.name, s.argv, env)
            s.started = time.time()
            self._record(s.name, state="running", since=int(s.started))

    def restart_servers(self) -> None:
        for s in self.servers:
            self._stop(s.proc)
            s.proc, s.failures = None, 0

    def _record(self, name: str, **fields) -> None:
        with self.lock:
            self.status.setdefault(name, {}).update(fields)
            home = Path(os.environ.get("UNDERWAY_HOME", "/underway"))
            try:
                tmp = home / ".supervisor.json.tmp"
                tmp.write_text(json.dumps(self.status, indent=1), encoding="utf-8")
                os.replace(tmp, home / "supervisor.json")
            except OSError:
                pass

    def _run_job(self, job: Job, env: dict[str, str]) -> None:
        t = time.time()
        try:
            proc = self._spawn(job.name, job.argv, env)
            try:
                rc = proc.wait(timeout=job.timeout)
            except subprocess.TimeoutExpired:
                log(job.name, f"still running after {job.timeout} s; stopping it")
                self._stop(proc)
                rc = -1
            if rc != 0:
                log(job.name, f"exited with {rc}")
            self._record(job.name, last_run=int(t), seconds=round(time.time() - t), exit=rc)
        finally:
            job.running = False

    def tend_jobs(self, env: dict[str, str]) -> None:
        now = time.time()
        for job in self.jobs:
            slot = int((now - job.offset) // job.period)
            if job.running or slot == job.last_slot or now - self.t0 < job.first_delay:
                continue
            first = job.last_slot < 0
            job.last_slot = slot
            if not job.want(env):
                continue
            # a long period is not run just because the supervisor started; the build and other
            # jobs of an hour or less are, so a restarted box shows fresh data at once
            if first and job.period > 3600:
                continue
            job.running = True
            threading.Thread(target=self._run_job, args=(job, env), daemon=True).start()

    def run(self) -> int:
        signal.signal(signal.SIGTERM, lambda *_: setattr(self, "stopping", True))
        signal.signal(signal.SIGINT, lambda *_: setattr(self, "stopping", True))
        env = environment()
        for d in ("db", "cache", "www", "chat", "camera360", "report", "game"):
            (Path(os.environ.get("UNDERWAY_HOME", "/underway")) / d).mkdir(parents=True, exist_ok=True)
        config_dir().mkdir(parents=True, exist_ok=True)
        mount_shares(env)
        smb = smb_signature(env)
        last_mount_check = time.time()
        log("supervise", "started")
        while not self.stopping:
            m = self._reload_mtime()
            env = environment()
            if m != self.reload_mtime:
                self.reload_mtime = m
                log("supervise", "settings changed: restarting the servers")
                mount_shares(env, remount=smb_signature(env) != smb)
                smb = smb_signature(env)
                time.sleep(2)           # let the page server finish the response that saved them
                self.restart_servers()
            elif time.time() - last_mount_check > 60:
                mount_shares(env)       # the NAS reboots now and then
                last_mount_check = time.time()
            self.tend_servers(env)
            self.tend_jobs(env)
            time.sleep(1)
        log("supervise", "stopping")
        for s in self.servers:
            self._stop(s.proc)
        return 0


def main() -> int:
    return Supervisor().run()


if __name__ == "__main__":
    raise SystemExit(main())
