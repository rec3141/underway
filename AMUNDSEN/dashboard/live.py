"""Live CTD cast from the SeaBird deck unit's network feed.

Seasave can send each scan of converted data as an ASCII line over the
network; the ship relays it on the LAN as UDP. This listener binds the port,
parses every line into named columns (``UNDERWAY_CTD_COLUMNS``, in the order
Seasave writes them), follows pressure to tell when a cast is in the water,
and keeps the cast in progress and the last completed one for ``/api/live``.
Port and columns can be changed while running (``POST /api/live``), and the
last raw packets are kept so an unknown feed can be read off the page.

Nothing here is required for the rest of the dashboard: with no feed the
endpoint simply reports that it is listening (or off, port 0).
"""

from __future__ import annotations

import json
import logging
import os
import re
import socket
import threading
import time

log = logging.getLogger(__name__)

DEFAULT_COLUMNS = os.environ.get("UNDERWAY_CTD_COLUMNS", "scan,pressure,temperature,conductivity,salinity,oxygen,fluorescence")
DEFAULT_PORT = int(os.environ.get("UNDERWAY_CTD_UDP_PORT", "0"))
IN_WATER_DBAR = 2.0        # a cast starts when pressure first exceeds this …
SURFACE_DBAR = 1.0         # … and ends after SURFACE_S below this
SURFACE_S = 60
MAX_SCANS = 40000
KEEP_HZ = 2.0              # samples kept per second (a 24 Hz feed is thinned)
NUM = re.compile(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?")
PRESSURE_NAMES = ("pressure", "prdm", "prm", "pr", "p", "depth", "depsm", "depth_m")


class LiveCTD:
    def __init__(self, port: int = DEFAULT_PORT, columns: str = DEFAULT_COLUMNS):
        self.lock = threading.Lock()
        self.port = 0
        self.columns: list[str] = []
        self.sock: socket.socket | None = None
        self.packets = 0
        self.last_t = 0.0
        self.last_src = ""
        self.times: list[float] = []            # packet times, last 30 s, for the rate
        self.raw: list[str] = []                # last raw payloads (trimmed)
        self.current: dict | None = None        # the cast in the water
        self.last: dict | None = None           # the last completed cast
        self.surface_since: float | None = None
        self._last_kept = 0.0
        self.configure(port, columns)
        threading.Thread(target=self._loop, daemon=True).start()

    # ------------------------------------------------------------ config
    def configure(self, port: int, columns: str | None = None) -> None:
        with self.lock:
            if columns:
                self.columns = [c.strip() for c in columns.split(",") if c.strip()]
            if port != self.port:
                if self.sock:
                    try:
                        self.sock.close()
                    except OSError:
                        pass
                    self.sock = None
                self.port = int(port)
                if self.port:
                    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                    try:
                        s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
                    except OSError:
                        pass
                    s.settimeout(1.0)
                    s.bind(("", self.port))
                    self.sock = s
                    log.info("live CTD: listening on UDP %d, columns %s", self.port, ",".join(self.columns))
                else:
                    log.info("live CTD: off (port 0)")

    @property
    def pcol(self) -> int:
        for i, c in enumerate(self.columns):
            if c.lower() in PRESSURE_NAMES:
                return i
        return -1

    # ------------------------------------------------------------ receive
    def _loop(self) -> None:
        while True:
            s = self.sock
            if s is None:
                time.sleep(0.5)
                continue
            try:
                data, addr = s.recvfrom(65535)
            except socket.timeout:
                self._tick(time.time())
                continue
            except OSError:
                time.sleep(0.2)
                continue
            now = time.time()
            text = data.decode("latin-1", errors="replace")
            with self.lock:
                self.packets += 1
                self.last_t = now
                self.last_src = f"{addr[0]}:{addr[1]}"
                self.times.append(now)
                self.times = [t for t in self.times if now - t < 30]
                self.raw.append(text[:300].replace("\r", "\\r").replace("\n", "\\n"))
                self.raw = self.raw[-20:]
                for line in text.replace("\r", "\n").split("\n"):
                    vals = [float(x) for x in NUM.findall(line)]
                    if len(vals) >= 2:
                        self._scan(now, vals)
            self._tick(now)

    def _scan(self, now: float, vals: list[float]) -> None:
        pi = self.pcol
        if pi < 0 or pi >= len(vals):
            return
        p = vals[pi]
        depth_like = self.columns[pi].lower().startswith("dep")
        if p > IN_WATER_DBAR:
            self.surface_since = None
            if self.current is None:
                self.current = {"started": now, "t": [], "cols": {c: [] for c in self.columns}, "max_p": 0.0, "n_raw": 0, "depth_like": depth_like}
                self._last_kept = 0.0
            c = self.current
            c["n_raw"] += 1
            c["max_p"] = max(c["max_p"], p)
            if now - self._last_kept >= 1.0 / KEEP_HZ and len(c["t"]) < MAX_SCANS:
                self._last_kept = now
                c["t"].append(round(now, 2))
                for i, name in enumerate(self.columns):
                    c["cols"][name].append(vals[i] if i < len(vals) else None)
        elif p < SURFACE_DBAR and self.current is not None:
            if self.surface_since is None:
                self.surface_since = now

    def _tick(self, now: float) -> None:
        """Close a cast that has been back at the surface long enough (or gone silent)."""
        with self.lock:
            c = self.current
            if c is None:
                return
            quiet = now - self.last_t > 300
            if (self.surface_since and now - self.surface_since > SURFACE_S) or quiet:
                c["ended"] = now
                self.last, self.current, self.surface_since = c, None, None
                log.info("live CTD: cast ended, %d scans, max %.1f", len(c["t"]), c["max_p"])

    # ------------------------------------------------------------ report
    def status(self) -> dict:
        with self.lock:
            now = time.time()
            def cast(c):
                if not c:
                    return None
                t = c["t"]; p = c["cols"].get(self.columns[self.pcol]) if self.pcol >= 0 else None
                direction = None
                if p and len(p) > 10:
                    a = [x for x in p[-10:] if x is not None]
                    direction = "down" if len(a) > 1 and a[-1] > a[0] + 0.3 else "up" if len(a) > 1 and a[-1] < a[0] - 0.3 else "hold"
                return {"started": c["started"], "ended": c.get("ended"), "n": len(t), "n_raw": c["n_raw"], "max_p": round(c["max_p"], 1),
                        "direction": direction, "depth_like": c["depth_like"], "t": t, "cols": c["cols"]}
            return {"port": self.port, "columns": self.columns, "pressure_col": self.columns[self.pcol] if self.pcol >= 0 else None,
                    "packets": self.packets, "last_packet_age_s": round(now - self.last_t, 1) if self.last_t else None,
                    "rate_hz": round(len(self.times) / 30.0, 2), "source": self.last_src, "raw": self.raw[-8:],
                    "current": cast(self.current), "last": cast(self.last)}
