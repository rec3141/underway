"""Live CTD cast from the SeaBird deck unit's network feed.

Seasave can send each scan of converted data as an ASCII line over the
network; the ship relays it on the LAN as UDP. This listener binds the port,
parses every line into named columns (``UNDERWAY_CTD_COLUMNS``, in the order
Seasave writes them), follows pressure to tell when a cast is in the water,
and keeps the cast in progress and the last completed one for ``/api/live``.
Port and columns can be changed while running (``POST /api/live``), and the
last raw packets are kept so an unknown feed can be read off the page.
Configuration is applied only after a replacement socket binds successfully.
Changing the port or column order ends the current cast; the next in-water
scan starts a new one. Saved casts retain their original column definitions.
Scans accept comma-, semicolon- or whitespace-separated finite numbers;
malformed lines remain visible in raw packets but are not plotted.

Nothing here is required for the rest of the dashboard: with no feed the
endpoint simply reports that it is listening (or off, port 0).
"""

from __future__ import annotations

import math
import logging
import os
import re
import socket
import threading
import time

log = logging.getLogger(__name__)

DEFAULT_COLUMNS = os.environ.get("UNDERWAY_CTD_COLUMNS", "scan,pressure,temperature,conductivity,salinity,oxygen,fluorescence")
DEFAULT_PORT = os.environ.get("UNDERWAY_CTD_UDP_PORT", "0")
IN_WATER_DBAR = 2.0        # a cast starts when pressure first exceeds this …
SURFACE_DBAR = 1.0         # … and ends after SURFACE_S below this
SURFACE_S = 60
MAX_SCANS = 40000
KEEP_HZ = 2.0              # samples kept per second (a 24 Hz feed is thinned)
NUM = re.compile(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?")
PRESSURE_NAMES = ("pressure", "prdm", "prm", "pr", "p", "depth", "depsm", "depth_m")


class LiveCTD:
    def __init__(self, port: int | str = DEFAULT_PORT, columns: str = DEFAULT_COLUMNS):
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
        self._generation = 0
        self._stop = threading.Event()
        self.configure(port, columns)
        self._thread = threading.Thread(target=self._loop, daemon=True)
        self._thread.start()

    # ------------------------------------------------------------ config
    def configure(self, port: int | str, columns: str | None = None) -> None:
        if isinstance(port, str) and port.strip().isdigit():
            port = int(port.strip())
        if type(port) is not int or not 0 <= port <= 65535:
            raise ValueError("UDP port must be an integer from 0 to 65535 (0 disables reception)")
        names = None
        if columns is not None:
            if not isinstance(columns, str):
                raise ValueError("Columns must be comma-separated names")
            names = [c.strip() for c in columns.split(",")]
            if not all(names) or len({c.lower() for c in names}) != len(names):
                raise ValueError("Column names must be nonempty and unique")
        with self.lock:
            if self._stop.is_set():
                raise ValueError("Listener is closed")
            names = self.columns if names is None else names
            replace_socket = port != self.port or (port != 0 and self.sock is None)
            candidate = self.sock
            if replace_socket:
                candidate = None
                if port:
                    candidate = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                    try:
                        candidate.settimeout(0.25)
                        candidate.bind(("", port))
                    except OSError:
                        candidate.close()
                        raise
            if replace_socket or names != self.columns:
                if self.current is not None:
                    self.current.update(ended=time.time(), end_reason="configuration changed")
                    self.last, self.current = self.current, None
                self.surface_since = None
                self._generation += 1
            old = self.sock
            self.sock, self.port, self.columns = candidate, port, names
            if replace_socket and old is not None:
                old.close()
            log.info("live CTD: UDP port %d (0 = off), columns %s", port, ",".join(names))

    def close(self) -> None:
        self._stop.set()
        with self.lock:
            if self.sock is not None:
                self.sock.close()
                self.sock = None
        self._thread.join(timeout=2)

    @property
    def pcol(self) -> int:
        for i, c in enumerate(self.columns):
            if c.lower() in PRESSURE_NAMES:
                return i
        return -1

    # ------------------------------------------------------------ receive
    def _loop(self) -> None:
        while not self._stop.is_set():
            with self.lock:
                s, generation = self.sock, self._generation
            if s is None:
                self._stop.wait(0.1)
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
                if s is not self.sock or generation != self._generation:
                    continue
                self.packets += 1
                self.last_t = now
                self.last_src = f"{addr[0]}:{addr[1]}"
                self.times.append(now)
                self.times = [t for t in self.times if now - t < 30]
                self.raw.append(text[:300].replace("\r", "\\r").replace("\n", "\\n"))
                self.raw = self.raw[-20:]
                for line in text.replace("\r", "\n").split("\n"):
                    # Reject malformed fields rather than shifting subsequent values
                    # into the wrong columns (particularly the pressure column).
                    fields = [field for part in re.split(r"[,;]", line.strip())
                              for field in (part.split() or [""])]
                    if not fields or not all(NUM.fullmatch(x) for x in fields):
                        continue
                    vals = [float(x) for x in fields]
                    if len(vals) >= 2 and all(math.isfinite(x) for x in vals):
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
                self.current.update(columns=self.columns[:], pressure_col=self.columns[pi])
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
                t = c["t"]; p = c["cols"].get(c["pressure_col"])
                direction = None
                if p and len(p) > 10:
                    a = [x for x in p[-10:] if x is not None]
                    direction = "down" if len(a) > 1 and a[-1] > a[0] + 0.3 else "up" if len(a) > 1 and a[-1] < a[0] - 0.3 else "hold"
                return {"started": c["started"], "ended": c.get("ended"), "n": len(t), "n_raw": c["n_raw"], "max_p": round(c["max_p"], 1),
                        "direction": direction, "depth_like": c["depth_like"], "t": t[:], "cols": {k: v[:] for k, v in c["cols"].items()},
                        "columns": c["columns"][:], "pressure_col": c["pressure_col"], "end_reason": c.get("end_reason")}
            return {"port": self.port, "columns": self.columns[:], "pressure_col": self.columns[self.pcol] if self.pcol >= 0 else None,
                    "packets": self.packets, "last_packet_age_s": round(now - self.last_t, 1) if self.last_t else None,
                    "rate_hz": round(len(self.times) / 30.0, 2), "source": self.last_src, "raw": self.raw[-8:],
                    "current": cast(self.current), "last": cast(self.last)}
