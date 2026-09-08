"""Live CTD cast from Seasave's "TCP/IP Out" of converted data.

The acquisition PC (``UNDERWAY_CTD_TCP``, host:port; the ship's is
10.0.0.22:49161) serves an XML stream: on connection an
``SBE_ConvertedDataSettings`` element lists the fields (``FieldDefinition``
with ``FullName`` and ``Tag`` Field0, Field1, ...), then each scan comes as
one top-level element holding ``<FieldN>value</FieldN>`` children. Column
names come from that field list, so nothing has to be configured; the
connection is retried every few seconds while acquisition is off.

The listener follows pressure (or depth) to tell when a cast is in the
water, and keeps the cast in progress and the last completed one for
``/api/live``. The source can be changed while running (``POST /api/live``),
and the last raw scans are kept so an unexpected stream can be read off the
page. Changing the source or the field list ends the current cast; the next
in-water scan starts a new one. Saved casts retain their column definitions.
A field that is not a finite number (NaN, a flag) is kept as a gap so the
other columns stay aligned, and a scan whose pressure field does not parse is
dropped.

The cast in the water and the last one are written to ``db/live_casts.json``
every few seconds and read back when the server starts, so a restart during
a cast loses nothing.

Nothing here is required for the rest of the dashboard: with no feed the
endpoint simply reports that it is connecting (or off, an empty source).
"""

from __future__ import annotations

import json
import logging
import math
import os
import re
import socket
import threading
import time

from .config import DB_DIR

log = logging.getLogger(__name__)

DEFAULT_TCP = os.environ.get("UNDERWAY_CTD_TCP", "10.0.0.22:49161")   # Seasave's TCP/IP Out; "" for none
TCP_RETRY_S = 5
SAVE_EVERY_S = 5           # how often the casts go to disk while one is in the water
ANNOUNCE_S = 15            # Seasave sends its field list at once; a connection without one by then is stale
SILENT_S = 120             # a connection that did announce may idle (acquisition off) this long before it is reopened
IN_WATER_DBAR = 2.0        # a cast starts when pressure first exceeds this …
SURFACE_DBAR = 1.0         # … and ends after SURFACE_S below this
SURFACE_S = 60
MAX_SCANS = 40000
KEEP_HZ = 2.0              # samples kept per second (a 24 Hz feed is thinned)
NUM = re.compile(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?")
PRESSURE_NAMES = ("pressure", "prdm", "prm", "pr", "p", "depth", "depsm", "depth_m")

# Seasave's XML stream: a settings element on connection, then one closed
# top-level element per scan with <FieldN> children
XML_START = re.compile(r"<([A-Za-z_][\w.-]*)(?:\s[^>]*)?>")
XML_FIELD_DEF = re.compile(r"<FieldDefinition>(.*?)</FieldDefinition>", re.S)
XML_FIELD = re.compile(r"<(Field\d+)>\s*([^<]*?)\s*</\1>")
# short column names for Seasave's field names (matched on the start, lower
# case). "Depth, NMEA" is the echosounder's bottom depth fed in over NMEA,
# not the package's: it must not pass for pressure.
COLUMN_NAMES = (("depth, nmea", "bottom_depth_m"), ("depth", "depth_m"), ("pressure", "pressure"), ("potential temperature", "theta"), ("temperature", "temperature"),
                ("conductivity", "conductivity"), ("salinity", "salinity"), ("oxygen sat", "oxygen_sat"), ("oxygen", "oxygen"),
                ("fluorescence", "fluorescence"), ("time, elapsed", "time_s"), ("scan count", "scan"), ("latitude", "lat"),
                ("longitude", "lon"), ("beam transmission", "transmission"), ("beam attenuation", "attenuation"),
                ("bottles fired", "bottles"), ("nitrogen", "nitrogen_sat"), ("turbidity", "turbidity"), ("par", "par"),
                ("spar", "spar"), ("ph", "ph"), ("density", "sigma_t"), ("sound velocity", "sound_velocity"),
                ("descent rate", "descent_rate"), ("altimeter", "altimeter"))


def xml_columns(settings: str) -> tuple[list[str], list[str], list[str]]:
    """(column names, field tags, Seasave's field names) from an
    SBE_ConvertedDataSettings body."""
    cols, tags, names = [], [], []
    for block in XML_FIELD_DEF.findall(settings):
        full = re.search(r"<FullName>(.*?)</FullName>", block, re.S)
        tag = re.search(r"<Tag>(.*?)</Tag>", block, re.S)
        name = (full.group(1) if full else "").strip()
        low = name.lower()
        short = next((s for k, s in COLUMN_NAMES if low.startswith(k)), None) \
            or re.sub(r"[^a-z0-9]+", "_", low.split("[")[0]).strip("_") or f"field{len(cols)}"
        base, n = short, 2
        while short in cols:
            short = f"{base}_{n}"; n += 1
        cols.append(short); tags.append((tag.group(1).strip() if tag else f"Field{len(tags)}")); names.append(name)
    return cols, tags, names


class LiveCTD:
    def __init__(self, tcp: str = ""):
        self.lock = threading.Lock()
        self.tcp = ""                           # Seasave TCP/IP Out, host:port ("" = off)
        self.tcp_state = "off"
        self.columns: list[str] = []
        self._xml_tags: list[str] = []          # field tags in column order, from the settings element
        self._xml_names: list[str] = []         # Seasave's names for them
        self.packets = 0
        self.last_t = 0.0
        self.times: list[float] = []            # scan times, last 30 s, for the rate
        self.raw: list[str] = []                # last raw scans (trimmed)
        self.current: dict | None = None        # the cast in the water
        self.last: dict | None = None           # the last completed cast
        self.surface_since: float | None = None
        self._last_kept = 0.0
        self._saved = 0.0
        self._stop = threading.Event()
        self._restore()
        self.configure(tcp)
        self._thread = threading.Thread(target=self._tcp_loop, daemon=True)
        self._thread.start()

    # ------------------------------------------------------------ config
    def configure(self, tcp: str | None) -> None:
        """Point the listener at ``host:port`` ("" for none); None leaves it."""
        if tcp is None:
            return
        if not isinstance(tcp, str):
            raise ValueError("Source must be host:port")
        tcp = tcp.strip()
        if tcp and not re.fullmatch(r"[\w.-]+:\d{1,5}", tcp):
            raise ValueError("Source must be host:port (empty for none)")
        with self.lock:
            if self._stop.is_set():
                raise ValueError("Listener is closed")
            if tcp == self.tcp:
                return
            self._end_cast(time.time(), "configuration changed")
            self._xml_tags, self._xml_names, self.columns = [], [], []
            self.tcp, self.tcp_state = tcp, ("connecting" if tcp else "off")
            log.info("live CTD: Seasave source %s", tcp or "off")

    def close(self) -> None:
        self._stop.set()
        self._thread.join(timeout=TCP_RETRY_S + 3)
        with self.lock:
            self._save(force=True)

    # ------------------------------------------------------------ persistence
    def _casts_path(self):
        return DB_DIR / "live_casts.json"

    def _restore(self) -> None:
        try:
            p = self._casts_path()
            if not p.is_file():
                return
            d = json.loads(p.read_text())
            self.current, self.last = d.get("current"), d.get("last")
            if self.current is not None:            # a cast under way when the server stopped: it goes on if scans resume
                self.surface_since = None
            log.info("live CTD: casts restored (%s)", ", ".join(k for k in ("current", "last") if d.get(k)) or "none")
        except Exception as e:                      # noqa: BLE001
            log.warning("live CTD: casts not restored: %s", e)

    def _save(self, force: bool = False) -> None:
        """Under the lock: the casts to disk, at most every SAVE_EVERY_S."""
        now = time.time()
        if not force and now - self._saved < SAVE_EVERY_S:
            return
        self._saved = now
        try:
            DB_DIR.mkdir(parents=True, exist_ok=True)
            tmp = self._casts_path().with_suffix(".tmp")
            tmp.write_text(json.dumps({"current": self.current, "last": self.last}))
            tmp.replace(self._casts_path())
        except Exception as e:                      # noqa: BLE001
            log.warning("live CTD: casts not saved: %s", e)

    def _end_cast(self, now: float, reason: str) -> None:
        """Under the lock: the cast in the water becomes the last one."""
        if self.current is not None:
            self.current.update(ended=now, end_reason=reason)
            self.last, self.current = self.current, None
            self._save(force=True)
        self.surface_since = None

    @property
    def pcol(self) -> int:
        for i, c in enumerate(self.columns):
            if c.lower() in PRESSURE_NAMES:
                return i
        return -1

    # ------------------------------------------------------------ receive
    def _tcp_loop(self) -> None:
        while not self._stop.is_set():
            with self.lock:
                target = self.tcp
            if not target:
                self._stop.wait(0.2)
                continue
            host, _, port = target.rpartition(":")
            try:
                s = socket.create_connection((host, int(port)), timeout=5)
                s.settimeout(2.0)
            except OSError as e:
                with self.lock:
                    if self.tcp == target:
                        self.tcp_state = f"connecting ({e.strerror or e})"
                self._stop.wait(TCP_RETRY_S)
                continue
            with self.lock:
                self.tcp_state = "connected"
            buf = ""
            last_rx = time.time()
            try:
                while not self._stop.is_set():
                    with self.lock:
                        if self.tcp != target:
                            break
                    try:
                        data = s.recv(65536)
                    except socket.timeout:
                        self._tick(time.time())
                        # a socket Seasave has gone quiet on (it stopped without closing) is
                        # dropped and reopened, which also asks for the field list afresh:
                        # quickly when nothing at all has arrived, patiently between casts
                        with self.lock:
                            announced = bool(self._xml_tags)
                        limit = SILENT_S if announced else ANNOUNCE_S
                        if time.time() - last_rx > limit:
                            log.info("live CTD: nothing from %s for %d s, reconnecting", target, limit)
                            break
                        continue
                    if not data:
                        break
                    last_rx = time.time()
                    buf = self._xml_feed(buf + data.decode("latin-1", errors="replace"), time.time())
                    if len(buf) > 400000:           # a stream that never closes an element must not grow forever
                        buf = buf[-100000:]
            except OSError:
                pass
            finally:
                s.close()
            with self.lock:
                if self.tcp == target:
                    self.tcp_state = "connecting"
            self._stop.wait(TCP_RETRY_S)

    def _xml_feed(self, buf: str, now: float) -> str:
        """Consume every complete top-level element at the front of ``buf``
        (a settings element or a scan); return what is left, which starts
        with an element still arriving."""
        while True:
            i = buf.find("<")
            if i < 0:
                buf = ""
                break
            buf = buf[i:]
            if buf.startswith("<?"):                     # the XML prolog
                j = buf.find("?>")
                if j < 0:
                    break
                buf = buf[j + 2:]
                continue
            m = XML_START.match(buf)
            if not m:                                     # not a start tag: skip to the next one
                j = buf.find("<", 1)
                if j < 0:
                    break
                buf = buf[j:]
                continue
            name = m.group(1)
            close = f"</{name}>"
            j = buf.find(close, m.end())
            if j < 0:
                break                                     # incomplete: wait for more
            self._xml_element(name, buf[m.end():j], buf[:j + len(close)], now)
            buf = buf[j + len(close):]
        self._tick(now)
        return buf

    def _xml_element(self, name: str, body: str, raw: str, now: float) -> None:
        if name == "SBE_ConvertedDataSettings":
            cols, tags, names = xml_columns(body)
            with self.lock:
                if cols and cols != self.columns:
                    self._end_cast(now, "Seasave field list changed")
                    self.columns = cols
                    log.info("live CTD: Seasave fields %s", ",".join(cols))
                self._xml_tags, self._xml_names = tags, names
            return
        fields = XML_FIELD.findall(body)
        if not fields:
            return
        with self.lock:
            tags = self._xml_tags
            if not tags:
                return
            vals: list[float | None] = [None] * len(tags)
            for tag, text in fields:
                if tag in tags:
                    vals[tags.index(tag)] = float(text) if NUM.fullmatch(text) and math.isfinite(float(text)) else None
            self.packets += 1
            self.last_t = now
            self.times.append(now)
            self.times = [t for t in self.times if now - t < 30]
            self.raw.append(raw[:300].replace("\r", "\\r").replace("\n", "\\n"))
            self.raw = self.raw[-20:]
            pi = self.pcol
            if 0 <= pi < len(vals) and vals[pi] is not None:
                self._scan(now, vals)
                if self.current is not None:
                    self._save()

    def _scan(self, now: float, vals: list[float | None]) -> None:
        pi = self.pcol
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
                self._save(force=True)
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
            return {"tcp": self.tcp, "tcp_state": self.tcp_state, "fields": self._xml_names[:],
                    "columns": self.columns[:], "pressure_col": self.columns[self.pcol] if self.pcol >= 0 else None,
                    "no_pressure": bool(self._xml_tags) and self.pcol < 0,
                    "packets": self.packets, "last_packet_age_s": round(now - self.last_t, 1) if self.last_t else None,
                    "rate_hz": round(len(self.times) / 30.0, 2), "raw": self.raw[-8:],
                    "current": cast(self.current), "last": cast(self.last)}
