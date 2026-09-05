"""Dashboard configuration: what to plot, over which windows, and how to find
the source columns.

ACSD headers differ between legs, so variables are not bound to fixed column
names. Each entry lists regex candidates tried in order against the canonical
column keys present in the data; the first match wins and the resolution is
recorded in the output so a header change is visible rather than silent.
"""

from __future__ import annotations

import re
import unicodedata
from dataclasses import dataclass, field

# ---------------------------------------------------------------- columns

def canonical_key(instrument: str, variable: str) -> str:
    """Normalise a header pair to a stable lookup key.

    Files arrive with inconsistent whitespace and occasionally mis-encoded
    unicode (e.g. ``W/mÂ²``), so matching is done on an NFKC, whitespace
    collapsed, lower-cased form. The display name keeps the original text.
    """
    def norm(s: str) -> str:
        s = unicodedata.normalize("NFKC", s)
        s = s.replace("Â", "")                      # stray byte from cp1252/utf-8 mix-ups
        s = re.sub(r"\s+", " ", s).strip()
        return s
    inst, var = norm(instrument), norm(variable)
    return f"{inst} — {var}".lower() if inst else var.lower()


def display_name(instrument: str, variable: str) -> str:
    inst = re.sub(r"\s+", " ", instrument).strip()
    var = re.sub(r"\s+", " ", variable).strip().replace("Â", "")
    return f"{inst} — {var}" if inst else var


# ---------------------------------------------------------------- variables

@dataclass(frozen=True)
class Variable:
    name: str                       # panel title
    unit: str
    candidates: tuple[str, ...]     # regexes, tried in order, against canonical keys
    derived: bool = False           # computed rather than read from a column
    log_ok: bool = False            # offer a log10 toggle (spiky, positive data)
    circular: bool = False          # degrees on a compass; plotted as points, not lines
    cmap: str = "Viridis"

    def resolve(self, keys: list[str]) -> str | None:
        for pat in self.candidates:
            rx = re.compile(pat, re.IGNORECASE)
            hits = [k for k in keys if rx.search(k)]
            if hits:
                return hits[0]
        return None


VARIABLES: tuple[Variable, ...] = (
    Variable("Surprise (−log10 p)", "", (), derived=True, cmap="Magma"),
    Variable("SST (°C)", "°C",
             (r"^tsg — hull temperature", r"^tsg — .*temperature", r"sea.*surface.*temp", r"hull temperature")),
    Variable("Salinity (PSU)", "PSU",
             (r"^tsg — salinity", r"salinity \(psu\)")),
    Variable("TSG line warming (°C)", "°C", (), derived=True),
    Variable("TSG flow (V)", "V", (), derived=True),
    Variable("Fluorescence (µg/L)", "µg/L",
             (r"^tsg — fluorescence", r"fluorescence"), log_ok=True),
    Variable("Oxygen (mL/L)", "mL/L",
             (r"^tsg — oxygen", r"oxygene?")),
    Variable("Short-wave radiation (W/m²)", "W/m²",
             (r"^ats_portside — short wave", r"^ats_starboard — short wave", r"short wave radiation")),
    Variable("Bottom depth (m)", "m",
             (r"^multibeam — bottom depth", r"^ek60 — bottom depth", r"bottom depth"), log_ok=True),
    Variable("Air temperature (°C)", "°C",
             (r"^avos — air temperature", r"^ats_mettower — air temperature", r"air temperature")),
    Variable("Atmospheric pressure (hPa)", "hPa",
             (r"^avos — atmospheric pressure", r"^ats_mettower — atmospheric pressure", r"pressure \(hpa\)")),
    Variable("True wind direction (°)", "°",
             (r"^avos — true wind direction", r"true wind direction"), circular=True, cmap="Phase"),
    Variable("Relative wind speed (kn)", "kn",
             (r"^avos — relative wind speed", r"^ats_mettower — relative wind speed", r"wind speed")),
    Variable("Heading (°)", "°", (r"^posmv — heading", r"^gc — heading", r"heading \(deg\)"), circular=True, cmap="Phase"),
    Variable("Ship speed (kn)", "kn",
             (r"^posmv — speed", r"^cnav — speed", r"speed \(knt\)")),
    Variable("Sea state · 4σ heave (m)", "m", (), derived=True),
    Variable("Roll & pitch RMS (°)", "°", (), derived=True),
    Variable("Time elapsed (h)", "h", (), derived=True),
    Variable("Distance travelled (km)", "km", (), derived=True),
)

# Position sources in order of preference; per row the first finite pair wins.
POSITION_CANDIDATES: tuple[tuple[str, str], ...] = (
    (r"^posmv — latitude", r"^posmv — longitude"),
    (r"^cnav — latitude", r"^cnav — longitude"),
    (r"^gc — latitude", r"^gc — longitude"),
    (r"latitude", r"longitude"),
)

# Salinometer minus hull temperature: how much the sample warms on its way
# through the ship. The FULL_CSV carries no TSG flow rate; a rising or
# erratic warming is the proxy for a slowing or bubbly intake line.
LINE_WARMING: tuple[tuple[str, ...], tuple[str, ...]] = ((r"^tsg — salinometer temperature",), (r"^tsg — hull temperature",))
# TSG intake flow (from the TSG's own files, a sensor voltage): below this the
# pump is off or the line is choked, and the TSG readings are the line's, not
# the sea's — the surprise model drops them for those minutes
LOW_FLOW_V = 0.5
# Ship motion from the POSMV (5 s cadence): 4σ of heave over MOTION_WINDOW is
# the significant-wave-height proxy, and the RMS of the roll and pitch
# departures from their running means the ship's tilt energy.
MOTION = {"roll": (r"^posmv — roll",), "pitch": (r"^posmv — pitch",), "heave": (r"^posmv — heave",)}
MOTION_WINDOW = "10min"
# Window-filled derived variables: computed per window, not in the record
WINDOW_FILLED = ("Time elapsed (h)", "Distance travelled (km)")

# Features feeding the surprise model, in the same resolution style.
SURPRISE_FEATURES: tuple[tuple[str, ...], ...] = (
    (r"^tsg — hull temperature", r"hull temperature"),
    (r"^tsg — salinity",),
    (r"^tsg — fluorescence",),
    (r"^tsg — oxygen",),
    (r"^avos — air temperature",),
)

# ---------------------------------------------------------------- windows

@dataclass(frozen=True)
class Window:
    label: str
    hours: float                    # span back from the latest observation
    step_s: int                     # sampling interval delivered to the browser

# Spans reach back across legs; empty stretches between legs collapse to a
# single gap marker, so long windows stay small.
WINDOWS: tuple[Window, ...] = (
    Window("1h", 1, 10),
    Window("3h", 3, 10),
    Window("6h", 6, 30),
    Window("12h", 12, 60),
    Window("24h", 24, 60),
    Window("48h", 48, 120),
    Window("72h", 72, 180),
    Window("1w", 24 * 7, 300),
    Window("1mo", 24 * 30, 900),
    Window("2mo", 24 * 61, 1800),
    Window("4mo", 24 * 122, 3600),
    Window("1y", 24 * 365, 3600),
    Window("2y", 24 * 730, 7200),
    Window("4y", 24 * 1461, 7200),
)
DEFAULT_WINDOW = "6h"

# ---------------------------------------------------------------- surprise

@dataclass(frozen=True)
class SurpriseConfig:
    min_feature_cover: float = 0.05 # features below this finite fraction over the record are dropped
    winsor: tuple[float, float] = (0.005, 0.995)
    ridge: float = 0.01             # variance floor (in IQR² units) added to every scale's covariance
    cap: float = 6.0                # −log10 p ceiling per scale
    log_floor: float = 0.01         # fluorescence is log-transformed above this

SURPRISE = SurpriseConfig()
# half-lives of the exponentially weighted references, roughly log-spaced
SURPRISE_SCALES: tuple[tuple[str, int], ...] = (("15 min", 15), ("1 h", 60), ("3 h", 180), ("12 h", 720), ("48 h", 2880))
SURPRISE_NAME = "Surprise (−log10 p)"

def surprise_scale_name(label: str) -> str:
    return f"Surprise · {label}"

# one variable per scale, after the combined score; the page shows a single
# surprise panel and picks the scale that matches the span shown
VARIABLES = VARIABLES[:1] + tuple(Variable(surprise_scale_name(l), "", (), derived=True) for l, _ in SURPRISE_SCALES) + VARIABLES[1:]

# ---------------------------------------------------------------- locations

import os  # noqa: E402  (kept with the settings that use it)
from pathlib import Path  # noqa: E402

DATA_ROOT = Path(os.environ.get("UNDERWAY_DATA_ROOT", "/mnt/ship/Data"))    # FULL_CSV/<leg>/, Rosette/<leg>/Logs/
SHARE_ROOT = Path(os.environ.get("UNDERWAY_SHARE_ROOT", "/mnt/ship/Share"))  # <year>/<leg>/ for archived seasons
# per-leg SQLite stores; derived data, safe to delete
DB_DIR = Path(os.environ.get("UNDERWAY_DB_DIR", Path(__file__).resolve().parents[1] / "db"))

# ---------------------------------------------------------------- ship intranet
# pages on the ship's own web server, linked from the dashboard (LAN only)
INTRANET_BASE = os.environ.get("UNDERWAY_INTRANET", "http://10.0.0.2")
INTRANET_LINKS: tuple[tuple[str, str], ...] = (("Schedule", "Schedule.html"), ("Event log", "Eventlog.html"), ("Rosette log", "RosetteLog.html"),
                                                ("Live", "live.html"), ("Tutorial", "Tuto.html"))

# ---------------------------------------------------------------- google calendar
GCAL = {
    "schedule": {"id": "d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d@group.calendar.google.com",
                 "label": "Amundsen Schedule", "colour": "#5cc8ff"},
    "surprise": {"id": "7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb@group.calendar.google.com",
                 "label": "Underway Updates", "colour": "#ffb454"},
}
GCAL_CREDS = Path(os.environ.get("UNDERWAY_GCAL_CREDS", "~/.config/underway/gcal-sa.json")).expanduser()  # service account key; never in the repo
GCAL_SYNC_MINUTES = 10        # the feed cache is refreshed at most this often
GCAL_SINCE = "2026-01-01"     # event-log operations before this were pushed by the R scheduler
GCAL_MAX_CALLS = 40           # API requests per push run, so a backlog catches up over a few runs
SURPRISE_ALERT_SCALE = "3 h"  # the scale watched for calendar alerts …
SURPRISE_ALERT = 2.0          # … and the level above which an episode starts

# ---------------------------------------------------------------- misc

LOCAL_TZ = "America/Toronto"
MAP_KM_STEP = 5.0     # the track never thins to fewer than one point per this many km
QUANTILE_LIMITS = (0.05, 0.95)      # colour scales clamp to these to survive outliers
FILE_PATTERN = re.compile(r"^ACSD_(\d{8})\.csv$")
