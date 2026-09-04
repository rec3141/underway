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
    Variable("Ship speed (kn)", "kn",
             (r"^posmv — speed", r"^cnav — speed", r"speed \(knt\)")),
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
    learn_hours: float = 48         # model is fitted on the most recent span
    min_feature_cover: float = 0.60 # features below this finite fraction are dropped
    winsor: tuple[float, float] = (0.005, 0.995)
    pca_variance: float = 0.95      # retained variance for the T² subspace

SURPRISE = SurpriseConfig()

# ---------------------------------------------------------------- locations

import os  # noqa: E402  (kept with the settings that use it)
from pathlib import Path  # noqa: E402

DATA_ROOT = Path(os.environ.get("UNDERWAY_DATA_ROOT", "/mnt/ship/Data"))    # FULL_CSV/<leg>/, Rosette/<leg>/Logs/
SHARE_ROOT = Path(os.environ.get("UNDERWAY_SHARE_ROOT", "/mnt/ship/Share"))  # <year>/<leg>/ for archived seasons
# per-leg SQLite stores; derived data, safe to delete
DB_DIR = Path(os.environ.get("UNDERWAY_DB_DIR", Path(__file__).resolve().parents[1] / "db"))

# ---------------------------------------------------------------- misc

LOCAL_TZ = "America/Toronto"
MAP_KM_STEP = 5.0     # the track never thins to fewer than one point per this many km
QUANTILE_LIMITS = (0.05, 0.95)      # colour scales clamp to these to survive outliers
FILE_PATTERN = re.compile(r"^ACSD_(\d{8})\.csv$")
