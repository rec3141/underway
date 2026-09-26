"""Paths and constants. Every path can be overridden from the environment.

The report reads the ship's shares and the underway dashboard's stores and
never writes to either: drafts, uploaded logsheets and built reports live
under ``STATE_DIR``.
"""

from __future__ import annotations

import os
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _env(*names: str, default: str) -> Path:
    """The first of ``names`` set in the environment, else ``default``."""
    return Path(next((os.environ[n] for n in names if os.environ.get(n)), default))


# The dashboard's own variables are honoured, so one site file serves both.
DATA_ROOT = _env("CRUISE_DATA_ROOT", "UNDERWAY_DATA_ROOT", default="/mnt/ship/Data")
SHARE_ROOT = _env("CRUISE_SHARE_ROOT", "UNDERWAY_SHARE_ROOT", default="/mnt/ship/Share")
# The underway dashboard's per-leg SQLite stores and parsed-cast cache (read only).
UNDERWAY_DB_DIR = _env("CRUISE_UNDERWAY_DB", "UNDERWAY_DB_DIR", default="/data/underway_server/db")
# The basemap the dashboard ships with (Natural Earth, clipped to the Arctic).
GEO_DIR = _env("CRUISE_GEO_DIR", default=str(HERE.parent / "dashboard" / "static" / "geo"))

TEMPLATE = _env("CRUISE_TEMPLATE", default=str(
    SHARE_ROOT / "Guidelines, Templates & Forms" / "Cruise Report Template_2026.docx"))

STATE_DIR = _env("CRUISE_STATE_DIR", default=str(Path.home() / ".local/share/cruise-report/state"))
# underway-report.service sets it to $UNDERWAY_HOME/report.

WORD_LIMIT = 3000

# Underway values are averaged over this many seconds either side of an event.
UNDERWAY_HALF_WINDOW_S = 150
