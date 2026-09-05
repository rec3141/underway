"""Discover cruise legs on the ship's shares.

Each leg keeps its own SQLite store so ingest stays incremental per leg; the
dashboard itself is built from all legs together.

Discovery raises rather than returning an empty list when the shares are not
there. The ship's SMB mounts drop routinely, and a silent empty result would let
a scheduled build publish an empty dashboard over a good one.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from pathlib import Path

from .config import DATA_ROOT, DB_DIR, FILE_PATTERN, SHARE_ROOT

log = logging.getLogger(__name__)

LEG_RE = re.compile(r"^(\d{4})_LEG_(\d{2})$")


class RootsUnavailable(RuntimeError):
    """The data shares are not mounted, or hold no legs.

    Distinct from "no new files": this means we cannot see the source data at
    all, so any build from it would be wrong rather than merely stale.
    """


@dataclass
class Leg:
    id: str                                 # e.g. 2026_LEG_03
    year: int
    number: int
    indirs: list[Path] = field(default_factory=list)
    stations: Path | None = None
    files: int = 0
    bytes: int = 0
    first_date: str | None = None
    last_date: str | None = None
    newest_mtime: float = 0.0
    live: bool = False

    @property
    def label(self) -> str:
        return f"{self.year} Leg {self.number}"

    @property
    def db(self) -> Path:
        return DB_DIR / f"{self.id}.db"

    def meta(self) -> dict:
        return {"id": self.id, "label": self.label, "year": self.year, "number": self.number,
                "live": self.live, "files": self.files, "mb": round(self.bytes / 1e6),
                "first_date": self.first_date, "last_date": self.last_date,
                "has_stations": self.stations is not None, "inputs": [str(p) for p in self.indirs]}


def _scan_dir(d: Path) -> tuple[int, int, str | None, str | None, float]:
    files = sorted(p for p in d.iterdir() if FILE_PATTERN.match(p.name))
    if not files:
        return 0, 0, None, None, 0.0
    size = sum(p.stat().st_size for p in files)
    mt = max(p.stat().st_mtime for p in files)
    d0 = FILE_PATTERN.match(files[0].name).group(1)
    d1 = FILE_PATTERN.match(files[-1].name).group(1)
    return len(files), size, d0, d1, mt


def discover() -> list[Leg]:
    legs: dict[str, Leg] = {}

    def add(d: Path):
        m = LEG_RE.match(d.name)
        if not m or not d.is_dir():
            return
        n, size, d0, d1, mt = _scan_dir(d)
        if n == 0:
            return
        leg = legs.setdefault(d.name, Leg(d.name, int(m.group(1)), int(m.group(2))))
        leg.indirs.append(d)
        leg.files += n
        leg.bytes += size
        leg.first_date = min(filter(None, [leg.first_date, d0]))
        leg.last_date = max(filter(None, [leg.last_date, d1]))
        leg.newest_mtime = max(leg.newest_mtime, mt)

    full = DATA_ROOT / "FULL_CSV"                   # current season
    have_full, have_share = full.is_dir(), SHARE_ROOT.is_dir()
    if not (have_full or have_share):
        raise RootsUnavailable(
            "no data shares are reachable — neither\n"
            f"    {full}   (UNDERWAY_DATA_ROOT={DATA_ROOT})\n"
            f"    {SHARE_ROOT}   (UNDERWAY_SHARE_ROOT={SHARE_ROOT})\n"
            "is a directory. The ship's SMB mounts are probably down, or these "
            "defaults are wrong for this machine (they assume Linux /mnt/ship; "
            "macOS mounts under /Volumes). Set UNDERWAY_DATA_ROOT and "
            "UNDERWAY_SHARE_ROOT to override."
        )
    # one of the two missing is survivable — the current season can exist without
    # the archive, and vice versa — but it is worth saying so out loud.
    if not have_full:
        log.warning("current-season root missing: %s", full)
    if not have_share:
        log.warning("archive root missing: %s", SHARE_ROOT)

    if have_full:
        for d in sorted(full.iterdir()):
            add(d)
    if have_share:                                 # archived seasons: Share/<year>/<leg>/
        for ydir in sorted(SHARE_ROOT.iterdir()):
            if ydir.is_dir() and re.fullmatch(r"\d{4}", ydir.name):
                for d in sorted(ydir.iterdir()):
                    add(d)

    for leg in legs.values():
        cand = DATA_ROOT / "Rosette" / leg.id / "Logs" / f"{leg.year}_{leg.number:02d}_CTD_logbook.csv"
        if cand.is_file():
            leg.stations = cand

    out = sorted(legs.values(), key=lambda l: (l.year, l.number))
    if not out:
        raise RootsUnavailable(
            "the shares are reachable but contain no legs matching YYYY_LEG_NN "
            f"with {FILE_PATTERN.pattern} files (looked under {full} and "
            f"{SHARE_ROOT}). An empty mount point looks exactly like this."
        )

    # Observation dates, not copy/reprocessing times, identify the latest leg.
    # A shared boundary day belongs to the later leg for this display marker.
    max(out, key=lambda l: (l.last_date or "", l.year, l.number)).live = True
    return out
