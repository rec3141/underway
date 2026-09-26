"""Rosette sheets: which team drew water from which bottle.

``Data/Rosette/<leg>/Logs/RosetteSheet_*.xlsx`` is filled on the rosette
console, one workbook per cast. Its header block names the station, the
event label (the event log's key), the cast number and the observer's
weather (cloud cover, sea state, ice); the bottle table lists each Niskin's
target and trip depth and, in one column per team, what that team drew
(usually litres).

Team columns are the header cells between "Trip Pressure" and "Comments".
Their names are typed per sheet and vary ("DFONL " / "DFONL", misspellings),
so they are kept as written, trimmed; the page lets a participant tick every
spelling that is theirs. Trace-metal sheets usually have no team columns.

The observer's cloud-cover and sea-state entries are kept as the codes
typed: the sheet's own lookup (0 Clear … 3 Overcast) does not match the
0–9 values people enter, so the report does not interpret them.

Cells are found by their labels, not fixed positions: the sheets on
different legs have a different number of team columns.
"""

from __future__ import annotations

import logging
import math
import re
from functools import lru_cache
from pathlib import Path

import openpyxl

from .config import DATA_ROOT

log = logging.getLogger(__name__)


def _clean(v) -> str | None:
    if v is None:
        return None
    s = " ".join(str(v).split())
    return s or None


def _num(v) -> float | None:
    try:
        f = float(v)
    except (TypeError, ValueError):
        return None
    return f if math.isfinite(f) else None


def _find(rows: list[tuple], pattern: str, *, col_max: int = 40) -> tuple[int, int] | None:
    rx = re.compile(pattern, re.I)
    for r, row in enumerate(rows):
        for c, v in enumerate(row[:col_max]):
            if isinstance(v, str) and rx.search(v):
                return r, c
    return None


def _right(rows, at, skip_blank=True, n=12):
    """The first value to the right of a label cell, not reaching the next label."""
    if at is None:
        return None
    r, c = at
    for v in rows[r][c + 1:c + 1 + n]:
        if isinstance(v, str) and v.strip().endswith(":"):
            return None
        if v is not None and (not skip_blank or str(v).strip()):
            return v
    return None


def _clock(rows, at) -> str | None:
    """'Begin: 13 : 21' laid out across cells -> '13:21'."""
    if at is None:
        return None
    r, c = at
    nums = [v for v in rows[r][c + 1:c + 8] if _num(v) is not None]
    if len(nums) >= 2:
        return f"{int(_num(nums[0])):02d}:{int(_num(nums[1])):02d}"
    return None


def parse(path: Path) -> dict | None:
    wb = openpyxl.load_workbook(path, data_only=True, read_only=True)
    try:
        ws = wb.worksheets[0]
        rows = [tuple(r) for r in ws.iter_rows(max_row=60, max_col=40, values_only=True)]
    finally:
        wb.close()

    btl = _find(rows, r"^btl no")
    if btl is None:
        return None
    hr, bc = btl
    header = rows[hr]
    trip = _find([header], r"trip")
    comments = _find([header], r"^comments")
    tc = trip[1] if trip else bc + 4
    cc = comments[1] if comments else tc + 14
    teams = [(c, _clean(header[c])) for c in range(tc + 1, cc) if _clean(header[c])]
    target_c = (_find([header], r"target") or (0, bc + 2))[1]
    light_c = (_find([header], r"% ?light") or (0, bc + 1))[1]

    bottles = []
    for row in rows[hr + 1:hr + 40]:
        n = _num(row[bc]) if bc < len(row) else None
        if n is None:
            if bottles:
                break
            continue
        draws = {name: row[c] for c, name in teams if row[c] not in (None, "")}
        bottles.append({
            "bottle": int(n),
            "target": _clean(row[target_c]),
            "light_pct": _clean(row[light_c]),
            "trip_db": _num(row[tc]),
            "draws": {k: (_num(v) if _num(v) is not None else _clean(v)) for k, v in draws.items()},
            "comment": _clean(row[cc]) if cc < len(row) else None,
        })

    wind = _find(rows, r"^wind")
    tair = _find(rows, r"^tair")
    return {
        "file": path.name,
        "label": _clean(_right(rows, _find(rows, r"^event\s*:"))),
        "station": _clean(_right(rows, _find(rows, r"^station\s*:"))),
        "cast": _clean(_right(rows, _find(rows, r"^cast number"))),
        "cast_name": _clean(_right(rows, _find(rows, r"^cast name"))),
        "entered_by": _clean(_right(rows, _find(rows, r"^entered by"))),
        "observed": {
            "pressure_hpa": _num(_right(rows, _find(rows, r"^pa\s*\("))),
            "air_c": _num(_right(rows, tair)),
            "humidity_pct": _num(rows[tair[0]][tair[1] + 5]) if tair else None,
            "wind_kn": _num(_right(rows, wind)),
            "wind_dir_deg": _num(rows[wind[0]][wind[1] + 5]) if wind else None,
            "cloud_cover": _clean(_right(rows, _find(rows, r"^cloud cover\s*:"))),
            "sea_state": _clean(_right(rows, _find(rows, r"^sea state\s*:"))),
            "ice": _clean(_right(rows, _find(rows, r"^ice\s*:"))),
            "day_night": _clean(_right(rows, _find(rows, r"^day/night"))),
        },
        "begin": _clock(rows, _find(rows, r"^begin")),
        "bottom": _clock(rows, _find(rows, r"^bottom\s*:")),
        "end": _clock(rows, _find(rows, r"^end\s*:")),
        "bottom_depth_m": _num(_right(rows, _find(rows, r"^bottom depth"))),
        "comments": _clean(_right(rows, _find(rows, r"^comments\s*:"))),
        "teams": [name for _, name in teams],
        "bottles": bottles,
    }


# The sheet's ice list (French CIS terms) in English.
ICE_TERMS = {
    "eau libre": "open water",
    "banquise très lâche": "very open drift ice",
    "banquise lâche": "open drift ice",
    "banquise serrée": "close pack ice",
    "banquise très serrée": "very close pack ice",
    "banquise compacte": "compact ice",
}


def ice_english(text: str | None) -> str | None:
    """'4-6/10 Banquise lâche' -> '4-6/10 open drift ice'; other entries as written."""
    if not text:
        return None
    m = re.match(r"^\s*([\d/+\-–]+/10)?\s*(.*)$", text)
    conc, words = (m.group(1) or "").strip(), m.group(2).strip()
    eng = ICE_TERMS.get(words.casefold())
    if eng == "open water":
        return "open water (under 1/10)"
    if eng:
        return f"{conc} {eng}".strip()
    if text.strip() == "-99" or re.fullmatch(r"\s*[\d.]+\s*", text):
        return None             # a bare number: the sheet does not say which scale
    return text


def _files(leg: str) -> list[Path]:
    d = DATA_ROOT / "Rosette" / leg / "Logs"
    if not d.is_dir():
        return []
    return sorted(p for p in d.glob("RosetteSheet_*.xlsx") if not p.name.startswith("~$"))


def sheets(leg: str) -> list[dict]:
    files = _files(leg)
    stamp = tuple((p.name, p.stat().st_mtime) for p in files)
    return list(_sheets(leg, stamp))


@lru_cache(maxsize=8)
def _sheets(leg: str, stamp: tuple) -> tuple[dict, ...]:
    out = []
    for name, _ in stamp:
        p = DATA_ROOT / "Rosette" / leg / "Logs" / name
        try:
            s = parse(p)
        except Exception as e:                  # one bad workbook must not hide the rest
            log.warning("%s: %s", p, e)
            continue
        if s:
            out.append(s)
    return tuple(out)


def _key(name: str) -> str:
    """Letters only, lower case: 'Archambault0' and 'archambault' -> 'archambault'."""
    return re.sub(r"[^a-z]", "", name.casefold())


def _close(a: str, b: str) -> bool:
    """One edit apart, or two adjacent letters swapped ('Archmabault')."""
    if a == b:
        return True
    if abs(len(a) - len(b)) > 1 or min(len(a), len(b)) < 5:
        return False
    if len(a) == len(b):
        diff = [i for i in range(len(a)) if a[i] != b[i]]
        return len(diff) == 1 or (len(diff) == 2 and diff[1] == diff[0] + 1
                                  and a[diff[0]] == b[diff[1]] and a[diff[1]] == b[diff[0]])
    short, long_ = sorted((a, b), key=len)
    return any(long_[:i] + long_[i + 1:] == short for i in range(len(long_)))


def _raw_counts(leg: str) -> dict[str, int]:
    counts: dict[str, int] = {}
    for s in sheets(leg):
        for t in s["teams"]:
            if any(t in b["draws"] for b in s["bottles"]):
                counts[t] = counts.get(t, 0) + 1
    return counts


# Spellings of one team that no spelling rule links, checked by hand: a short
# name and the same name with the PI in brackets. None of these pairs heads
# two columns on one sheet.
# Keys and values are compared letters-only (see ``_key``); the value is the
# spelling the team is shown under when it is on the leg's sheets.
SAME_TEAM = {
    "obriencote": "obrien",
    "kim": "kimchailloux",
    "chaillou": "kimchailloux",
    "seb": "sebastienmuller",
    "margaux": "margauxbroeder",
}


def canonical(leg: str) -> dict[str, str]:
    """Each team spelling on the leg's sheets -> the name it is merged under.

    Only obvious slips merge: case, stray digits or punctuation, one letter
    added, dropped or changed, or two letters swapped, on names of five
    letters or more. Different words stay apart ("obrien" and "obrien cote"),
    and so do two names that head columns on the same sheet ("DFONL" and
    "DFONL_2"): they are separate columns, not one team typed twice.
    The merged name is the spelling used on the most casts. ``SAME_TEAM``
    adds the merges no spelling rule would make.
    """
    counts = _raw_counts(leg)
    together = {frozenset((a, b)) for s in sheets(leg) for a in s["teams"] for b in s["teams"] if a != b}
    groups: list[list[str]] = []
    for name in sorted(counts, key=lambda n: -counts[n]):
        k = _key(name)
        for g in groups:
            if any(frozenset((name, m)) in together for m in g):
                continue
            if any(_close(k, _key(m)) for m in g):
                g.append(name)
                break
        else:
            groups.append([name])
    for alias, target in SAME_TEAM.items():
        ga = next((g for g in groups if any(_key(m) == alias for m in g)), None)
        gt = next((g for g in groups if any(_key(m) == target for m in g)), None)
        if ga and gt and ga is not gt:
            gt.extend(ga)
            groups.remove(ga)
    return {name: g[0] for g in groups for name in g}


def teams(leg: str) -> dict[str, dict]:
    """Merged team name -> casts it drew water from and the spellings merged into it."""
    counts = _raw_counts(leg)
    out: dict[str, dict] = {}
    for name, canon in canonical(leg).items():
        t = out.setdefault(canon, {"casts": 0, "spellings": []})
        t["spellings"].append(name)
    for canon, t in out.items():
        # A cast where two spellings of one team both drew water counts once.
        t["casts"] = sum(1 for s in sheets(leg)
                         if any(n in b["draws"] for b in s["bottles"] for n in t["spellings"]))
        t["spellings"].sort(key=lambda n: -counts[n])
    return dict(sorted(out.items(), key=lambda kv: kv[0].lower()))
