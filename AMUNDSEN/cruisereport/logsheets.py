"""Team logsheets: a participant's own spreadsheet, matched to operations.

Teams keep their own sheets (the sediment logbook, net sample lists, lab
records) in whatever layout suits them. An upload is read as a table: the
header row is the first of the top rows that looks like one, and each
column's role (event label, station, date/time, latitude, longitude, depth,
bottle, sample id) is guessed from its name and its values. The page shows
the guesses and the participant corrects them.

Each row is matched to an event-log operation, and the method is kept with
it so the participant can check it:

0. ``by hand``      the participant chose the operation for this row, or
                    ``removed by hand``: chose none, so nothing below is tried
1. ``label``        the row carries an event label (``AMD2603-010``)
1b. ``cast``        the row's cast number is in the leg's CTD logbook, which
                    names the event label (a misread station name then does
                    not matter)
2. ``station+time`` same station, closest operation within ``TIME_WINDOW_H``;
                    a time with no date is taken on the days of the station's
                    operations
3. ``station+date`` same station, same day, no usable time
4. ``time+position`` closest operation within ``TIME_WINDOW_H`` and ``MAX_KM``
5. ``station``      no usable date, but the leg visited that station only once
                    (all its operations within ``ONE_VISIT_H``)

A row that matches nothing stays in the table, unmatched.

Dates and times are read as logbooks write them: day and month in either
order (whichever falls within the leg), no year (the leg's), day of year
("DOY 257"), times as 14:20, 14h20, 1420, 14.20, 2:20 PM or an Excel day
fraction. A table's times are UTC unless it is marked as kept in ship time
(``SHIP_TZ``); then the event log's times are compared in ship time.
"""

from __future__ import annotations

import io
import json
import math
import re
import threading
import uuid
import warnings
from pathlib import Path

import pandas as pd

from . import ctd, eventlog
from .config import SHIP_TZ, STATE_DIR

LABEL_RE = re.compile(r"\bAMD\d{4}-\d{3}\b", re.I)
# A digitized table's rows matched by hand carry the event label in this column.
HAND_COLUMN = "Event label (matched by hand)"
# A row the participant unmatched by hand carries this instead of an event label:
# it stays unmatched however well it would match automatically.
NO_MATCH = "none"
# The column an export leads with: the operation each row matches, as the page shows it.
MATCH_COLUMN = "Matched to"
TIME_WINDOW_H = 3
ONE_VISIT_H = 12
MAX_KM = 5
_lock = threading.Lock()              # one read-modify-write of a logsheet record at a time

ROLES = ["label", "station", "datetime", "date", "time", "lat", "lon", "depth", "bottle",
         "cast", "sample_id"]
NAME_HINTS = [
    ("label", r"^(event|label|event.?id|operation|unique.?identifier)\b"),
    ("station", r"station|^site\b|^stn?\.?$|^sta\.?$"),
    ("datetime", r"date.*time|datetime|time.*utc"),
    ("date", r"date"),
    ("time", r"^time|heure"),
    ("lat", r"^lat"),
    ("lon", r"^lon"),
    ("depth", r"depth|prof"),
    ("bottle", r"bottle|niskin|\bbtl\b|^bot\.?$|^btl\.?$"),
    ("cast", r"^cast|ctd.?#|ctd.?no"),
    ("sample_id", r"sample.?(id|name)|unique|^id$|vial"),
]


def _dir() -> Path:
    d = STATE_DIR / "logsheets"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _norm_station(s) -> str:
    return re.sub(r"[\s_\-]+", "", str(s or "")).casefold()


def _is_blank(v) -> bool:
    return v is None or (isinstance(v, float) and math.isnan(v)) or str(v).strip() == ""


def _header_row(df: pd.DataFrame) -> int:
    """The first row, among the top 15, that looks like column headings."""
    best, best_score = 0, -1
    for i in range(min(15, len(df))):
        row = df.iloc[i]
        texts = [v for v in row if isinstance(v, str) and v.strip() and len(v) < 60]
        filled = sum(not _is_blank(v) for v in row)
        below = df.iloc[i + 1:i + 4]
        data_below = below.notna().sum(axis=1).mean() if len(below) else 0
        score = len(texts) * 2 + (1 if data_below >= max(2, filled * 0.4) else -5)
        if len(texts) >= 2 and score > best_score:
            best, best_score = i, score
    return best


def read_table(name: str, raw: bytes) -> dict[str, pd.DataFrame]:
    """Sheets of an upload as data frames with a detected header row."""
    buf = io.BytesIO(raw)
    if name.lower().endswith((".csv", ".txt", ".tsv")):
        text = raw.decode("utf-8-sig", errors="replace")
        frames = {"csv": pd.read_csv(io.StringIO(text), sep=None, engine="python", header=None,
                                     dtype=object)}
    else:
        frames = pd.read_excel(buf, sheet_name=None, header=None, dtype=object)
    out = {}
    for sheet, df in frames.items():
        df = df.dropna(how="all").dropna(axis=1, how="all")
        if len(df) < 2:
            continue
        h = _header_row(df)
        cols, seen = [], {}
        for j, v in enumerate(df.iloc[h]):
            c = " ".join(str(v).split()) if not _is_blank(v) else f"column {j + 1}"
            seen[c] = seen.get(c, 0) + 1
            cols.append(c if seen[c] == 1 else f"{c} ({seen[c]})")
        body = df.iloc[h + 1:].copy()
        body.columns = cols
        body = body.dropna(how="all").reset_index(drop=True)
        if len(body):
            out[str(sheet)] = body
    return out


def guess_roles(df: pd.DataFrame) -> dict[str, str]:
    """Role -> column name, from names first and label-shaped values second."""
    roles: dict[str, str] = {}
    for role, pattern in NAME_HINTS:
        rx = re.compile(pattern, re.I)
        for c in df.columns:
            if c not in roles.values() and rx.search(c):
                roles.setdefault(role, c)
                break
    # A column whose values are event labels is the label, whatever it is called.
    for c in df.columns:
        vals = df[c].dropna().astype(str).head(50)
        if len(vals) and vals.str.contains(LABEL_RE).mean() > 0.5:
            if roles.get("sample_id") == c:
                del roles["sample_id"]
            roles["label"] = c
            break
    if "datetime" in roles and roles.get("date") == roles["datetime"]:
        del roles["date"]
    return roles


def _float(v) -> float | None:
    try:
        f = float(str(v).replace(",", "."))
    except (TypeError, ValueError):
        return None
    return f if math.isfinite(f) else None


CLOCK_RE = re.compile(r"(?<![\d.])(\d{1,2})\s*[:hH.]\s*(\d{2})(?:\s*[:.]\s*\d{2})?(?!\d)(?![.\-/]\d)\s*([aApP])?\.?[mM]?\.?")
HHMM_RE = re.compile(r"\s*(\d{3,4})\s*([aApP])?\.?[mM]?\.?\s*")
DOY_RE = re.compile(r"^\s*(?:doy|jd|julian(?:\s*day)?|day)\s*[#:]?\s*(\d{1,3})\s*$", re.I)


def _hm(h: int, m: int, ampm: str | None) -> tuple[int, int] | None:
    if ampm:
        if not 1 <= h <= 12:
            return None
        h = h % 12 + (12 if ampm.lower() == "p" else 0)
    return (h, m) if h < 24 and m < 60 else None


def _clock(v) -> tuple[int, int] | None:
    """Hour and minute from a time cell, however it is written."""
    if _is_blank(v):
        return None
    if hasattr(v, "hour") and not isinstance(v, str):
        return v.hour, v.minute
    if isinstance(v, float) and 0 <= v < 1:            # Excel's fraction of a day
        mins = round(v * 1440) % 1440
        return mins // 60, mins % 60
    text = str(v)
    m = HHMM_RE.fullmatch(text) if isinstance(v, (int, str)) else None
    if m:
        return _hm(int(m[1]) // 100, int(m[1]) % 100, m[2])
    m = CLOCK_RE.search(text)
    return _hm(int(m[1]), int(m[2]), m[3]) if m else None


def _date(v, span: tuple[pd.Timestamp, pd.Timestamp] | None) -> pd.Timestamp | None:
    """A day from a date cell. Day and month are taken in the order that puts
    the day within ``span`` (the leg), and a date with no year takes the leg's."""
    if _is_blank(v):
        return None
    if isinstance(v, pd.Timestamp) or (hasattr(v, "year") and not isinstance(v, str)):
        t = pd.Timestamp(v)
        return t.tz_localize(None) if t.tzinfo else t
    text = str(v).strip()
    year = span[0].year if span else None
    if re.fullmatch(r"\d{8}", text):                  # 20260914
        t = pd.to_datetime(text, format="%Y%m%d", errors="coerce")
        return None if pd.isna(t) else t
    m = DOY_RE.match(text)
    if m and year:
        return pd.Timestamp(year, 1, 1) + pd.Timedelta(days=int(m[1]) - 1)
    has_year = bool(re.search(r"\d{4}|\d{1,2}[/.\-]\d{1,2}[/.\-]\d{2}\b", text))
    cands = []
    for dayfirst in (True, False):
        with warnings.catch_warnings():               # both orders are tried on purpose
            warnings.simplefilter("ignore")
            t = pd.to_datetime(text, dayfirst=dayfirst, errors="coerce")
        if pd.isna(t):
            continue
        t = t.tz_localize(None) if t.tzinfo else t
        if not has_year and year:
            try:
                t = t.replace(year=year)
            except ValueError:                        # 29 February
                continue
        cands.append(t)
    if not cands:
        return None
    if span:
        lo, hi = span[0].normalize() - pd.Timedelta(days=2), span[1].normalize() + pd.Timedelta(days=2)
        inside = [t for t in cands if lo <= t <= hi]
        if inside:
            return inside[0]
    return cands[0]


def _when(row: dict, roles: dict, span=None) -> tuple[pd.Timestamp | None, tuple[int, int] | None]:
    """(day, clock) from the datetime column, or the date and time columns. The
    day is None when only a time is written; the clock None when only a day is."""
    day = clock = None
    if roles.get("datetime"):
        v = row.get(roles["datetime"])
        day, clock = _date(v, span), _clock(v)
        if day is not None and clock is None and (day.hour, day.minute) != (0, 0):
            clock = (day.hour, day.minute)
    if day is None and roles.get("date"):
        day = _date(row.get(roles["date"]), span)
    if clock is None and roles.get("time"):
        clock = _clock(row.get(roles["time"]))
    return (day.normalize() if day is not None else None), clock


def leg_span(leg: str) -> tuple[pd.Timestamp, pd.Timestamp] | None:
    """First and last operation start of the leg, UTC."""
    starts = [pd.Timestamp(o.summary()["start_utc"]) for o in eventlog.operations(leg) if o.group != "void"]
    return (min(starts), max(starts)) if starts else None


def shift_times(rows: list[dict], roles: dict, local_in: bool, local_out: bool, span=None) -> list[dict]:
    """Rows with their dates and times rewritten from UTC or ship time
    (``local_in``) into the other (``local_out``). A row with a date and a time
    is converted exactly (the date may change); a time with no date is moved
    by the ship's offset on the leg's first day; a date alone is left as is."""
    if local_in == local_out or not (roles.get("datetime") or roles.get("time")):
        return rows
    to = lambda t: (t.tz_localize(SHIP_TZ, ambiguous="NaT", nonexistent="shift_forward").tz_convert("UTC")  # noqa: E731
                    if local_in else t.tz_localize("UTC").tz_convert(SHIP_TZ)).tz_localize(None)
    out = []
    for r in rows:
        day, clock = _when(r, roles, span)
        if clock is None:
            out.append(r)
            continue
        base = day if day is not None else (span[0].normalize() if span else pd.Timestamp("2000-01-01"))
        t = to(base + pd.Timedelta(hours=clock[0], minutes=clock[1]))
        if pd.isna(t):
            out.append(r)
            continue
        r = dict(r)
        if roles.get("datetime") and _clock(r.get(roles["datetime"])) is not None:
            r[roles["datetime"]] = t.strftime("%Y-%m-%d %H:%M") if day is not None else t.strftime("%H:%M")
        else:
            if roles.get("time"):
                r[roles["time"]] = t.strftime("%H:%M")
            if day is not None and roles.get("date"):
                r[roles["date"]] = t.strftime("%Y-%m-%d")
        out.append(r)
    return out


def _km(lat1, lon1, lat2, lon2) -> float:
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dl = math.radians(lon2 - lon1)
    a = math.sin((p2 - p1) / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 6371 * 2 * math.asin(math.sqrt(min(1, a)))


def match(rows: list[dict], roles: dict, leg: str, groups: list[str] | None = None,
          local: bool = False) -> list[dict]:
    """Each row with ``_op`` (operation key or None) and ``_how``. With ``local``
    the rows' dates and times are ship time (SHIP_TZ), else UTC."""
    ops = [o.summary() for o in eventlog.operations(leg) if o.group != "void"]
    # Each operation's start in the rows' own time: UTC, or ship time.
    start = {}
    for o in ops:
        t = pd.Timestamp(o["start_utc"])
        start[o["key"]] = t.tz_localize("UTC").tz_convert(SHIP_TZ).tz_localize(None) if local else t
    span = (min(start.values()), max(start.values())) if start else None
    by_label = {o["label"]: o for o in ops if o["label"]}
    by_key = {o["key"]: o for o in ops}
    by_station: dict[str, list[dict]] = {}
    for o in ops:
        by_station.setdefault(_norm_station(o["station"]), []).append(o)
    pref = set(groups or [])
    cast_labels = ctd.label_for_cast(leg)

    def rank(cands, t):
        # Prefer the instruments the participant ticked, then the nearest in time.
        return sorted(cands, key=lambda o: (o["group"] not in pref if pref else False,
                                            abs((start[o["key"]] - t).total_seconds())
                                            if t is not None else 0))

    def near(cands, t):
        return [o for o in cands if abs((start[o["key"]] - t).total_seconds()) <= TIME_WINDOW_H * 3600]

    out = []
    for r in rows:
        key, how = None, None
        text = " ".join(str(v) for v in r.values() if not _is_blank(v))
        hand = str(r.get(HAND_COLUMN) or "")
        if hand == NO_MATCH:
            out.append({**r, "_op": None, "_how": "removed by hand"})
            continue
        if hand and hand in by_key:
            key, how = hand, "by hand"
        m = None if key else (LABEL_RE.search(str(r.get(roles.get("label", ""), "")) or "")
                              or LABEL_RE.search(text))
        if m and m[0].upper() in by_label:
            key, how = by_label[m[0].upper()]["key"], "label"
        if key is None and roles.get("cast"):
            m = re.fullmatch(r"\s*0*(\d{1,4})\s*", str(r.get(roles["cast"]) or ""))
            label = cast_labels.get(int(m[1])) if m else None
            if label and label in by_label:
                key, how = by_label[label]["key"], "cast"
        day, clock = _when(r, roles, span)
        at = lambda d: d + pd.Timedelta(hours=clock[0], minutes=clock[1])  # noqa: E731
        t = at(day) if day is not None and clock else day
        if key is None and roles.get("station"):
            cands = by_station.get(_norm_station(r.get(roles["station"])), [])
            if cands and day is not None and clock:
                hits = near(cands, t)
                if hits:
                    key, how = rank(hits, t)[0]["key"], "station+time"
            if key is None and cands and day is None and clock:
                # A time with no date: on the day before, of or after each of the station's operations.
                best = None
                for o in cands:
                    for shift in (-1, 0, 1):
                        tt = at(start[o["key"]].normalize() + pd.Timedelta(days=shift))
                        gap = abs((start[o["key"]] - tt).total_seconds())
                        if gap <= TIME_WINDOW_H * 3600 and (best is None or gap < best[0]):
                            best = (gap, o)
                if best:
                    key, how = best[1]["key"], "station+time"
            if key is None and cands and day is not None:
                same = [o for o in cands if start[o["key"]].normalize() == day]
                if same:
                    key, how = rank(same, t)[0]["key"], "station+date"
        if key is None and day is not None and clock and roles.get("lat") and roles.get("lon"):
            lat, lon = _float(r.get(roles["lat"])), _float(r.get(roles["lon"]))
            if lat is not None and lon is not None:
                hits = [o for o in near(ops, t) if o["lat"] is not None and _km(lat, lon, o["lat"], o["lon"]) <= MAX_KM]
                if hits:
                    key, how = rank(hits, t)[0]["key"], "time+position"
        if key is None and roles.get("station"):
            cands = by_station.get(_norm_station(r.get(roles["station"])), [])
            if cands:
                starts = [start[o["key"]] for o in cands]
                if max(starts) - min(starts) <= pd.Timedelta(hours=ONE_VISIT_H):
                    key, how = rank(cands, None)[0]["key"], "station"
        out.append({**r, "_op": key, "_how": how})
    return out


def _clean_cell(v):
    if _is_blank(v):
        return None
    if isinstance(v, pd.Timestamp):
        return v.isoformat()
    if hasattr(v, "isoformat"):
        return v.isoformat()
    if isinstance(v, float) and v.is_integer():
        return int(v)
    return v


def save_upload(name: str, raw: bytes) -> dict:
    sheets = read_table(name, raw)
    if not sheets:
        raise ValueError("no table found in the upload")
    ident = uuid.uuid4().hex[:12]
    safe = re.sub(r"[^\w.\- ]+", "_", Path(name).name)[:120]
    (_dir() / f"{ident}__{safe}").write_bytes(raw)
    return _save_meta(ident, safe, sheets)


def save_frames(name: str, sheets: dict[str, pd.DataFrame], source: dict | None = None) -> dict:
    """A logsheet made from tables already in hand (a digitized logbook page)."""
    ident = uuid.uuid4().hex[:12]
    safe = re.sub(r"[^\w.\-· ]+", "_", name)[:120]
    return _save_meta(ident, safe, sheets, source)


def _save_meta(ident: str, safe: str, sheets: dict[str, pd.DataFrame], source: dict | None = None) -> dict:
    meta = {"id": ident, "name": safe, "sheets": {}, "source": source}
    for sheet, df in sheets.items():
        meta["sheets"][sheet] = {
            "columns": list(df.columns),
            "roles": guess_roles(df),
            "rows": [{k: _clean_cell(v) for k, v in r.items()} for r in df.to_dict("records")],
        }
    (_dir() / f"{ident}.json").write_text(json.dumps(meta, default=str))
    return meta


def load(ident: str) -> dict:
    if not re.fullmatch(r"[0-9a-f]{12}", ident):
        raise ValueError("bad logsheet id")
    return json.loads((_dir() / f"{ident}.json").read_text())


def matched(ident: str, sheet: str, roles: dict, leg: str, groups=None, local: bool = False) -> dict:
    """The sheet's rows matched to operations; rows the participant matched by
    hand keep that match. ``sheets`` gives every sheet of the upload with its
    guessed roles, so the page can offer the others. ``edited`` lists the [row, column] cells changed on
    the page, and ``editable`` is false for a logsheet made from a digitized
    table, which is edited there and rewritten from it."""
    meta = load(ident)
    sh = meta["sheets"][sheet]
    manual = sh.get("manual") or {}
    rows = [{**r, HAND_COLUMN: manual[str(i)]} if str(i) in manual else r
            for i, r in enumerate(sh["rows"])]
    rows = match(rows, roles, leg, groups, local)
    return {"id": ident, "local": local, "name": meta["name"], "sheet": sheet, "columns": sh["columns"],
            "roles": roles, "rows": rows, "edited": sh.get("edited") or [],
            "sheets": {k: {"roles": v["roles"], "n": len(v["rows"])} for k, v in meta["sheets"].items()},
            "editable": not meta.get("source"),
            "matched": sum(1 for r in rows if r["_op"]), "total": len(rows)}


def _write(ident: str, meta: dict) -> None:
    (_dir() / f"{ident}.json").write_text(json.dumps(meta, default=str))


def _editable(ident: str, sheet: str) -> tuple[dict, dict]:
    meta = load(ident)
    if meta.get("source"):
        raise ValueError("this log is made from a transcribed table: correct it there")
    return meta, meta["sheets"][sheet]


def _typed(text: str):
    """A typed cell as the upload would have read it: a number stays a number,
    but one with a leading zero (a sample id "0012") stays text."""
    text = text.strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?(0|[1-9]\d*)", text):
        return int(text)
    if re.fullmatch(r"[-+]?(\d+\.\d*|\.\d+)([eE][-+]?\d+)?", text):
        return float(text)
    return text


def edit(ident: str, sheet: str, row: int, col: int, text: str) -> dict:
    """A participant's correction of one cell; row -1 renames the column."""
    with _lock:
        meta, sh = _editable(ident, sheet)
        old = sh["columns"][col]
        if row < 0:
            new = " ".join(text.split())
            if not new or new == old:
                return meta
            if new in sh["columns"]:
                raise ValueError(f"there is already a column called {new}")
            sh["columns"][col] = new
            sh["rows"] = [{(new if k == old else k): v for k, v in r.items()} for r in sh["rows"]]
        else:
            sh["rows"][row][old] = _typed(text)
            if [row, col] not in sh.setdefault("edited", []):
                sh["edited"].append([row, col])
        _write(ident, meta)
    return meta


def grow(ident: str, sheet: str, what: str) -> dict:
    """An empty row at the bottom or a column at the right."""
    with _lock:
        meta, sh = _editable(ident, sheet)
        if what == "row":
            sh["rows"].append({c: None for c in sh["columns"]})
        elif what == "col":
            n = len(sh["columns"]) + 1
            while f"column {n}" in sh["columns"]:
                n += 1
            sh["columns"].append(f"column {n}")
            for r in sh["rows"]:
                r[f"column {n}"] = None
        else:
            raise ValueError("add a row or a col")
        _write(ident, meta)
    return meta


def set_match(ident: str, sheet: str, row: int, op_key: str | None) -> dict:
    """A participant's own match for one row: an operation key, NO_MATCH to
    leave it unmatched, or None to go back to the automatic match."""
    with _lock:
        meta, sh = _editable(ident, sheet)
        manual = sh.setdefault("manual", {})
        if op_key:
            manual[str(row)] = op_key
        else:
            manual.pop(str(row), None)
        _write(ident, meta)
    return meta


def _text(v) -> str:
    return "" if v is None else re.sub(r"[\t\r\n]+", " ", str(v))


def op_labels(keys: list[str | None], leg: str) -> list[str]:
    """Each operation key as its station and event label ("" for no match)."""
    ops = {o.key: o.summary() for o in eventlog.operations(leg)}
    return ["" if not k else " ".join(x for x in (ops.get(k, {}).get("station"), ops.get(k, {}).get("label")) if x) or k
            for k in keys]


def _match_labels(ident: str, sheet: str, match: dict, per: dict | None) -> list[str]:
    """A sheet's "Matched to" column; ``match`` is {leg, groups} and ``per`` the
    sheet's {roles, local} on the page (its guessed roles when absent)."""
    per = per or {}
    roles = per.get("roles") or load(ident)["sheets"][sheet]["roles"]
    m = matched(ident, sheet, roles, match["leg"], match.get("groups"), bool(per.get("local")))
    return op_labels([r["_op"] for r in m["rows"]], match["leg"])


def tsv(ident: str, sheet: str, times: dict | None = None, match: dict | None = None) -> str:
    """One sheet as TSV. ``times`` ({roles, local, out, span}) rewrites its
    dates and times into UTC or ship time (shift_times). With ``match``
    ({leg, groups, roles, local}) it leads with the "Matched to" column."""
    sh = load(ident)["sheets"][sheet]
    rows = sh["rows"]
    if times:
        rows = shift_times(rows, times.get("roles") or {}, bool(times.get("local")), bool(times.get("out")),
                           times.get("span"))
    lead = _match_labels(ident, sheet, match, match) if match else None
    lines = ["\t".join(_text(c) for c in ([MATCH_COLUMN] if lead else []) + sh["columns"])]
    lines += ["\t".join(_text(c) for c in ([lead[i]] if lead else []) + [r.get(c) for c in sh["columns"]])
              for i, r in enumerate(rows)]
    return "\n".join(lines) + "\n"


def xlsx(ident: str, times: dict | None = None, match: dict | None = None) -> bytes:
    """Every sheet of a logsheet as it stands, cells corrected on the page filled
    as a transcription's corrected cells are. ``times`` ({out, span, sheets:
    {sheet: {roles, local}}}) rewrites the dates and times into UTC or ship time.
    With ``match`` ({leg, groups, sheets: {sheet: {roles, local}}}) each sheet
    leads with the "Matched to" column."""
    from openpyxl import Workbook
    from openpyxl.styles import Font, PatternFill
    from openpyxl.utils import get_column_letter

    from .digitize import EDITED, colour

    meta = load(ident)
    wb = Workbook()
    wb.remove(wb.active)
    fill = PatternFill("solid", fgColor=colour(EDITED))
    for name, sh in meta["sheets"].items():
        ws = wb.create_sheet(re.sub(r"[\[\]:*?/\\]", " ", name)[:31] or "Sheet")
        lead = _match_labels(ident, name, match, (match.get("sheets") or {}).get(name)) if match else None
        ws.append(([MATCH_COLUMN] if lead else []) + sh["columns"])
        for cell in ws[1]:
            cell.font = Font(bold=True)
        rows = sh["rows"]
        per = (times or {}).get("sheets", {}).get(name)
        if per:
            rows = shift_times(rows, per.get("roles") or {}, bool(per.get("local")), bool(times.get("out")),
                               times.get("span"))
        for i, r in enumerate(rows):
            ws.append(([lead[i]] if lead else []) + [r.get(c) for c in sh["columns"]])
        skip = 1 if lead else 0
        for i, j in sh.get("edited") or []:
            ws.cell(i + 2, j + 1 + skip).fill = fill
        for j in range(1, len(sh["columns"]) + 1 + skip):
            width = max(len(str(ws.cell(i, j).value or "")) for i in range(1, ws.max_row + 1))
            ws.column_dimensions[get_column_letter(j)].width = min(40, max(8, width + 2))
        ws.freeze_panes = "A2"
    buf = io.BytesIO()
    wb.save(buf)
    return buf.getvalue()


def made_from(ident: str, table: int) -> list[str]:
    """The logsheets made from digitized table (ident, table)."""
    out = []
    for p in _dir().glob("*.json"):
        try:
            src = json.loads(p.read_text()).get("source") or {}
        except ValueError:
            continue
        if src.get("digitized") == ident and src.get("table") == table:
            out.append(p.stem)
    return out


def refresh_digitized(ident: str, table: int, build) -> list[str]:
    """Rewrite every logsheet made from digitized table (ident, table) with its
    corrected rows, ``build(fill_down)`` giving the frame for the logsheet's own
    carry-down choice; the ids of the logsheets rewritten."""
    done = []
    for p in _dir().glob("*.json"):
        try:
            meta = json.loads(p.read_text())
        except ValueError:
            continue
        src = meta.get("source") or {}
        if src.get("digitized") != ident or src.get("table") != table:
            continue
        frame = build(bool(src.get("fill_down", True)))
        for sheet in meta["sheets"].values():
            sheet["columns"] = list(frame.columns)
            sheet["rows"] = [{k: _clean_cell(v) for k, v in r.items()} for r in frame.to_dict("records")]
        p.write_text(json.dumps(meta, default=str))
        done.append(meta["id"])
    return done
