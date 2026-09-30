"""Report tables: one row per operation, per bottle or per logsheet row, with
columns drawn from any of the linked tables.

Everything links through the event label: a bottle belongs to a rosette
cast, a logsheet row is matched to an operation, and an operation carries
its conditions. So a bottle table can show the station's air temperature,
and a logsheet table the bottom depth, without the participant joining
anything. Links run from the finer row to the coarser one only; an
operation row shows bottles as counts and volumes, not as a list.

In a table of log rows, ``bottle.*`` is the rosette bottle the row names
(its operation, matched from the event log, and its bottle number) with the
rosette sheet's and bottle file's values: "Depth (Rosette)"; ``logrole.*``
is the log's own column that holds that kind of value: "Depth (log)";
``uw.*`` is the conditions column's value from the underway record at the
row's own date and time (conditions.at_time), for rows that give both.

Column ids are ``<table>.<field>``: ``op.*`` (conditions), ``bottle.*``,
``draw.<team>`` (what that team drew from the bottle, under the merged team
name from ``rosette.canonical``), ``log.<column>`` and ``uw.*``.
"""

from __future__ import annotations

import re
from dataclasses import dataclass

from . import conditions, ctd, eventlog, logsheets, rosette


@dataclass(frozen=True)
class Col:
    id: str
    label: str
    unit: str = ""
    digits: int | None = None


# Bottle-file parameters worth offering, by the names SeaBird writes (first found wins).
BTL_PARAMS = [
    ("temperature", "Temperature", "°C", 3, ["T090C", "T190C"]),
    ("salinity", "Salinity", "PSU", 3, ["Sal00", "Sal11"]),
    ("oxygen", "Oxygen", "mL/L", 2, ["Sbeox0ML/L"]),
    ("fluorescence", "Fluorescence", "µg/L", 3, ["FlSP", "FlECO-AFL", "Fluorescence"]),
    ("transmission", "Beam transmission", "%", 1, ["CStarTr0"]),
    ("par", "PAR", "µE/m²/s", 2, ["Par", "PAR"]),
]

BOTTLE_COLS = [
    Col("bottle.bottle", "Bottle"),
    Col("bottle.target", "Target depth"),
    Col("bottle.trip_db", "Trip pressure", "dbar", 0),
    Col("bottle.depth_m", "Depth", "m", 1),
    Col("bottle.time_utc", "Trip time (UTC)"),
    *[Col(f"bottle.{k}", label, unit, d) for k, label, unit, d, _ in BTL_PARAMS],
    Col("bottle.light_pct", "% light"),
    Col("bottle.in_log", "In your logs"),
    Col("bottle.volume_team_l", "Volume drawn (team)", "L", 1),
    Col("bottle.note", "Note"),
    Col("bottle.cast", "Cast no."),
    Col("bottle.comment", "Bottle comment"),
]

# Columns that take, from each log, whichever of its own columns holds that
# kind of value (its header row says which), so logs with differently named
# columns stack into one column.
LOG_ROLES = {"station": "Station (log)", "cast": "Cast (log)", "bottle": "Bottle (log)",
             "depth": "Depth (log)", "datetime": "Date and time (log)", "date": "Date (log)",
             "sample_id": "Sample ID (log)", "label": "Event label (log)"}

OP_EXTRA = [
    Col("op.n_bottles_team", "Bottles sampled (team)"),
    Col("op.volume_team_l", "Volume drawn (team)", "L", 1),
    Col("op.n_log_rows", "Logsheet rows"),
]


def op_columns() -> list[Col]:
    return [Col(f"op.{c.key}", c.label, c.unit, c.digits) for c in conditions.COLUMNS] + OP_EXTRA


def uw_columns() -> list[Col]:
    """The conditions a log row takes at its own time, labelled "(at row time)"."""
    return [Col("uw.time_utc", "Row time (UTC)")] + [
        Col(f"uw.{c.key}", f"{c.label} (at row time)", c.unit, c.digits)
        for c in conditions.COLUMNS if c.key in conditions.AT_TIME]


def catalog(leg: str, teams: list[str], logs: list[dict]) -> dict:
    """What the column picker offers, grouped by table."""
    out = {
        "op": [c.__dict__ for c in op_columns()],
        "bottle": [c.__dict__ for c in BOTTLE_COLS],
        "draw": [Col(f"draw.{t}", f"{t} (drawn)", "L").__dict__ for t in teams],
        "uw": [c.__dict__ for c in uw_columns()],
        "logrole": [Col("logmeta.log", "Log").__dict__]
                   + [Col(f"logrole.{k}", v).__dict__ for k, v in LOG_ROLES.items()],
    }
    for lg in logs:
        meta = logsheets.load(lg["id"])
        cols = meta["sheets"][lg["sheet"]]["columns"]
        out[f"log:{lg['id']}:{lg['sheet']}"] = [Col(f"log.{c}", c).__dict__ for c in cols]
    return out


def _bottles(leg: str, keys: set[str]) -> list[dict]:
    """Bottles of the selected casts, rosette-sheet draws joined to bottle-file values."""
    casts = ctd.cast_cache(leg)
    by_cast = ctd.label_for_cast(leg)
    canon = rosette.canonical(leg)
    out = []
    seen = set()
    for s in rosette.sheets(leg):
        label = s.get("label")
        if label not in keys and s.get("cast"):
            try:
                label = by_cast.get(int(float(s["cast"])), label)
            except ValueError:
                pass
        if label not in keys or label in seen:
            continue
        seen.add(label)
        cast = next(iter(v for k, v in (casts.get(label) or {}).items() if k != "LADCP"), None)
        btl = {b["bottle"]: b for b in (cast or {}).get("bottles", [])}
        for b in s["bottles"]:
            f = btl.get(b["bottle"], {})
            draws: dict = {}
            for name, v in b["draws"].items():
                team = canon.get(name, name)
                if isinstance(v, (int, float)) and isinstance(draws.get(team), (int, float)):
                    draws[team] += v            # two spellings of one team on one bottle
                else:
                    draws.setdefault(team, v)
            params = f.get("parameters", {})
            row = {
                "label": label, "bottle": b["bottle"], "target": b["target"],
                "trip_db": b["trip_db"] if b["trip_db"] is not None else f.get("p"),
                "depth_m": f.get("depth_m"), "time_utc": f.get("time"),
                "light_pct": b["light_pct"], "cast": s.get("cast"), "comment": b["comment"],
                "draws": draws,
            }
            for k, _, _, _, names in BTL_PARAMS:
                row[k] = next((params[n] for n in names if params.get(n) is not None), None)
            out.append(row)
    return out


def _team_volume(draws: dict, teams: list[str]) -> float | None:
    vals = [v for t, v in draws.items() if t in teams and isinstance(v, (int, float))]
    return sum(vals) if vals else None


def _bottle_no(row: dict, roles: dict) -> int | None:
    """The bottle number a log row names in its Bottle column."""
    col = (roles or {}).get("bottle")
    n = re.fullmatch(r"\s*(\d{1,2})\s*", str(row.get(col) or "")) if col else None
    return int(n[1]) if n else None


def logged_bottles(logs: dict[str, dict], used: list[dict]) -> dict[tuple[str, int], str]:
    """(operation, bottle number) -> the log that lists it, from the logs ticked
    as bottle sources (``bottles``, apart from their tick for operations) whose
    rows name a bottle (the "bottle" role) and match an operation."""
    out: dict[tuple[str, int], str] = {}
    for lg in used:
        if lg.get("bottles") is False:
            continue
        m = logs.get(f"log:{lg['id']}:{lg['sheet']}")
        if not (m or {}).get("roles", {}).get("bottle"):
            continue
        for r in m["rows"]:
            n = _bottle_no(r, m["roles"])
            if r.get("_op") and n is not None:
                out.setdefault((r["_op"], n), m.get("name") or lg.get("name") or "log")
    return out


def bottle_key(b: dict) -> str:
    return f"{b['label']}#{b['bottle']}"


def _sources(b: dict, teams: list[str], logged: dict) -> list[str]:
    """What says a bottle is the team's: rosette-sheet columns, then a log."""
    why = [t for t in teams if t in b["draws"]]
    if (b["label"], b["bottle"]) in logged:
        why.append(logged[(b["label"], b["bottle"])])
    return why


def chosen_bottles(bottles: list[dict], teams: list[str], logged: dict, picked: dict | None) -> dict[str, str]:
    """The team's bottles among ``bottles`` (the selected operations' casts),
    bottle_key -> why. A bottle is the team's when a ticked rosette-sheet
    column drew from it or a ticked log lists it; ``picked`` (selection.bottles:
    {added, removed, edits}) adds and removes bottles by hand, and that always
    stands. With nothing to go by, every bottle counts."""
    picked = picked or {}
    added, removed = set(picked.get("added") or []), set(picked.get("removed") or [])
    named = bool(teams or logged or added)
    out = {}
    for b in bottles:
        k = bottle_key(b)
        if k in removed:
            continue
        why = _sources(b, teams, logged)
        if k in added and not why:
            why = ["by hand"]
        if why or not named:
            out[k] = ", ".join(why) or "every bottle"
    return out


def _decorate(bottles: list[dict], teams: list[str], logged: dict, picked: dict | None) -> dict[str, str]:
    """Mark each bottle chosen or not, with its team volume (a volume typed on
    the page wins) and note; the chosen set."""
    edits = (picked or {}).get("edits") or {}
    chosen = chosen_bottles(bottles, teams, logged, picked)
    for b in bottles:
        k = bottle_key(b)
        e = edits.get(k) or {}
        b["in_log"] = logged.get((b["label"], b["bottle"]))
        b["chosen"] = k in chosen
        b["volume_team_l"] = e["volume"] if e.get("volume") not in (None, "") else _team_volume(b["draws"], teams)
        b["note"] = e.get("note") or None
    return chosen


def bottle_rows(leg: str, op_keys: list[str], teams: list[str], logged: dict, picked: dict | None) -> list[dict]:
    """Every bottle of the selected operations' casts, for the page's bottle table."""
    bottles = _bottles(leg, set(op_keys))
    _decorate(bottles, teams, logged, picked)
    auto = chosen_bottles(bottles, teams, logged, None)        # before the page's own ticks
    ops = eventlog.by_key(leg)
    picked = picked or {}
    added, removed = set(picked.get("added") or []), set(picked.get("removed") or [])
    order = {k: i for i, k in enumerate(op_keys)}
    out = []
    for b in sorted(bottles, key=lambda b: (order.get(b["label"], 1e9), b["bottle"])):
        k = bottle_key(b)
        op = ops.get(b["label"])
        out.append({"key": k, "label": b["label"], "station": op.station if op else None, "cast": b["cast"],
                    "bottle": b["bottle"], "target": b["target"], "depth_m": b["depth_m"],
                    "trip_db": b["trip_db"], "draws": {t: b["draws"][t] for t in teams if t in b["draws"]},
                    "sources": _sources(b, teams, logged), "chosen": b["chosen"], "auto": k in auto,
                    "own": "added" if k in added else "removed" if k in removed else None,
                    "volume_team_l": b["volume_team_l"], "note": b["note"], "comment": b["comment"]})
    return out


def build(leg: str, spec: dict, op_keys: list[str], teams: list[str],
          logs: dict[str, dict], logged: dict[tuple[str, int], str] | None = None,
          picked: dict | None = None) -> dict:
    """One report table: ``spec`` = {title, rows, columns}.

    ``logs`` maps "log:<id>:<sheet>" to a matched logsheet (logsheets.matched);
    ``logged`` is ``logged_bottles`` and ``picked`` the page's own bottle
    choices and edits (chosen_bottles).
    """
    logged = logged or {}
    keys = set(op_keys)
    op_rows = {r["key"]: r for r in conditions.table(leg, list(keys))}
    bottles = _bottles(leg, keys)
    _decorate(bottles, teams, logged, picked)
    named = bool(teams or logged or (picked or {}).get("added"))
    for r in op_rows.values():
        mine = [b for b in bottles if b["label"] == r["key"] and b["chosen"]]
        r["n_bottles_team"] = len(mine) if named else None
        vols = [b["volume_team_l"] for b in mine]
        r["volume_team_l"] = sum(v for v in vols if v) if any(vols) else None
        r["n_log_rows"] = sum(1 for lg in logs.values() for x in lg["rows"] if x["_op"] == r["key"]) \
            if logs else None

    source = spec.get("rows", "operations")
    if source == "operations":
        base = [{"op": r} for r in sorted(op_rows.values(), key=lambda r: r["start_utc"])]
    elif source == "bottles":
        rows = [b for b in bottles if b["chosen"]]
        base = [{"op": op_rows.get(b["label"], {}), "bottle": b} for b in rows]
        base.sort(key=lambda x: (x["op"].get("start_utc") or "", x["bottle"]["bottle"]))
    elif source == "logs" or source.startswith("log:"):
        # Every ticked log's rows, stacked, or one log's ("log:<id>:<sheet>"),
        # whether or not it is ticked to select operations.
        chosen = [logs[source]] if source in logs else [] if source != "logs" else \
            [lg for lg in logs.values() if lg.get("use") is not False]
        # Each row's rosette bottle: its operation and the bottle number it names.
        by_bottle = {(b["label"], b["bottle"]): b for b in bottles}
        base = [{"op": op_rows.get(x["_op"]) or {}, "log": x, "lg": lg,
                 "bottle": by_bottle.get((x["_op"], _bottle_no(x, lg.get("roles")))) or {}}
                for lg in chosen
                for x in lg["rows"] if x["_op"] is None or x["_op"] in keys or not keys]
        if any(c.startswith("uw.") for c in spec.get("columns", [])):
            span = logsheets.leg_span(leg)
            for b in base:
                when = logsheets.row_time(b["log"], b["lg"].get("roles") or {}, bool(b["lg"].get("local")), span)
                b["uw"] = {**conditions.at_time(leg, when), "time_utc": when} if when else {}
    else:
        raise ValueError(f"unknown row source {source}")

    cols = [_col_meta(c, teams) for c in spec.get("columns", [])]
    if source == "logs" or source.startswith("log:"):
        cols = [{**c, "label": f"{c['label']} (Rosette)"} if c["id"].startswith("bottle.") else c for c in cols]
    body = [[_value(b, c["id"]) for c in cols] for b in base]
    return {"title": spec.get("title") or "", "rows": source, "columns": cols, "body": body}


def _col_meta(cid: str, teams: list[str]) -> dict:
    known = {c.id: c for c in op_columns() + uw_columns() + BOTTLE_COLS}
    if cid in known:
        return known[cid].__dict__
    table, _, field = cid.partition(".")
    if table == "draw":
        return Col(cid, f"{field} (L)").__dict__
    if table == "logrole":
        return Col(cid, LOG_ROLES.get(field, field)).__dict__
    if table == "logmeta":
        return Col(cid, "Log").__dict__
    return Col(cid, field).__dict__


def _value(base: dict, cid: str):
    table, _, field = cid.partition(".")
    if table in ("op", "uw"):
        v = (base.get(table) or {}).get(field)
        # The row keeps degrees (the narrative averages them); a table says NNW.
        return conditions.compass(v) if field == "wind_dir_deg" and v is not None else v
    if table == "bottle":
        return (base.get("bottle") or {}).get(field)
    if table == "draw":
        return ((base.get("bottle") or {}).get("draws") or {}).get(field)
    if table == "log":
        # The row's column whose header reads the same, ignoring case and spacing.
        row, want = base.get("log") or {}, _header_key(field)
        return next((v for k, v in row.items() if not k.startswith("_") and _header_key(k) == want), None)
    if table == "logrole":
        roles = (base.get("lg") or {}).get("roles") or {}
        col = roles.get(field)
        return (base.get("log") or {}).get(col) if col else None
    if table == "logmeta":
        return (base.get("lg") or {}).get("name") if field == "log" else None
    return None


def _header_key(h: str) -> str:
    return " ".join(str(h).split()).casefold()


def fmt(v, col: dict) -> str:
    """A cell as text, rounded to the column's digits, ISO times shortened."""
    if v is None or v == "":
        return ""
    if isinstance(v, bool):
        return "yes" if v else ""
    if isinstance(v, (int, float)) and col.get("digits") is not None:
        s = f"{v:.{col['digits']}f}"
        return s.replace("-", "−")
    if isinstance(v, float):
        return f"{v:g}"
    s = str(v)
    if len(s) >= 16 and s[4] == "-" and s[10] == "T":
        return s[:16].replace("T", " ")
    return s


def ops_for(leg: str, groups: list[str]) -> list[str]:
    return [o.key for o in eventlog.operations(leg) if o.group in set(groups)]
