"""Report tables: one row per operation, per bottle or per logsheet row, with
columns drawn from any of the linked tables.

Everything links through the event label: a bottle belongs to a rosette
cast, a logsheet row is matched to an operation, and an operation carries
its conditions. So a bottle table can show the station's air temperature,
and a logsheet table the bottom depth, without the participant joining
anything. Links run from the finer row to the coarser one only; an
operation row shows bottles as counts and volumes, not as a list.

Column ids are ``<table>.<field>``: ``op.*`` (conditions), ``bottle.*``,
``draw.<team>`` (what that team drew from the bottle, under the merged team
name from ``rosette.canonical``) and ``log.<column>``.
"""

from __future__ import annotations

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
    Col("bottle.cast", "Cast no."),
    Col("bottle.comment", "Bottle comment"),
]

OP_EXTRA = [
    Col("op.n_bottles_team", "Bottles sampled (team)"),
    Col("op.volume_team_l", "Volume drawn (team)", "L", 1),
    Col("op.n_log_rows", "Logsheet rows"),
]


def op_columns() -> list[Col]:
    return [Col(f"op.{c.key}", c.label, c.unit, c.digits) for c in conditions.COLUMNS] + OP_EXTRA


def catalog(leg: str, teams: list[str], logs: list[dict]) -> dict:
    """What the column picker offers, grouped by table."""
    out = {
        "op": [c.__dict__ for c in op_columns()],
        "bottle": [c.__dict__ for c in BOTTLE_COLS],
        "draw": [Col(f"draw.{t}", f"{t} (drawn)", "L").__dict__ for t in teams],
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


def build(leg: str, spec: dict, op_keys: list[str], teams: list[str],
          logs: dict[str, dict]) -> dict:
    """One report table: ``spec`` = {title, rows, columns}.

    ``logs`` maps "log:<id>:<sheet>" to a matched logsheet (logsheets.matched).
    """
    keys = set(op_keys)
    op_rows = {r["key"]: r for r in conditions.table(leg, list(keys))}
    bottles = _bottles(leg, keys)
    for r in op_rows.values():
        mine = [b for b in bottles if b["label"] == r["key"] and
                any(t in b["draws"] for t in teams)]
        r["n_bottles_team"] = len(mine) if teams else None
        vols = [_team_volume(b["draws"], teams) for b in mine]
        r["volume_team_l"] = sum(v for v in vols if v) if any(vols) else None
        r["n_log_rows"] = sum(1 for lg in logs.values() for x in lg["rows"] if x["_op"] == r["key"]) \
            if logs else None

    source = spec.get("rows", "operations")
    if source == "operations":
        base = [{"op": r} for r in sorted(op_rows.values(), key=lambda r: r["start_utc"])]
    elif source == "bottles":
        rows = [b for b in bottles if not teams or any(t in b["draws"] for t in teams)]
        base = [{"op": op_rows.get(b["label"], {}), "bottle": b} for b in rows]
        base.sort(key=lambda x: (x["op"].get("start_utc") or "", x["bottle"]["bottle"]))
    elif source.startswith("log:"):
        lg = logs.get(source)
        if lg is None:
            raise KeyError(f"logsheet {source} is not loaded")
        base = [{"op": op_rows.get(x["_op"]) or {}, "log": x} for x in lg["rows"]
                if x["_op"] is None or x["_op"] in keys or not keys]
    else:
        raise ValueError(f"unknown row source {source}")

    cols = [_col_meta(c, teams) for c in spec.get("columns", [])]
    body = [[_value(b, c["id"]) for c in cols] for b in base]
    return {"title": spec.get("title") or "", "rows": source, "columns": cols, "body": body}


def _col_meta(cid: str, teams: list[str]) -> dict:
    known = {c.id: c for c in op_columns() + BOTTLE_COLS}
    if cid in known:
        return known[cid].__dict__
    table, _, field = cid.partition(".")
    if table == "draw":
        return Col(cid, f"{field} (L)").__dict__
    return Col(cid, field).__dict__


def _value(base: dict, cid: str):
    table, _, field = cid.partition(".")
    if table == "op":
        return (base.get("op") or {}).get(field)
    if table == "bottle":
        return (base.get("bottle") or {}).get(field)
    if table == "draw":
        return ((base.get("bottle") or {}).get("draws") or {}).get(field)
    if table == "log":
        return (base.get("log") or {}).get(field)
    return None


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
