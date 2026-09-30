"""A report specification from the page, turned into content and a .docx.

The page sends one JSON document (saved as the participant's draft):

    {
      "leg": "2026_LEG_03", "leg_label": "Leg 3",
      "header": {"title", "leaders": [{name, email, affiliation}], "participants": [...]},
      "text": {"intro", "methods", "results", "references", "recommendations",
               "publications", "presentations", "in_progress"},
      "selection": {"groups": [...], "ops": [keys], "teams": [...],
                    "logsheets": [{"id", "sheet", "roles", "use", "bottles", "local"}],
                    "bottles": {"added": [keys], "removed": [keys],
                                "edits": {key: {"volume", "note"}}}},
      "conditions": {"narrative": "summary" | "stations" | null},
      "tables": [{"title", "rows", "columns": [...], "section"}],
      "figures": [{"kind": "map"|"profiles"|"ts"|"underway", "caption", "section", "options"}],
                  (a map's options: "ops" false leaves out the ticked operations;
                   "logs": {"<id>:<sheet>": {"show", "ids"}} plots a ticked log's
                   rows, with their sample ids when "ids" is set)
      "figure_times": "utc" | "ship"      (the underway record's time axis)
    }

``selection.ops`` is the explicit list of operations; the page fills it from
the ticked instruments and matched logsheet rows, and the participant can
untick any of them.
"""

from __future__ import annotations

from . import conditions, docxbuild, eventlog, figures, logsheets, tables

FIGURE_CAPTIONS = {
    "map": "Ship track and the stations sampled, by instrument.",
    "profiles": "CTD downcast profiles at the stations sampled.",
    "ts": "Temperature–salinity diagram for the CTD casts, coloured by pressure.",
    "underway": "The ship's underway record over the sampling period; grey lines mark the selected operations.",
}


def _logs(report: dict) -> dict[str, dict]:
    sel = report.get("selection", {})
    out = {}
    for lg in sel.get("logsheets", []):
        m = logsheets.matched(lg["id"], lg["sheet"], lg.get("roles") or {}, report["leg"],
                              sel.get("groups"), bool(lg.get("local")))
        m["use"] = lg.get("use", True)
        m["name"] = lg.get("name") or m.get("name")
        out[f"log:{lg['id']}:{lg['sheet']}"] = m
    return out


def log_layers(report: dict, wanted: dict) -> list[dict]:
    """The rows of each ticked log that ``wanted`` ({"<id>:<sheet>": {"show",
    "ids"}}) asks for, as map points. A row is placed by its own latitude and
    longitude when its sheet has them, else at the operation it matches, else
    where the ship was at its date and time (a flow-through sample, taken
    under way, matches no operation); a row with none of these is left off.
    Each point carries the row's sample id when ``ids`` is set and the sheet
    has a sample-id column."""
    leg = report["leg"]
    ops = track = span = None
    layers = []
    for key, m in _logs(report).items():
        want = wanted.get(key.removeprefix("log:")) or {}
        if not m["use"] or not want.get("show"):
            continue
        roles = m.get("roles") or {}
        id_col = roles.get("sample_id") if want.get("ids") else None
        points = []
        for r in m["rows"]:
            lat = logsheets._float(r.get(roles["lat"])) if roles.get("lat") else None
            lon = logsheets._float(r.get(roles["lon"])) if roles.get("lon") else None
            if (lat is None or lon is None) and r.get("_op"):
                ops = eventlog.by_key(leg) if ops is None else ops
                op = ops.get(r["_op"])
                lat, lon = (op.start.lat, op.start.lon) if op else (None, None)
            if lat is None or lon is None:
                span = logsheets.leg_span(leg) if span is None else span
                when = logsheets.row_time(r, roles, bool(m.get("local")), span)
                if when:
                    track = figures.ship_track(leg) if track is None else track
                    lon, lat = _ship_at(track, when)
            if lat is None or lon is None or not (-90 <= lat <= 90 and -180 <= lon <= 360):
                continue
            sid = r.get(id_col) if id_col else None
            points.append((lon, lat, None if logsheets._is_blank(sid) else str(sid).strip()))
        layers.append({"label": m["name"], "points": points})
    return layers


def _ship_at(track, when: str, reach_s: float = 1800) -> tuple[float | None, float | None]:
    """The ship's position (lon, lat) closest to ``when`` (ISO UTC) in
    ``track`` (rows of lon, lat, epoch seconds), if one is within ``reach_s``."""
    if track is None or not len(track):
        return None, None
    import numpy as np
    import pandas as pd
    t = pd.Timestamp(when).tz_localize("UTC").timestamp() if pd.Timestamp(when).tzinfo is None \
        else pd.Timestamp(when).timestamp()
    i = int(np.abs(track[:, 2] - t).argmin())
    return (float(track[i, 0]), float(track[i, 1])) if abs(track[i, 2] - t) <= reach_s else (None, None)


def logged_bottles(report: dict, logs: dict | None = None) -> dict:
    """The bottles the ticked logs list (tables.logged_bottles)."""
    logs = _logs(report) if logs is None else logs
    return tables.logged_bottles(logs, report.get("selection", {}).get("logsheets", []))


def figure(report: dict, spec: dict) -> list[bytes]:
    """One figure spec as one or more PNGs (underway panels split across images)."""
    leg = report["leg"]
    if spec["kind"] == "underway":
        keys = report.get("selection", {}).get("ops", [])
        return figures.underway_images(leg, conditions.table(leg, keys),
                                       (spec.get("options") or {}).get("panels"),
                                       report.get("figure_times") == "ship")
    return [_figure(report, spec)]


def _figure(report: dict, spec: dict) -> bytes:
    leg = report["leg"]
    keys = report.get("selection", {}).get("ops", [])
    opts = spec.get("options") or {}
    kind = spec["kind"]
    if kind == "map":
        return figures.station_map(leg, conditions.table(leg, keys) if opts.get("ops", True) else [],
                                   label_stations=opts.get("label_stations", True),
                                   colour=opts.get("track_colour") or "time",
                                   logs=log_layers(report, opts.get("logs") or {}))
    rosette_keys = [r["key"] for r in conditions.table(leg, keys)
                    if r["group"] in ("rosette", "tm_rosette")]
    if kind == "profiles":
        return figures.profiles(leg, rosette_keys, opts.get("variables"), opts.get("max_depth"),
                                bool(opts.get("compressed_depth")))
    if kind == "ts":
        bottles, label = None, "Bottles sampled"
        teams = report.get("selection", {}).get("teams", [])
        if opts.get("team_bottles"):
            logged = logged_bottles(report)
            picked = report.get("selection", {}).get("bottles")
            every = tables._bottles(leg, set(keys))
            chosen = tables.chosen_bottles(every, teams, logged, picked)
            bottles = [(b["salinity"], b["temperature"]) for b in every if tables.bottle_key(b) in chosen
                       and b.get("salinity") is not None and b.get("temperature") is not None]
            if teams or logged or (picked or {}).get("added"):
                label = "Bottles sampled by " + ", ".join(
                    teams + (["your logs"] if logged else []) + (["your picks"] if (picked or {}).get("added") else []))
        return figures.ts_diagram(leg, rosette_keys, bottles, label, opts.get("colour") or "pressure")
    raise ValueError(f"unknown figure kind {kind}")


def content(report: dict) -> dict:
    leg = report["leg"]
    sel = report.get("selection", {})
    keys = sel.get("ops", [])
    teams = sel.get("teams", [])
    logs = _logs(report)
    narratives = []
    mode = report.get("conditions", {}).get("narrative", "summary")
    if mode and keys:
        rows = conditions.table(leg, keys)
        narratives = ([conditions.summary(rows)] if mode in ("summary", True)
                      else [conditions.narrative(v) for v in conditions.visits(rows)])
    blocks = []
    for t in report.get("tables", []):
        if not t.get("columns"):
            continue
        tab = tables.build(leg, t, keys, teams, logs, logged_bottles(report, logs), sel.get("bottles"))
        blocks.append({"section": t.get("section", "methods"), "kind": "table",
                       "caption": t.get("title") or "", "table": tab})
    for f in report.get("figures", []):
        caption = f.get("caption") or FIGURE_CAPTIONS.get(f["kind"], "")
        pngs = figure(report, f)
        for i, png in enumerate(pngs, start=1):
            part = f" ({i} of {len(pngs)})" if len(pngs) > 1 else ""
            blocks.append({"section": f.get("section", "methods"), "kind": "figure",
                           "caption": caption.rstrip() + part, "png": png})
    return {"narratives": narratives, "blocks": blocks,
            "words": docxbuild.word_count(report, narratives)}


def docx_bytes(report: dict) -> bytes:
    return docxbuild.build(report, content(report))
