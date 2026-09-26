"""A report specification from the page, turned into content and a .docx.

The page sends one JSON document (saved as the participant's draft):

    {
      "leg": "2026_LEG_03", "leg_label": "Leg 3",
      "header": {"title", "leaders": [{name, email, affiliation}], "participants": [...]},
      "text": {"intro", "methods", "results", "references", "recommendations",
               "publications", "presentations", "in_progress"},
      "selection": {"groups": [...], "ops": [keys], "teams": [...],
                    "logsheets": [{"id", "sheet", "roles"}]},
      "conditions": {"narrative": "summary" | "stations" | null},
      "tables": [{"title", "rows", "columns": [...], "section"}],
      "figures": [{"kind": "map"|"profiles"|"ts"|"underway", "caption", "section", "options"}]
    }

``selection.ops`` is the explicit list of operations; the page fills it from
the ticked instruments and matched logsheet rows, and the participant can
untick any of them.
"""

from __future__ import annotations

from . import conditions, docxbuild, figures, logsheets, tables

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
                              sel.get("groups"))
        out[f"log:{lg['id']}:{lg['sheet']}"] = m
    return out


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
                                       (spec.get("options") or {}).get("panels"))
    return [_figure(report, spec)]


def _figure(report: dict, spec: dict) -> bytes:
    leg = report["leg"]
    keys = report.get("selection", {}).get("ops", [])
    opts = spec.get("options") or {}
    kind = spec["kind"]
    if kind == "map":
        return figures.station_map(leg, conditions.table(leg, keys),
                                   whole_leg_track=opts.get("whole_leg_track", True),
                                   label_stations=opts.get("label_stations", True))
    rosette_keys = [r["key"] for r in conditions.table(leg, keys)
                    if r["group"] in ("rosette", "tm_rosette")]
    if kind == "profiles":
        return figures.profiles(leg, rosette_keys, opts.get("variables"), opts.get("max_depth"))
    if kind == "ts":
        bottles, label = None, "Bottles sampled"
        teams = report.get("selection", {}).get("teams", [])
        if opts.get("team_bottles"):
            logged = logged_bottles(report)
            rows = [b for b in tables._bottles(leg, set(keys))
                    if not (teams or logged) or tables._team_bottle(b, teams, logged)]
            bottles = [(b["salinity"], b["temperature"]) for b in rows
                       if b.get("salinity") is not None and b.get("temperature") is not None]
            if teams or logged:
                label = "Bottles sampled by " + ", ".join(teams + (["your logs"] if logged else []))
        return figures.ts_diagram(leg, rosette_keys, bottles, label)
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
        tab = tables.build(leg, t, keys, teams, logs, logged_bottles(report, logs))
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
