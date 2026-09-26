"""Instrument groups for the event log's free-text Activity column.

The bridge types Activity by hand, so one instrument appears under many
spellings ("TM Rosette", "TM-Rosette A2", "TM-CTD"). A participant picks
groups, not spellings; the original text is kept on every operation.

An entry the bridge marked as an error ("erreur, à enlever") goes to the
``void`` group, which the page does not offer.

Rules are tried in order and the first match wins, so a specific rule
("TM" before "Rosette", "IKMT" before "Beam Trawl") must come first.
"""

from __future__ import annotations

import re

GROUPS: list[tuple[str, str, str]] = [
    # (group id, label, pattern on the lower-cased activity)
    ("void", "Entries marked as errors", r"erreur|\berror"),
    ("mvp", "Moving Vessel Profiler", r"\bmvp\b"),
    ("transit", "Transit", r"^transit"),
    ("tm_rosette", "Trace-metal rosette", r"\btm[- ]?(rosette|ctd)"),
    ("rosette", "CTD-Rosette", r"rosette|^ctd\b"),
    ("box_core", "Box core", r"box ?core"),
    ("multicorer", "Multicorer", r"multicorer"),
    ("gravity_piston", "Gravity / piston core", r"gravity|piston|\bggc\b|\bgcc\b|^pc\b"),
    ("grab", "Van Veen grab", r"van veen"),
    ("ikmt_beam", "IKMT / beam trawl", r"ikmt|beam ?trawl|agassiz"),
    ("plankton_nets", "Plankton nets (Monster, Hydrobios, Tucker)", r"monster|hydrobios|tucker"),
    ("drop_cam", "Drop camera", r"drop ?cam"),
    ("baited_cam", "Baited camera / lander", r"baited|lander"),
    ("rov", "ROV / SubOcean", r"\brov\b|subocean"),
    ("mooring", "Mooring", r"moor"),
    ("ice", "Sea-ice station", r"^ice\b|snow catcher"),
    ("optics", "Optics (C-OPS, Hydroscat)", r"c-ops|hydroscat"),
    ("pumps", "In-situ pumps", r"in-?situ pump"),
    ("small_craft", "Zodiac / barge / diving", r"zodiac|barge|scuba|ram_kelp"),
    ("helicopter", "Helicopter", r"h[ée]lic"),
    ("mapping", "Mapping", r"mapping"),
    ("ship", "Ship operations", r"crew change|small boat|sea ?trial|shelter"),
]

# How a count of operations reads in prose: (singular, plural).
NOUNS = {
    "rosette": ("CTD-rosette cast", "CTD-rosette casts"),
    "tm_rosette": ("trace-metal rosette cast", "trace-metal rosette casts"),
    "box_core": ("box core", "box cores"),
    "multicorer": ("multicorer deployment", "multicorer deployments"),
    "gravity_piston": ("gravity or piston core", "gravity or piston cores"),
    "grab": ("Van Veen grab", "Van Veen grabs"),
    "ikmt_beam": ("IKMT or beam trawl", "IKMT or beam trawls"),
    "plankton_nets": ("plankton net tow", "plankton net tows"),
    "drop_cam": ("drop-camera deployment", "drop-camera deployments"),
    "baited_cam": ("baited-camera or lander deployment", "baited-camera or lander deployments"),
    "rov": ("ROV dive", "ROV dives"),
    "mvp": ("MVP line", "MVP lines"),
    "mooring": ("mooring operation", "mooring operations"),
    "ice": ("ice station", "ice stations"),
    "optics": ("optics profile", "optics profiles"),
    "pumps": ("in-situ pump deployment", "in-situ pump deployments"),
    "small_craft": ("small-craft operation", "small-craft operations"),
    "helicopter": ("helicopter flight", "helicopter flights"),
    "mapping": ("mapping survey", "mapping surveys"),
}


def noun(group: str, n: int) -> str:
    one, many = NOUNS.get(group, ("operation", "operations"))
    return f"one {one}" if n == 1 else f"{n} {many}"


# The order the page lists them in: science first, logistics last.
DISPLAY_ORDER = ["rosette", "tm_rosette", "box_core", "multicorer", "gravity_piston", "grab",
                 "ikmt_beam", "plankton_nets", "drop_cam", "baited_cam", "rov", "mvp", "mooring",
                 "ice", "optics", "pumps", "small_craft", "helicopter", "mapping", "transit", "ship",
                 "other"]

_COMPILED = [(gid, label, re.compile(p)) for gid, label, p in GROUPS]
LABELS = {gid: label for gid, label, _ in GROUPS} | {"other": "Other"}


def group_of(activity: str | None) -> str:
    text = " ".join(str(activity or "").lower().split())
    for gid, _, rx in _COMPILED:
        if rx.search(text):
            return gid
    return "other"
