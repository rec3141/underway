"""The leg's cruise plan from a Google Earth KMZ: the planned track(s) and
the stations, for the map's Plan layer.

The plan lives in ``db/plan/plan.kmz`` (a KMZ or a bare KML). The build
publishes it as ``data/plan.json``; the page can also replace it by dropping
a new file on the map (``POST /api/plan``), which writes the same two files.

A KMZ from the expedition office holds one Document with a folder of
communities (skipped: the map has its own), one or two cruise-track
LineStrings ("Plan A ...", "Altern ...") and folders of station Points
grouped by programme. Station names and folder paths are kept; the
LineString whose name says Altern is the alternate track.
"""

from __future__ import annotations

import io
import json
import logging
import os
import re
import xml.etree.ElementTree as ET
import zipfile
from datetime import datetime, timezone
from pathlib import Path

from .config import DB_DIR

log = logging.getLogger(__name__)
KML_NS = "{http://www.opengis.net/kml/2.2}"
SKIP_FOLDERS = ("communit",)          # folders (lower-cased prefix) left out: the map has its own settlements
MAX_BYTES = 8 * 1024 * 1024


def plan_path() -> Path:
    return DB_DIR / "plan" / "plan.kmz"


def _tag(e) -> str:
    return e.tag.split("}")[-1]


def _text(e, name: str) -> str:
    c = e.find(KML_NS + name)
    return (c.text or "").strip() if c is not None else ""


def _coords(e) -> list[list[float]]:
    c = e.find(".//" + KML_NS + "coordinates")
    out = []
    for tok in (c.text or "").split() if c is not None else []:
        parts = tok.split(",")
        try:
            lon, lat = float(parts[0]), float(parts[1])
        except (ValueError, IndexError):
            continue
        out.append([round(lon, 5), round(lat, 5)])
    return out


# the expedition office's schema: a SimpleData per column of its station
# sheet; the ones worth keeping, under short names
EXTENDED = {"Station_Type": "type", "Region": "region", "Operations": "ops", "Depth__m_": "depth_m", "Operation_Time__hours_": "hours", "__of_ops": "n_ops"}


def _extended(pm) -> dict:
    """A placemark's ExtendedData (SimpleData name/value pairs) under the
    EXTENDED names; depth and hours as numbers."""
    out = {}
    for sd in pm.iter(KML_NS + "SimpleData"):
        key = EXTENDED.get(sd.get("name", ""))
        val = (sd.text or "").strip()
        if not key or not val:
            continue
        if key in ("depth_m", "hours"):
            try:
                val = float(val)
            except ValueError:
                continue
        out[key] = val
    return out


def kml_bytes(data: bytes) -> bytes:
    """The KML inside a KMZ (its doc.kml, else the first .kml), or the bytes
    themselves when they already are KML."""
    if data[:2] == b"PK":
        with zipfile.ZipFile(io.BytesIO(data)) as z:
            names = [n for n in z.namelist() if n.lower().endswith(".kml")]
            if not names:
                raise ValueError("no .kml inside the KMZ")
            pick = "doc.kml" if "doc.kml" in names else names[0]
            return z.read(pick)
    return data


def parse(data: bytes) -> dict:
    """The plan: name, tracks (name, alternate flag, coordinates) and
    stations (name, group path, lon, lat, description)."""
    root = ET.fromstring(kml_bytes(data))
    doc = root.find(KML_NS + "Document")
    if doc is None:
        doc = root
    name = _text(doc, "name") or "cruise plan"
    tracks, stations = [], []
    # a lone folder under the Document merely wraps the plan: its name is not
    # part of any station's group
    top = [c for c in doc if _tag(c) == "Folder"]
    wrapper = top[0] if len(top) == 1 else None

    def walk(e, path):
        for c in e:
            t = _tag(c)
            if t in ("Folder", "Document"):
                fname = _text(c, "name")
                if any(fname.lower().startswith(s) for s in SKIP_FOLDERS):
                    continue
                walk(c, path + ([fname] if fname and c is not wrapper else []))
            elif t == "Placemark":
                pname = _text(c, "name")
                if c.find(".//" + KML_NS + "LineString") is not None:
                    pts = _coords(c.find(".//" + KML_NS + "LineString"))
                    if len(pts) >= 2:
                        tracks.append({"name": pname, "alternate": bool(re.search(r"altern", pname, re.I)), "coords": pts})
                elif c.find(".//" + KML_NS + "Point") is not None:
                    pts = _coords(c.find(".//" + KML_NS + "Point"))
                    if pts:
                        desc = re.sub(r"<[^>]+>", " ", _text(c, "description")).strip()
                        st = {"name": pname, "group": " / ".join(path), "lon": pts[0][0], "lat": pts[0][1], **({"desc": desc[:300]} if desc else {})}
                        st.update(_extended(c))
                        stations.append(st)
    walk(doc, [])
    if not tracks and not stations:
        raise ValueError("no tracks or stations found in the file")
    groups = sorted({s["group"] for s in stations})
    return {"name": name, "tracks": tracks, "stations": stations, "groups": groups}


def load() -> dict | None:
    p = plan_path()
    if not p.is_file():
        return None
    try:
        out = parse(p.read_bytes())
        out["stamp"] = datetime.fromtimestamp(p.stat().st_mtime, timezone.utc).isoformat(timespec="seconds")
        return out
    except (OSError, ValueError, ET.ParseError, zipfile.BadZipFile) as e:
        log.warning("plan: cannot read %s (%s)", p, e)
        return None


def save(data: bytes) -> dict:
    """Store a new plan (validated first) and return it parsed."""
    if len(data) > MAX_BYTES:
        raise ValueError("file too large")
    out = parse(data)
    p = plan_path()
    p.parent.mkdir(parents=True, exist_ok=True)
    tmp = p.with_suffix(".tmp")
    tmp.write_bytes(data)
    os.replace(tmp, p)
    out["stamp"] = datetime.now(timezone.utc).isoformat(timespec="seconds")
    return out


def publish(root: Path, plan: dict | None = None) -> dict | None:
    """Write ``root/data/plan.json``; the manifest entry, or None without a plan."""
    plan = plan or load()
    if not plan:
        return None
    dest = root / "data" / "plan.json"
    dest.parent.mkdir(parents=True, exist_ok=True)
    tmp = dest.with_suffix(".tmp")
    tmp.write_text(json.dumps(plan, separators=(",", ":")))
    os.replace(tmp, dest)
    stamp = "".join(ch for ch in plan.get("stamp", "") if ch.isdigit())[:14]
    return {"file": f"data/plan.json?v={stamp}", "name": plan["name"], "n_stations": len(plan["stations"]),
            "n_tracks": len(plan["tracks"]), "stamp": plan.get("stamp")}
