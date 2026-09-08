"""Recent satellite imagery around the ship from the Copernicus Data Space
(Sentinel Hub processing API), rendered as one PNG per sensor for the map.

Two pictures: ``s1``, the newest Sentinel-1 radar passes (EW mode, HH) of
the last two days, which see ice through cloud and dark; ``s2``, the least
cloudy Sentinel-2 true colour of the last week, cloudy tiles left out. Each
covers the fixed ``REGION`` (the Queen Elizabeth Islands and the channels
around them: 74 °N to 83.5 °N, 130 °W to 60 °W), drawn in Web Mercator so
the map lays it between its corners without warping. The service caps a
request at 2500 pixels a side, so the region is fetched as ``TILES`` tiles
and stitched into one WebP.

``refresh()`` renders a sensor again once its picture is older than its
``max_age_h``; the timer runs it every half hour. The results live in
``db/sat/`` (the pictures and ``sat.json`` with corners, scene times and
cost) and the build copies them under ``data/sat/``. A render whose newest
scene is new goes to ``db/sat/archive/`` as well, dated by that scene, so
the map can step back through the pictures (``ARCHIVE_MAX`` per sensor).

Credentials: an OAuth client of the user's Copernicus account, as
``COPERNICUS_ID`` and ``COPERNICUS_SECRET`` in the environment (the
timer reads ``~/.config/underway/underway.env``). Without them nothing is
rendered and the map has no satellite layer.
"""

from __future__ import annotations

import json
import logging
import math
import os
from datetime import datetime, timedelta, timezone
from pathlib import Path

from .config import DB_DIR

log = logging.getLogger(__name__)

TOKEN_URL = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
PROCESS_URL = "https://sh.dataspace.copernicus.eu/api/v1/process"
CATALOG_URL = "https://sh.dataspace.copernicus.eu/api/v1/catalog/1.0.0/search"
WEBROOT = Path(os.environ.get("UNDERWAY_WEBROOT", "/data/underway/www"))
REGION = (-130.0, 74.0, -60.0, 83.5)   # lon west, lat south, lon east, lat north
MERC_M_PER_PX = 1950.0             # mercator metres a pixel: 540 m on the ground at 74 °N, 240 m at 83 °N
TILES = (2, 2)                     # requests across and down (each stays under the 2500 px cap)
WEBP_QUALITY = 80
ARCHIVE_MAX = 400                  # dated pictures kept per sensor (about 6 weeks of radar)
TIMEOUT = 240
R_EARTH = 6378137.0

# Sentinel-1: HH backscatter in dB, -25 dB black to 0 dB white; ice and land
# bright, calm open water dark. Sentinel-2: true colour, gain 2.5; a tile
# cloudier than maxCloudCoverage is left out, so cloud shows as basemap.
EVALSCRIPTS = {
    "s1": ("//VERSION=3\nfunction setup(){return {input:[\"HH\",\"dataMask\"],output:{bands:4}};}\n"
           "function evaluatePixel(s){var db=10*Math.log(s.HH)/Math.LN10; var v=Math.max(0,Math.min(1,(db+25)/25)); return [v,v,v,s.dataMask];}"),
    "s2": ("//VERSION=3\nfunction setup(){return {input:[\"B04\",\"B03\",\"B02\",\"dataMask\"],output:{bands:4}};}\n"
           "function evaluatePixel(s){var g=function(x){return Math.min(1,2.5*x);}; return [g(s.B04),g(s.B03),g(s.B02),s.dataMask];}"),
}
SENSORS = {
    "s1": {"type": "sentinel-1-grd", "days": 2, "max_age_h": 3.0, "filter": {"mosaickingOrder": "mostRecent"},
           "processing": {"backCoeff": "GAMMA0_ELLIPSOID"}, "label": "Sentinel-1 radar"},
    "s2": {"type": "sentinel-2-l2a", "days": 7, "max_age_h": 6.0, "filter": {"mosaickingOrder": "leastCC", "maxCloudCoverage": 40},
           "processing": {}, "label": "Sentinel-2 optical"},
    # the radar again at close to its native resolution, in a box that follows
    # the ship (a new one once it has moved NEAR_MOVE_KM); drawn over the region
    "s1near": {"type": "sentinel-1-grd", "days": 2, "max_age_h": 3.0, "filter": {"mosaickingOrder": "mostRecent"},
               "processing": {"backCoeff": "GAMMA0_ELLIPSOID"}, "label": "Sentinel-1 radar, 50 m near the ship",
               "near": True, "box_km": (240.0, 160.0), "ground_m_per_px": 50.0},
}
NEAR_MOVE_KM = 40.0
EVALSCRIPTS["s1near"] = EVALSCRIPTS["s1"]


def sat_dir() -> Path:
    return DB_DIR / "sat"


def credentials() -> tuple[str, str] | None:
    cid, sec = os.environ.get("COPERNICUS_ID", ""), os.environ.get("COPERNICUS_SECRET", "")
    return (cid, sec) if cid and sec else None


# ---------------------------------------------------------------- geometry

def to_mercator(lon: float, lat: float) -> tuple[float, float]:
    return R_EARTH * math.radians(lon), R_EARTH * math.log(math.tan(math.pi / 4 + math.radians(lat) / 2))


def from_mercator(x: float, y: float) -> tuple[float, float]:
    return math.degrees(x / R_EARTH), math.degrees(2 * math.atan(math.exp(y / R_EARTH)) - math.pi / 2)


def box_around(lat: float, lon: float, km: tuple[float, float]) -> list[float]:
    """The Web Mercator bbox [w, s, e, n] of a box ``km`` across on the
    ground at the ship: mercator metres are stretched by 1/cos(lat)."""
    cx, cy = to_mercator(lon, lat)
    k = 1 / math.cos(math.radians(lat))
    hw, hh = km[0] * 500 * k, km[1] * 500 * k
    return [cx - hw, cy - hh, cx + hw, cy + hh]


def region_bbox(region: tuple[float, float, float, float] = REGION) -> list[float]:
    """The region as a Web Mercator bbox [w, s, e, n]."""
    w, s = to_mercator(region[0], region[1]); e, n = to_mercator(region[2], region[3])
    return [w, s, e, n]


def region_size(bbox: list[float], m_per_px: float = MERC_M_PER_PX) -> tuple[int, int]:
    """Pixels across and down for the bbox at the given mercator metres a pixel."""
    w, s, e, n = bbox
    return max(1, round((e - w) / m_per_px)), max(1, round((n - s) / m_per_px))


def tiles(bbox: list[float], size: tuple[int, int], grid: tuple[int, int] = TILES) -> list[tuple[list[float], tuple[int, int], tuple[int, int]]]:
    """(tile bbox, tile size, paste offset) for each tile of a grid over
    the bbox, top-left first, row by row; the last row/column takes the
    remainder so the tiles cover the picture exactly."""
    w, s, e, n = bbox
    W, H = size
    nx, ny = grid
    xs = [round(W * i / nx) for i in range(nx + 1)]
    ys = [round(H * j / ny) for j in range(ny + 1)]
    out = []
    for j in range(ny):
        for i in range(nx):
            px0, px1, py0, py1 = xs[i], xs[i + 1], ys[j], ys[j + 1]
            tb = [w + (e - w) * px0 / W, n - (n - s) * py1 / H, w + (e - w) * px1 / W, n - (n - s) * py0 / H]
            out.append((tb, (px1 - px0, py1 - py0), (px0, py0)))
    return out


def corners(bbox: list[float]) -> list[list[float]]:
    """The box's corners as [lon, lat], top-left clockwise (what a map image layer wants)."""
    w, s, e, n = bbox
    tl, br = from_mercator(w, n), from_mercator(e, s)
    return [[tl[0], tl[1]], [br[0], tl[1]], [br[0], br[1]], [tl[0], br[1]]]


def distance_km(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    p1, p2 = math.radians(lat1), math.radians(lat2)
    a = math.sin((p2 - p1) / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(math.radians(lon2 - lon1) / 2) ** 2
    return 2 * 6371.0 * math.asin(math.sqrt(a))


# ---------------------------------------------------------------- the service

def token(creds: tuple[str, str]) -> str:
    import requests
    r = requests.post(TOKEN_URL, data={"grant_type": "client_credentials", "client_id": creds[0], "client_secret": creds[1]}, timeout=30)
    r.raise_for_status()
    return r.json()["access_token"]


def newest_scene(tok: str, kind: str, bbox: list[float], start: datetime, end: datetime) -> str | None:
    """The acquisition time of the newest scene of ``kind`` touching the box
    in the range, so the layer can say how old its picture is."""
    import requests
    w, s, e, n = bbox
    ll = from_mercator(w, s); ur = from_mercator(e, n)
    q = {"collections": [SENSORS[kind]["type"]], "bbox": [ll[0], ll[1], ur[0], ur[1]],
         "datetime": f"{start:%Y-%m-%dT%H:%M:%SZ}/{end:%Y-%m-%dT%H:%M:%SZ}", "limit": 50}
    try:
        r = requests.post(CATALOG_URL, json=q, headers={"Authorization": f"Bearer {tok}"}, timeout=60)
        r.raise_for_status()
        times = [f["properties"]["datetime"] for f in r.json().get("features", [])]
        return max(times) if times else None
    except Exception as e:                  # noqa: BLE001 — the picture still counts without a date
        log.info("satellite: catalog search failed: %s", e)
        return None


def render_tile(tok: str, kind: str, bbox: list[float], size: tuple[int, int], end: datetime) -> tuple[bytes, float]:
    """One PNG tile for ``kind`` over the bbox, and the processing units it cost."""
    import requests
    sp = SENSORS[kind]
    start = end - timedelta(days=sp["days"])
    body = {"input": {"bounds": {"bbox": bbox, "properties": {"crs": "http://www.opengis.net/def/crs/EPSG/0/3857"}},
                      "data": [{"type": sp["type"],
                                "dataFilter": {"timeRange": {"from": start.strftime("%Y-%m-%dT%H:%M:%SZ"), "to": end.strftime("%Y-%m-%dT%H:%M:%SZ")}, **sp["filter"]},
                                "processing": sp["processing"]}]},
            "output": {"width": size[0], "height": size[1], "responses": [{"identifier": "default", "format": {"type": "image/png"}}]},
            "evalscript": EVALSCRIPTS[kind]}
    r = requests.post(PROCESS_URL, json=body, headers={"Authorization": f"Bearer {tok}"}, timeout=TIMEOUT)
    if r.status_code != 200:
        raise RuntimeError(f"{r.status_code}: {r.text[:200]}")
    return r.content, float(r.headers.get("x-processingunits-spent") or 0)


def render(tok: str, kind: str, bbox: list[float], end: datetime, m_per_px: float = MERC_M_PER_PX) -> tuple[bytes, float, tuple[int, int]]:
    """The whole box for ``kind`` as one WebP (tiles fetched and stitched),
    the processing units it cost, and its size in pixels."""
    import io
    from PIL import Image
    size = region_size(bbox, m_per_px)
    out = Image.new("RGBA", size, (0, 0, 0, 0))
    cost = 0.0
    grid = (1 if size[0] <= 2500 else 2, 1 if size[1] <= 2500 else 2)
    for tb, tsize, offset in tiles(bbox, size, grid):
        png, c = render_tile(tok, kind, tb, tsize, end)
        cost += c
        out.paste(Image.open(io.BytesIO(png)).convert("RGBA"), offset)
    buf = io.BytesIO()
    out.save(buf, format="WEBP", quality=WEBP_QUALITY, method=4)
    return buf.getvalue(), cost, size


# ---------------------------------------------------------------- the refresh

def ship_position() -> tuple[float, float] | None:
    """(lat, lon) of the newest fix the dashboard has published."""
    p = WEBROOT / "data" / "manifest.json"
    try:
        latest = json.loads(p.read_text()).get("latest") or {}
        if latest.get("lat") is not None and latest.get("lon") is not None:
            return float(latest["lat"]), float(latest["lon"])
    except (OSError, ValueError, TypeError):
        pass
    return None


def load_info() -> dict:
    p = sat_dir() / "sat.json"
    try:
        return json.loads(p.read_text()) if p.is_file() else {}
    except (OSError, ValueError):
        return {}


def due(info: dict, now: datetime, kinds=tuple(SENSORS), ship: tuple[float, float] | None = None) -> list[str]:
    """Which sensors want a new picture: those without one (or with one of
    another region), those older than their max_age_h, and a near box the
    ship has moved NEAR_MOVE_KM away from."""
    out = []
    for k in kinds:
        sp = SENSORS[k]
        cur = (info.get("images") or {}).get(k)
        if sp.get("near") and ship is None:
            continue                                     # no fix yet: nothing to centre on
        if not cur or (not sp.get("near") and cur.get("region") != list(REGION)):
            out.append(k); continue
        try:
            age = (now - datetime.fromisoformat(cur["fetched"])).total_seconds() / 3600
            moved = distance_km(ship[0], ship[1], cur["centre"][0], cur["centre"][1]) if sp.get("near") else 0.0
        except (KeyError, ValueError, TypeError, IndexError):
            out.append(k); continue
        if age >= sp["max_age_h"] or moved >= NEAR_MOVE_KM:
            out.append(k)
    return out


def refresh(force: bool = False, now: datetime | None = None, kinds=tuple(SENSORS)) -> dict:
    """Render what is due and has a new scene (everything with ``force``);
    returns the info written."""
    now = now or datetime.now(timezone.utc)
    creds = credentials()
    if creds is None:
        log.info("satellite: no COPERNICUS_ID/COPERNICUS_SECRET; nothing rendered")
        return load_info()
    info = load_info()
    info.setdefault("images", {})
    ship = ship_position()
    wanted = [k for k in kinds if not (SENSORS[k].get("near") and ship is None)] if force else due(info, now, kinds, ship)
    if not wanted:
        return info
    sat_dir().mkdir(parents=True, exist_ok=True)
    try:
        tok = token(creds)
    except Exception as e:                  # noqa: BLE001 — the link is down or the service is: the next run
        log.warning("satellite: no token (%s); nothing rendered", str(e).split("(Caused by")[0][:160])
        return info
    changed = False
    for k in wanted:
        sp = SENSORS[k]
        try:
            if sp.get("near"):
                bbox = box_around(ship[0], ship[1], sp["box_km"])
                m_per_px = sp["ground_m_per_px"] / math.cos(math.radians(ship[0]))
            else:
                bbox, m_per_px = region_bbox(), MERC_M_PER_PX
            # the catalog is free and a render costs processing units: a
            # picture is bought only when the newest scene in the box is not
            # the one already on the map (or, near the ship, the box moved)
            scene = newest_scene(tok, k, bbox, now - timedelta(days=sp["days"]), now)
            cur = info["images"].get(k) or {}
            if not force and cur:
                moved = distance_km(ship[0], ship[1], cur["centre"][0], cur["centre"][1]) if sp.get("near") and cur.get("centre") else 0.0
                if scene is None:
                    log.info("satellite: %s kept: the catalog did not answer, the picture stays", k)
                    continue
                if scene == cur.get("scene") and moved < NEAR_MOVE_KM:
                    log.info("satellite: %s unchanged (newest scene %s); not rendered", k, scene)
                    continue
            data, cost, size = render(tok, k, bbox, now, m_per_px)
            changed = True
            tmp = sat_dir() / f"{k}.webp.tmp"
            tmp.write_bytes(data)
            os.replace(tmp, sat_dir() / f"{k}.webp")
            info["images"][k] = {"file": f"{k}.webp", "label": sp["label"], "corners": corners(bbox), "region": None if sp.get("near") else list(REGION),
                                 "centre": list(ship) if sp.get("near") else None, "size": list(size), "fetched": now.isoformat(timespec="seconds"), "scene": scene,
                                 "days": sp["days"], "bytes": len(data), "cost_pu": cost}
            info["cost_pu_total"] = round(float(info.get("cost_pu_total") or 0) + cost, 2)
            if not sp.get("near"):
                archive(info, k, data, scene, now)
            log.info("satellite: %s rendered (%dx%d, %d kB, newest scene %s, %.1f PU)", k, size[0], size[1], len(data) // 1024, scene, cost)
        except Exception as e:                  # noqa: BLE001 — one sensor failing must not stop the other
            log.warning("satellite: %s failed: %s", k, e)
    if not changed:
        return info
    tmp = sat_dir() / "sat.json.tmp"
    tmp.write_text(json.dumps(info, indent=1))
    os.replace(tmp, sat_dir() / "sat.json")
    return info


def archive(info: dict, kind: str, data: bytes, scene: str | None, now: datetime) -> None:
    """Keep the picture under ``archive/<kind>_<stamp>.webp`` when its newest
    scene is one the archive has not seen (a render that only repeats the
    last mosaic is not worth the disk); the oldest go once past ARCHIVE_MAX."""
    entries = info.setdefault("archive", {}).setdefault(kind, [])
    when = scene or now.isoformat(timespec="seconds")
    if entries and entries[-1].get("scene") == when:
        return
    stamp = "".join(ch for ch in when if ch.isdigit())[:14]
    d = sat_dir() / "archive"
    d.mkdir(parents=True, exist_ok=True)
    name = f"{kind}_{stamp}.webp"
    (d / name).write_bytes(data)
    entries.append({"file": name, "scene": when, "fetched": now.isoformat(timespec="seconds"), "bytes": len(data)})
    while len(entries) > ARCHIVE_MAX:
        old = entries.pop(0)
        try:
            (d / old["file"]).unlink()
        except OSError:
            pass


def publish(root: Path) -> dict | None:
    """Copy the pictures under ``root/data/sat`` for the page; the manifest
    entry (corners, times, file URLs) or None when there is nothing."""
    import shutil
    info = load_info()
    imgs = info.get("images") or {}
    if not imgs:
        return None
    dest = root / "data" / "sat"
    dest.mkdir(parents=True, exist_ok=True)
    out = {}
    for k, im in imgs.items():
        src = sat_dir() / im["file"]
        if not src.is_file():
            continue
        d = dest / im["file"]
        if not d.exists() or d.stat().st_mtime < src.stat().st_mtime:
            shutil.copy2(src, d)
        stamp = "".join(ch for ch in im["fetched"] if ch.isdigit())[:14]   # a cache-buster: the fetch time, digits only
        out[k] = {**im, "url": f"data/sat/{im['file']}?v={stamp}"}
    # the archive: dated pictures the map can step back through (all of one
    # region share the current picture's corners); only new files are copied
    arch = {}
    adest = dest / "archive"
    for k, entries in (info.get("archive") or {}).items():
        if k not in out:
            continue
        rows = []
        for e in entries:
            src = sat_dir() / "archive" / e["file"]
            if not src.is_file():
                continue
            adest.mkdir(parents=True, exist_ok=True)
            if not (adest / e["file"]).exists():
                shutil.copy2(src, adest / e["file"])
            rows.append({**e, "url": f"data/sat/archive/{e['file']}"})
        if rows:
            arch[k] = rows
    return {"images": out, "archive": arch, "cost_pu_total": info.get("cost_pu_total")} if out else None
