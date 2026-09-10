"""Photographs from the ship's share into the journal.

People put their pictures on the share (``/Share/<year>/<leg>/Pictures/…``).
The Nature tab lets them point at a folder there, say who they are and how
the pictures may be used, and import it: each photograph becomes a line of
the ship's journal (``nature.append``) with the picture beside it, so it
stands on the map and in the record and goes up to grid with the next push.
A folder, never single files; a photograph already in the journal (the
registry ``db/photos/imported.json``) is passed over, so a folder can be
imported again for what is new in it. With their permission, a folder is
watched: every ten minutes the ship looks for photographs added to it and
imports them under the same name and licence, until they stop it.

Where and when come from the photograph itself: the EXIF time, read in the
camera's own zone when the camera wrote one, else in the zone the importer
names (the ship's, by default); the EXIF position when the camera had a GPS,
else the ship's position at that moment from the leg's own record (the
``obs`` table of the leg databases). A photograph without a time, or taken
when the ship's record has no fix, is reported and left out.

What is in the picture comes from the local vision model: the photographs go
through it in contact sheets, nine numbered tiles to a sheet, and it answers
with a caption, tags and a subject for each number. A subject that names a
published subject (by any of its names) becomes that subject's line; any
other stays as written, and grid asks before it is entered.

Nothing here touches the share: it is read, and the copies are the journal's.
"""

from __future__ import annotations

import base64
import hashlib
import io
import json
import logging
import re
import shutil
import sqlite3
import subprocess
import threading
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

from PIL import Image, ImageDraw, ImageFont, ImageOps

from .config import DB_DIR, LOCAL_TZ, SHARE_ROOT

log = logging.getLogger(__name__)

IMAGE_EXT = {".jpg", ".jpeg", ".png", ".webp"}
PHOTOS_DIR = DB_DIR / "photos"                  # the ship's own: thumbnails of the share, the import records
THUMB_DIR = PHOTOS_DIR / "thumbs"
JOBS_DIR = PHOTOS_DIR / "imports"
MAX_FILES = 300                                 # photographs in one import
MAX_LIST = 2000                                 # entries read from one folder of the share
SHEET_N = 9                                     # tiles to a contact sheet
TILE = 480                                      # a tile's side, pixels
PHOTO_MAX = 2400                                # the journal's copy: the long side, pixels
NEAR_S = 900                                    # a fix this close in time places a photograph
IMPORTED = PHOTOS_DIR / "imported.json"         # photograph (its path on the share) -> the journal id it became
WATCHES = PHOTOS_DIR / "watches.json"           # the folders watched, with the form each was given
WATCH_S = 600                                   # how often the watched folders are looked at
SETTLE_S = 120                                  # a photograph younger than this may still be copying: next time
LEG_DB = re.compile(r"^\d{4}_LEG_\d+\.db$")
LICENCES = {                                    # what the form offers; the codes grid's writer knows
    "attribution": "reusable with attribution, the photographer credited",
    "cc-by-4.0": "CC BY 4.0",
    "cc-by-sa-4.0": "CC BY-SA 4.0",
    "cc-by-nc-4.0": "CC BY-NC 4.0",
    "cc0": "CC0, no rights reserved",
    "rights-reserved": "all rights reserved: shown on the ship's wiki, not for reuse",
}
CLOCKS = ("exif", "ship", "utc")                # or an offset, +02:00: what the camera's clock was set to
OFFSET_RX = re.compile(r"^[+-]\d{2}:\d{2}$")
_JOBS: dict[str, dict] = {}
_lock = threading.Lock()                        # one import runs at a time: one model, one share
_reg_lock = threading.Lock()                    # the registry and the watches, read and written whole


# ---------------------------------------------------------------- the share
def _safe(rel: str) -> Path:
    """A path under the share, or a refusal: the browser names folders and
    files by their path relative to the share's root, nothing else."""
    root = SHARE_ROOT.resolve()
    rel = str(rel or "").replace("\\", "/").strip("/")
    if any(part in ("..", "") for part in Path(rel).parts):
        raise ValueError("no such folder")
    try:
        p = (root / rel).resolve()
    except (OSError, RuntimeError) as e:
        raise ValueError("no such folder") from e
    if not p.is_relative_to(root):
        raise ValueError("no such folder")
    return p


def rel_of(p: Path) -> str:
    return str(p.resolve().relative_to(SHARE_ROOT.resolve())).replace("\\", "/")


def start_path(root: Path | None = None) -> str:
    """Where the browser opens: the live leg's Pictures folder (the leg the
    dashboard's manifest names), else the newest leg's that has photographs
    in it, else as far down that path as the share has."""
    def pictures(leg: Path) -> Path | None:
        return next((d for d in leg.iterdir() if d.is_dir() and d.name.lower() in ("pictures", "photos", "photographs")), None)
    try:
        live = ""
        if root:
            try:
                live = str(json.loads((root / "data" / "manifest.json").read_text(encoding="utf-8")).get("live") or "")
            except (OSError, ValueError):
                live = ""
        if re.fullmatch(r"\d{4}_LEG_\d+", live) and (SHARE_ROOT / live[:4] / live).is_dir():
            leg = SHARE_ROOT / live[:4] / live
            return rel_of(pictures(leg) or leg)
        years = sorted((d for d in SHARE_ROOT.iterdir() if d.is_dir() and re.fullmatch(r"\d{4}", d.name)), key=lambda d: d.name)
        if not years:
            return ""
        year = years[-1]
        legs = sorted((d for d in year.iterdir() if d.is_dir() and re.fullmatch(r"\d{4}_LEG_\d+", d.name)), key=lambda d: d.name)
        if not legs:
            return rel_of(year)
        for leg in reversed(legs):                      # the newest leg with photographs, else the newest leg
            pics = pictures(leg)
            if pics and any(x.suffix.lower() in IMAGE_EXT or x.is_dir() for x in pics.iterdir()):
                return rel_of(pics)
        return rel_of(pictures(legs[-1]) or legs[-1])
    except OSError:
        return ""


def listing(rel: str) -> dict:
    """One folder of the share: its folders (with a count of the photographs
    in each) and its photographs, for the browser."""
    p = _safe(rel)
    if not p.is_dir():
        raise ValueError("no such folder")
    folders, files = [], []
    for i, c in enumerate(sorted(p.iterdir(), key=lambda x: x.name.lower())):
        if i >= MAX_LIST:
            break
        if c.name.startswith((".", "~$")):
            continue
        try:
            if c.is_dir():
                n = 0
                for j, x in enumerate(c.iterdir()):
                    if j >= MAX_LIST:
                        break
                    if x.suffix.lower() in IMAGE_EXT:
                        n += 1
                folders.append({"name": c.name, "images": n})
            elif c.suffix.lower() in IMAGE_EXT:
                st = c.stat()
                files.append({"name": c.name, "size": st.st_size, "mtime": datetime.fromtimestamp(st.st_mtime, timezone.utc).isoformat(timespec="seconds")})
        except OSError:
            continue
    here = rel_of(p) if p != SHARE_ROOT.resolve() else ""
    parent = None if not here else (str(Path(here).parent) if "/" in here else "")
    return {"path": here, "parent": parent, "folders": folders, "files": files[:MAX_FILES], "more": max(0, len(files) - MAX_FILES)}


def thumb(rel: str, size: int = 200) -> bytes:
    """A small JPEG of one photograph on the share, kept in the ship's cache
    by the file's size and time, so the browser's grid is quick."""
    p = _safe(rel)
    if not p.is_file() or p.suffix.lower() not in IMAGE_EXT:
        raise ValueError("no such photograph")
    st = p.stat()
    key = hashlib.sha1(f"{p}|{st.st_size}|{int(st.st_mtime)}|{size}".encode()).hexdigest()
    THUMB_DIR.mkdir(parents=True, exist_ok=True)
    cached = THUMB_DIR / f"{key}.jpg"
    if cached.is_file():
        return cached.read_bytes()
    with Image.open(p) as im:
        im.draft("RGB", (size * 2, size * 2))
        im = ImageOps.exif_transpose(im).convert("RGB")
        im.thumbnail((size, size))
        buf = io.BytesIO()
        im.save(buf, "JPEG", quality=80)
    cached.write_bytes(buf.getvalue())
    return buf.getvalue()


# ---------------------------------------------------------------- when and where
EXIFTOOL = shutil.which("exiftool")
EXIF_TAGS = ["-DateTimeOriginal", "-CreateDate", "-OffsetTimeOriginal", "-OffsetTime", "-GPSLatitude", "-GPSLongitude",
             "-GPSLatitudeRef", "-GPSLongitudeRef", "-Make", "-Model"]


def exif_many(paths: list[Path]) -> dict[Path, dict]:
    """The EXIF of many photographs at once: exiftool, which reads every
    camera's GPS block (Pillow misses a drone's), in one call per hundred
    files; Pillow for each file when exiftool is not installed."""
    out: dict[Path, dict] = {}
    if EXIFTOOL:
        for k in range(0, len(paths), 100):
            chunk = paths[k:k + 100]
            try:
                r = subprocess.run([EXIFTOOL, "-j", "-n", "-q", "-fast2", *EXIF_TAGS, *map(str, chunk)], capture_output=True, text=True, timeout=600)
                rows = json.loads(r.stdout or "[]")
            except (OSError, ValueError, subprocess.SubprocessError) as e:
                log.info("exiftool did not answer (%s); Pillow reads the EXIF", e)
                rows = []
            for row in rows:
                src = Path(row.get("SourceFile", ""))
                match = next((q for q in chunk if q.resolve() == src.resolve() or str(q) == str(src)), None)
                if match:
                    out[match] = _exif_row(row)
    for q in paths:
        if q not in out:
            out[q] = exif_pillow(q)
    return out


def _exif_row(row: dict) -> dict:
    """One exiftool JSON row (-n: numbers, GPS signed by the Composite tags)
    as the record exif_of gives."""
    out = {"taken": None, "offset": None, "lat": None, "lon": None,
           "model": " ".join(str(row.get(k) or "").strip() for k in ("Make", "Model")).strip()}
    m = re.match(r"^(\d{4}):(\d{2}):(\d{2})[ T](\d{2}):(\d{2}):(\d{2})", str(row.get("DateTimeOriginal") or row.get("CreateDate") or ""))
    if m:
        out["taken"] = f"{m[1]}-{m[2]}-{m[3]}T{m[4]}:{m[5]}:{m[6]}"
    off = str(row.get("OffsetTimeOriginal") or row.get("OffsetTime") or "").strip()
    if OFFSET_RX.match(off):
        out["offset"] = off
    try:
        lat, lon = row.get("GPSLatitude"), row.get("GPSLongitude")
        if lat is not None and lon is not None:
            lat, lon = float(lat), float(lon)
            if str(row.get("GPSLatitudeRef") or "").upper().startswith("S") and lat > 0:
                lat = -lat
            if str(row.get("GPSLongitudeRef") or "").upper().startswith("W") and lon > 0:
                lon = -lon
            if -90 <= lat <= 90 and -180 <= lon <= 180 and not (lat == 0 and lon == 0):
                out["lat"], out["lon"] = lat, lon
    except (TypeError, ValueError):
        pass
    return out


def exif_of(path: Path) -> dict:
    """The time the camera wrote (as it wrote it, no zone), the zone if it
    wrote one, the GPS position if it had one, and the camera's name."""
    return exif_many([path])[path]


def exif_pillow(path: Path) -> dict:
    """The same, read by Pillow alone."""
    out = {"taken": None, "offset": None, "lat": None, "lon": None, "model": ""}
    dt = off = None
    try:
        with Image.open(path) as im:
            ex = im.getexif()
            out["model"] = " ".join(str(ex.get(k) or "").strip() for k in (0x010F, 0x0110)).strip()   # Make, Model
            ifd = ex.get_ifd(0x8769)                                                                # the Exif sub-IFD
            dt = ifd.get(0x9003) or ifd.get(0x9004) or ex.get(0x0132)                              # DateTimeOriginal, DateTimeDigitized, DateTime
            off = ifd.get(0x9011) or ifd.get(0x9010)                                                # OffsetTimeOriginal, OffsetTime
            gps = ex.get_ifd(0x8825)
            if gps and gps.get(2) and gps.get(4):
                out["lat"] = _dms(gps[2], str(gps.get(1) or "N"))
                out["lon"] = _dms(gps[4], str(gps.get(3) or "E"))
                if not (-90 <= out["lat"] <= 90 and -180 <= out["lon"] <= 180) or (out["lat"] == 0 and out["lon"] == 0):
                    out["lat"] = out["lon"] = None
    except Exception as e:                      # noqa: BLE001  (a picture without readable EXIF is a picture without a time)
        log.debug("no EXIF in %s: %s", path.name, e)
    m = re.match(r"^(\d{4}):(\d{2}):(\d{2})[ T](\d{2}):(\d{2}):(\d{2})", str(dt or ""))
    if m:
        out["taken"] = f"{m[1]}-{m[2]}-{m[3]}T{m[4]}:{m[5]}:{m[6]}"
    if off and OFFSET_RX.match(str(off).strip()):
        out["offset"] = str(off).strip()
    return out


def _dms(v, ref: str) -> float:
    d = float(v[0]) + float(v[1]) / 60 + float(v[2]) / 3600
    return -d if ref.upper().startswith(("S", "W")) else d


def taken_utc(ex: dict, clock: str = "exif") -> datetime | None:
    """The moment the photograph was taken, in UTC. ``clock`` says what the
    camera's clock was set to: ``exif`` trusts the zone the camera wrote and
    falls back to the ship's zone; ``ship`` and ``utc`` override it; so does
    an offset such as ``+02:00``."""
    if not ex.get("taken"):
        return None
    naive = datetime.fromisoformat(ex["taken"])
    off = ex.get("offset") if clock == "exif" else (clock if OFFSET_RX.match(clock or "") else None)
    if off:
        sign = 1 if off[0] == "+" else -1
        tz = timezone(sign * timedelta(hours=int(off[1:3]), minutes=int(off[4:6])))
    elif clock == "utc":
        tz = timezone.utc
    else:
        tz = ZoneInfo(LOCAL_TZ)
    return naive.replace(tzinfo=tz).astimezone(timezone.utc)


class Track:
    """The ship's fixes over a stretch of time, and the position at a moment:
    interpolated between the fixes either side when both are within NEAR_S,
    the nearer one alone when only it is, nothing when neither."""

    def __init__(self, rows: list[tuple[float, float, float]]):
        self.rows = sorted(rows)

    def at(self, when: datetime) -> tuple[float, float] | None:
        t = when.timestamp()
        rows = self.rows
        if not rows:
            return None
        import bisect
        i = bisect.bisect_left(rows, (t, -1e9, -1e9))
        before = rows[i - 1] if i > 0 else None
        after = rows[i] if i < len(rows) else None
        ok_b = before and t - before[0] <= NEAR_S
        ok_a = after and after[0] - t <= NEAR_S
        if ok_b and ok_a:
            if after[0] == before[0]:
                return round(before[1], 5), round(before[2], 5)
            f = (t - before[0]) / (after[0] - before[0])
            return round(before[1] + f * (after[1] - before[1]), 5), round(before[2] + f * (after[2] - before[2]), 5)
        if ok_b:
            return round(before[1], 5), round(before[2], 5)
        if ok_a:
            return round(after[1], 5), round(after[2], 5)
        return None


def leg_fixes(t0: float, t1: float) -> list[tuple[float, float, float]]:
    """(t, lat, lon) from every leg database whose record reaches into the
    stretch, read only, the first position pair with a plausible fix."""
    from .derive import resolve_position
    out: list[tuple[float, float, float]] = []
    for db in sorted(DB_DIR.glob("*.db")):
        if not LEG_DB.match(db.name):
            continue
        try:
            c = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
        except sqlite3.Error:
            continue
        try:
            cmap = dict(c.execute("SELECT key, col FROM columns"))
            pairs = resolve_position(list(cmap))
            if not pairs:
                continue
            cols = [cmap[k] for pair in pairs for k in pair]
            for row in c.execute(f"SELECT t, {', '.join(cols)} FROM obs WHERE t BETWEEN ? AND ? ORDER BY t", (int(t0), int(t1))):
                for j in range(0, len(cols), 2):
                    lat, lon = row[1 + j], row[2 + j]
                    if lat is not None and lon is not None and abs(lat) <= 90 and abs(lon) <= 180 and not (lat == 0 and lon == 0):
                        out.append((float(row[0]), float(lat), float(lon)))
                        break
        except sqlite3.Error as e:
            log.info("leg database %s unreadable: %s", db.name, e)
        finally:
            c.close()
    return out


TRACK_SOURCE = leg_fixes                        # swapped in tests


def track_for(times: list[datetime]) -> Track:
    if not times:
        return Track([])
    t0 = min(times).timestamp() - NEAR_S
    t1 = max(times).timestamp() + NEAR_S
    return Track(TRACK_SOURCE(t0, t1))


# ---------------------------------------------------------------- the sheets
def _tile(path: Path, side: int) -> Image.Image:
    with Image.open(path) as im:
        im.draft("RGB", (side * 2, side * 2))
        im = ImageOps.exif_transpose(im).convert("RGB")
        im.thumbnail((side, side))
        return im.copy()


def make_sheet(paths: list[Path], side: int = TILE) -> str:
    """A contact sheet: the photographs in reading order, numbered 1.. in a
    box at each tile's corner, three to a row; a JPEG data URL for the model."""
    n = len(paths)
    cols = 1 if n == 1 else 2 if n <= 4 else 3
    rows = -(-n // cols)
    sheet = Image.new("RGB", (cols * side, rows * side), (24, 24, 24))
    draw = ImageDraw.Draw(sheet)
    try:
        font = ImageFont.load_default(size=int(side * 0.12))
    except TypeError:                           # an older Pillow: the one size it has
        font = ImageFont.load_default()
    for i, p in enumerate(paths):
        x0, y0 = (i % cols) * side, (i // cols) * side
        try:
            tile = _tile(p, side)
            sheet.paste(tile, (x0 + (side - tile.width) // 2, y0 + (side - tile.height) // 2))
        except Exception as e:                  # noqa: BLE001
            log.info("tile %s unreadable: %s", p.name, e)
            draw.text((x0 + 20, y0 + side // 2), "unreadable", fill=(200, 200, 200), font=font)
        label = str(i + 1)
        box = draw.textbbox((0, 0), label, font=font)
        w, h = box[2] - box[0] + 16, box[3] - box[1] + 12
        draw.rectangle((x0 + 6, y0 + 6, x0 + 6 + w, y0 + 6 + h), fill=(255, 208, 0))
        draw.text((x0 + 14 - box[0], y0 + 12 - box[1]), label, fill=(0, 0, 0), font=font)
    buf = io.BytesIO()
    sheet.save(buf, "JPEG", quality=85)
    return "data:image/jpeg;base64," + base64.b64encode(buf.getvalue()).decode("ascii")


PROMPT = ("This is a contact sheet of {n} numbered photographs taken by scientists and crew aboard the CCGS Amundsen, a Canadian "
          "Coast Guard icebreaker on a research cruise in the Canadian Arctic Archipelago, or ashore near it. The number of each "
          "photograph is in the yellow box at its top-left corner. For each number, in order, give: caption, one plain sentence "
          "saying what the photograph shows; tags, three to six short words or phrases; subject, the main natural subject if "
          "there is one, as a common name (polar bear, muskox, glaucous gull, narwhal, sea ice, iceberg, glacier, fog, aurora, "
          "sunset, sedimentary rock) or else the main thing shown (the ship, people, a box core, a CTD rosette, a helicopter, an "
          "instrument, a settlement); and kind, one word from wildlife, ice, landscape, sky, weather, ship, people, work, other. "
          "Say only what is visible; do not guess species you cannot see clearly, and say 'bird' or 'seal' rather than a species "
          "when unsure. Answer with a JSON array of {n} objects, each with the keys n, caption, tags, subject, kind, and nothing "
          "else: no prose before or after the JSON.")


def tag_sheet(data_url: str, n: int) -> list[dict]:
    """One sheet through the local vision model; a dict per tile, by number.
    The model is the chat's (chatbot.model_status): the shared server the
    camera pipeline runs, or a resident Ollama model, never one loaded here."""
    import requests
    from . import chatbot
    status = chatbot.model_status()
    if not status["online"]:
        raise chatbot.ModelOffline(status["why"])
    text = PROMPT.format(n=n)
    url, model = status["url"], status["model"]
    if status["backend"] == "openai":
        body = {"model": model, "stream": False, "temperature": 0, "max_tokens": 2400, "chat_template_kwargs": {"enable_thinking": False},
                "messages": [{"role": "user", "content": [{"type": "text", "text": text}, {"type": "image_url", "image_url": {"url": data_url}}]}]}
        r = requests.post(url + "/v1/chat/completions", json=body, timeout=600)
        r.raise_for_status()
        content = r.json()["choices"][0]["message"].get("content") or ""
    else:
        body = {"model": model, "stream": False, "keep_alive": -1, "options": {"temperature": 0, "num_predict": 2400},
                "messages": [{"role": "user", "content": text, "images": [data_url.split(",", 1)[1]]}]}
        r = requests.post(url + "/api/chat", json=body, timeout=600)
        r.raise_for_status()
        content = (r.json().get("message") or {}).get("content") or ""
    return parse_tags(content, n)


TAGGER = tag_sheet                              # swapped in tests


def parse_tags(content: str, n: int) -> list[dict]:
    """The model's JSON, however it wrapped it, as n dicts in tile order: an
    unparseable answer is n empty dicts, so the photographs still import."""
    text = str(content or "")
    text = re.sub(r"^\s*```(?:json)?\s*|\s*```\s*$", "", text.strip())
    a, b = text.find("["), text.rfind("]")
    rows: list = []
    if a >= 0 and b > a:
        try:
            rows = json.loads(text[a:b + 1])
        except ValueError:
            rows = []
    by_n: dict[int, dict] = {}
    for k, r in enumerate(rows if isinstance(rows, list) else []):
        if not isinstance(r, dict):
            continue
        try:
            i = int(r.get("n", k + 1))
        except (TypeError, ValueError):
            i = k + 1
        tags = r.get("tags") or []
        if isinstance(tags, str):
            tags = [t.strip() for t in re.split(r"[,;]", tags) if t.strip()]
        by_n.setdefault(i, {"caption": str(r.get("caption") or "").strip()[:400],
                            "tags": [str(t).strip()[:40] for t in tags if str(t).strip()][:8],
                            "subject": str(r.get("subject") or "").strip()[:80],
                            "kind": str(r.get("kind") or "").strip().lower()[:20]})
    return [by_n.get(i, {}) for i in range(1, n + 1)]


# ---------------------------------------------------------------- the subjects
def subject_names(root: Path) -> dict[str, str]:
    """Every published name, lower-cased, to the subject's own name."""
    f = root / "data" / "history" / "subjects.json"
    out: dict[str, str] = {}
    try:
        subjects = json.loads(f.read_text(encoding="utf-8")).get("subjects", []) if f.is_file() else []
    except (OSError, ValueError):
        subjects = []
    for s in subjects:
        names = [s.get(k) for k in ("name", "english", "french", "inuktitut", "kalaallisut")] + [x.strip() for x in str(s.get("also") or "").split(";")]
        for n in names:
            if n and len(n) >= 3:
                out.setdefault(n.lower().strip(), s["name"])
    return out


def match_subject(guess: str, tags: list[str], names: dict[str, str]) -> str | None:
    """The published subject the model's words name, if any: the subject
    first, then each tag; a plural is tried in the singular."""
    for cand in [guess, *tags]:
        c = str(cand or "").lower().strip().strip(".")
        if not c:
            continue
        for form in (c, re.sub(r"^(a|an|the)\s+", "", c), c[:-1] if c.endswith("s") else c, c[:-2] if c.endswith("es") else c):
            if form in names:
                return names[form]
    return None


# ---------------------------------------------------------------- the import
def _job_path(job_id: str) -> Path:
    return JOBS_DIR / f"{job_id}.json"


def _save(job: dict) -> None:
    JOBS_DIR.mkdir(parents=True, exist_ok=True)
    tmp = _job_path(job["id"]).with_suffix(".tmp")
    tmp.write_text(json.dumps(job, ensure_ascii=False, indent=1), encoding="utf-8")
    tmp.replace(_job_path(job["id"]))


def job(job_id: str) -> dict | None:
    if not re.fullmatch(r"[0-9a-z-]{8,40}", job_id or ""):
        return None
    with _lock:
        j = _JOBS.get(job_id)
    if j:
        return j
    p = _job_path(job_id)
    try:
        return json.loads(p.read_text(encoding="utf-8")) if p.is_file() else None
    except (OSError, ValueError):
        return None


def jobs(limit: int = 20) -> list[dict]:
    """The imports so far, newest first, without their items."""
    out = []
    try:
        files = sorted(JOBS_DIR.glob("*.json"), key=lambda p: p.stat().st_mtime, reverse=True)[:limit]
    except OSError:
        files = []
    for p in files:
        try:
            j = json.loads(p.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        out.append({k: v for k, v in j.items() if k != "items"} | {"imported": sum(1 for x in j.get("items", []) if x.get("id")), "email": ""})
    return out


def public(j: dict) -> dict:
    """A job as the page sees it: the email stays with the ship."""
    return {**j, "form": {k: v for k, v in j.get("form", {}).items() if k != "email"}}


def folder_files(rel: str, settled: bool = False) -> list[str]:
    """A folder's photographs and those one folder down, in path order;
    ``settled`` leaves out any written in the last SETTLE_S seconds (a copy
    to the share may still be under way)."""
    d = _safe(str(rel))
    if not d.is_dir():
        raise ValueError("no such folder")
    now = time.time()
    def ok(x: Path) -> bool:
        if not (x.is_file() and x.suffix.lower() in IMAGE_EXT and not x.name.startswith(".")):
            return False
        if settled:
            try:
                return now - x.stat().st_mtime >= SETTLE_S
            except OSError:
                return False
        return True
    found = []
    for i, c in enumerate(sorted(d.iterdir(), key=lambda x: x.name.lower())):
        if i >= MAX_LIST:
            break
        if c.name.startswith("."):
            continue
        if ok(c):
            found.append(c)
        elif c.is_dir():
            for j, x in enumerate(sorted(c.iterdir(), key=lambda x: x.name.lower())):
                if j >= MAX_LIST:
                    break
                if ok(x):
                    found.append(x)
    return [rel_of(c) for c in found]


def _files_of(spec: dict) -> list[str]:
    """The photographs an import names: the folder's own and those one
    folder down, in path order, MAX_FILES at most. A folder, never single
    files (``files`` is refused)."""
    if spec.get("files"):
        raise ValueError("the import takes a folder, not single photographs: open the folder and import it")
    folders = spec.get("folders") or ([spec["folder"]] if spec.get("folder") else [])
    seen: dict[str, None] = {}
    for rel in folders:
        d = _safe(str(rel))
        if not d.is_dir():
            continue
        found = []
        for i, c in enumerate(sorted(d.iterdir(), key=lambda x: x.name.lower())):
            if i >= MAX_LIST:
                break
            if c.name.startswith("."):
                continue
            if c.is_file() and c.suffix.lower() in IMAGE_EXT:
                found.append(c)
            elif c.is_dir():
                for j, x in enumerate(sorted(c.iterdir(), key=lambda x: x.name.lower())):
                    if j >= MAX_LIST:
                        break
                    if x.is_file() and x.suffix.lower() in IMAGE_EXT and not x.name.startswith("."):
                        found.append(x)
        for c in found:
            seen.setdefault(rel_of(c))
    return list(seen)[:MAX_FILES]


# ---------------------------------------------------------------- the registry and the watches
def _read_json(path: Path, default):
    try:
        return json.loads(path.read_text(encoding="utf-8")) if path.is_file() else default
    except (OSError, ValueError):
        return default


def _write_json(path: Path, data) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    tmp.write_text(json.dumps(data, ensure_ascii=False, indent=1), encoding="utf-8")
    tmp.replace(path)


def imported() -> dict[str, str]:
    """Every photograph an import has dealt with, by its path on the share:
    the journal id it became, or ``skipped: <why>`` for one that could not
    be placed, so a watched folder is not read again for it every look."""
    with _reg_lock:
        d = _read_json(IMPORTED, {})
    return d if isinstance(d, dict) else {}


def _register(rel: str, id_: str) -> None:
    with _reg_lock:
        d = _read_json(IMPORTED, {})
        d[rel] = id_
        _write_json(IMPORTED, d)


def watches() -> list[dict]:
    with _reg_lock:
        rows = _read_json(WATCHES, [])
    return rows if isinstance(rows, list) else []


def watches_public() -> list[dict]:
    return [{**w, "form": {k: v for k, v in w.get("form", {}).items() if k != "email"}} for w in watches()]


def watch_add(path: str, form: dict, who: str = "") -> dict:
    """Watch a folder: the permission given on the form, kept with the form
    itself, so later photographs go in under the same name and licence. A
    folder watched again takes the newer form."""
    rel = rel_of(_safe(path))
    row = {"path": rel, "form": dict(form), "who": who[:60], "since": datetime.now(timezone.utc).isoformat(timespec="seconds"),
           "checked": None, "last_job": None, "imported": 0}
    with _reg_lock:
        rows = [w for w in _read_json(WATCHES, []) if isinstance(w, dict) and w.get("path") != rel]
        rows.append(row)
        _write_json(WATCHES, rows)
    return row


def watch_remove(path: str) -> bool:
    with _reg_lock:
        rows = _read_json(WATCHES, [])
        keep = [w for w in rows if isinstance(w, dict) and w.get("path") != path]
        _write_json(WATCHES, keep)
    return len(keep) != len(rows)


def _watch_update(path: str, **fields) -> None:
    with _reg_lock:
        rows = _read_json(WATCHES, [])
        for w in rows:
            if isinstance(w, dict) and w.get("path") == path:
                w.update(fields)
        _write_json(WATCHES, rows)


def watch_scan(root: Path) -> dict | None:
    """One look at the watched folders: the first with photographs the
    journal has not taken, and settled, starts an import under its form;
    the job as it stands, or None when there was nothing to do."""
    known = imported()
    now = datetime.now(timezone.utc).isoformat(timespec="seconds")
    for w in watches():
        path = w.get("path", "")
        try:
            new = [f for f in folder_files(path, settled=True) if f not in known]
        except ValueError as e:
            log.info("watched folder %s: %s", path, e)
            _watch_update(path, checked=now, error=str(e))
            continue
        _watch_update(path, checked=now, error="")
        if not new:
            continue
        try:
            j = start(root, {"folder": path, **{k: v for k, v in w.get("form", {}).items() if k in ("name", "org", "email", "licence", "clock")}, "from_watch": True, "only": new},
                      who=f"{w.get('form', {}).get('name', '')} (watched folder)")
        except ValueError as e:                     # an import is running, or the form has gone bad
            log.info("watched folder %s not imported now: %s", path, e)
            return None
        _watch_update(path, last_job=j["id"])
        return j
    return None


def start_watcher(root: Path) -> None:
    """The watcher's thread: the watched folders looked at every WATCH_S."""
    def loop():
        time.sleep(120)
        while True:
            try:
                if watches():
                    watch_scan(root)
            except Exception as e:                  # noqa: BLE001
                log.warning("photo watcher: %s", e)
            time.sleep(WATCH_S)
    threading.Thread(target=loop, daemon=True, name="photo-watcher").start()


def start(root: Path, spec: dict, who: str = "") -> dict:
    """Begin an import in a thread and answer with the job as it stands.
    ``spec``: folder (a path under the share), name, org, email, licence,
    clock, and watch (the permission to keep importing from the folder).
    Refused (ValueError) when no folder is named, the folder holds nothing
    the journal has not taken, or the form is short of a name."""
    name = str(spec.get("name") or "").strip()[:80]
    if not name:
        raise ValueError("a name is needed: whose photographs these are, as they want to be credited")
    licence = str(spec.get("licence") or "attribution").strip()
    if licence not in LICENCES:
        raise ValueError("the licence is one of " + ", ".join(LICENCES))
    clock = str(spec.get("clock") or "exif").strip()
    if clock not in CLOCKS and not OFFSET_RX.match(clock):
        raise ValueError("the camera's clock is exif, ship, utc or an offset such as +02:00")
    files = _files_of(spec)
    if not files:
        raise ValueError("no photographs in that folder")
    known = imported()
    fresh = [f for f in files if f not in known]
    if spec.get("from_watch") and isinstance(spec.get("only"), list):
        fresh = [f for f in fresh if f in set(spec["only"])]         # the watcher's pick: what has settled since its last look
    if not fresh:
        raise ValueError(f"every photograph in that folder is in the journal already ({len(files)} of them)")
    with _lock:
        if any(j["status"] in ("queued", "running") for j in _JOBS.values()):
            raise ValueError("an import is already running; wait for it to finish")
        job_id = datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S") + "-" + hashlib.sha1(f"{name}{time.time()}".encode()).hexdigest()[:6]
        j = {"id": job_id, "status": "queued", "started": datetime.now(timezone.utc).isoformat(timespec="seconds"), "finished": None,
             "form": {"name": name, "org": str(spec.get("org") or "").strip()[:120], "email": str(spec.get("email") or "").strip()[:120],
                      "licence": licence, "clock": clock},
             "who": who[:60], "total": len(fresh), "done": 0, "stage": "queued", "error": "", "known": len(files) - len(fresh),
             "folder": (spec.get("folders") or [spec.get("folder")])[0], "watch": bool(spec.get("watch")), "from_watch": bool(spec.get("from_watch")),
             "items": [{"file": f, "status": "queued"} for f in fresh]}
        _JOBS[job_id] = j
    _save(j)
    if j["watch"]:
        watch_add(j["folder"], j["form"], who)
    threading.Thread(target=run, args=(j, root), daemon=True).start()
    return public(j)


def run(j: dict, root: Path) -> None:
    """The import itself: when and where for each photograph, the sheets
    through the model, then a journal line each. Runs to the end whatever
    any one photograph does; the job says what became of each."""
    from . import nature
    form = j["form"]
    try:
        j["status"], j["stage"] = "running", "reading the photographs"
        _save(j)
        whens: dict[int, datetime] = {}                 # item index -> the moment, kept off the record (it is not JSON)
        paths: dict[int, Path] = {}
        for idx, it in enumerate(j["items"]):
            try:
                paths[idx] = _safe(it["file"])
            except ValueError as e:
                it["status"], it["error"] = "skipped", str(e)
        exif = exif_many(list(paths.values()))
        for idx, it in enumerate(j["items"]):
            if idx not in paths:
                continue
            try:
                p = paths[idx]
                ex = exif[p]
                it["camera"] = ex["model"]
                when = taken_utc(ex, form["clock"])
                if not when:
                    it["status"], it["error"] = "skipped", "no time in the photograph's EXIF"
                    continue
                it["date"] = when.strftime("%Y-%m-%dT%H:%MZ")
                whens[idx] = when
                if ex["lat"] is not None:
                    it["lat"], it["lon"], it["position"] = round(ex["lat"], 5), round(ex["lon"], 5), "the camera's GPS"
            except Exception as e:                  # noqa: BLE001
                it["status"], it["error"] = "skipped", f"unreadable: {e}"
        j["stage"] = "placing them on the ship's track"
        _save(j)
        need = [(idx, it) for idx, it in enumerate(j["items"]) if idx in whens and it.get("lat") is None]
        if need:
            track = track_for([whens[idx] for idx, _ in need])
            for idx, it in need:
                pos = track.at(whens[idx])
                if pos:
                    it["lat"], it["lon"], it["position"] = pos[0], pos[1], "the ship's track at that moment"
                else:
                    it["status"], it["error"] = "skipped", "no position: no GPS in the photograph and no fix of the ship's within 15 minutes of it"
        todo = [it for it in j["items"] if it.get("lat") is not None]
        j["stage"] = f"reading {len(todo)} photographs with the model"
        _save(j)
        for k in range(0, len(todo), SHEET_N):
            batch = todo[k:k + SHEET_N]
            try:
                tags = TAGGER(make_sheet([_safe(it["file"]) for it in batch]), len(batch))
            except Exception as e:                  # noqa: BLE001
                log.warning("photo import: the model did not answer for a sheet: %s", e)
                tags = [{} for _ in batch]
                j["error"] = f"the model did not answer for every sheet ({e}); those photographs carry no caption"
            for it, t in zip(batch, tags):
                it.update({k2: t.get(k2) for k2 in ("caption", "tags", "subject", "kind") if t.get(k2)})
            j["done"] = min(len(todo), k + len(batch))
            j["stage"] = f"read {j['done']} of {len(todo)} with the model"
            _save(j)
        j["stage"] = "writing the journal"
        names = subject_names(root)
        for it in todo:
            try:
                it["subject_page"] = match_subject(it.get("subject", ""), it.get("tags") or [], names)
                subject = it["subject_page"] or it.get("subject") or "photograph"
                p = _safe(it["file"])
                credit = form["name"] + (f" ({form['org']})" if form.get("org") else "")
                detail = " ".join(x for x in [it.get("caption", ""), f"Photograph by {credit}, {p.name}" + (f", {it['camera']}" if it.get("camera") else "") + ".",
                                              ("Tags: " + ", ".join(it["tags"]) + ".") if it.get("tags") else ""] if x)
                entry = {"kind": "observation", "subject": subject, "date": it["date"], "lat": it["lat"], "lon": it["lon"], "method": "camera",
                         "observer": form["name"], "vessel": "CCGS Amundsen", "detail": detail[:2000], "licence": form["licence"], "origin": "ship",
                         "image": journal_jpeg(p)}
                row = nature.append(entry, form["name"])
                it["id"], it["artifact_file"], it["status"] = row["id"], row.get("artifact_file", ""), "imported"
                _register(it["file"], row["id"])
            except Exception as e:                  # noqa: BLE001
                it["status"], it["error"] = "failed", str(e)[:300]
        for it in j["items"]:
            if it["status"] == "skipped":
                _register(it["file"], "skipped: " + it.get("error", ""))
        j["status"], j["stage"] = "done", "done"
    except Exception as e:                          # noqa: BLE001
        log.warning("photo import %s failed: %s", j["id"], e)
        j["status"], j["stage"], j["error"] = "failed", "failed", str(e)[:300]
    finally:
        j["finished"] = datetime.now(timezone.utc).isoformat(timespec="seconds")
        j["done"] = sum(1 for it in j["items"] if it.get("id"))
        _save(j)
        if j.get("folder") and any(w.get("path") == j["folder"] for w in watches()):
            _watch_update(j["folder"], imported=sum(1 for f, v in imported().items() if f.startswith(j["folder"] + "/") and not v.startswith("skipped")), last_job=j["id"])


def journal_jpeg(path: Path) -> str:
    """The journal's copy of a photograph: upright, at most PHOTO_MAX on the
    long side, as the data URL the journal's writer takes."""
    with Image.open(path) as im:
        im = ImageOps.exif_transpose(im).convert("RGB")
        im.thumbnail((PHOTO_MAX, PHOTO_MAX))
        buf = io.BytesIO()
        im.save(buf, "JPEG", quality=88)
    return "data:image/jpeg;base64," + base64.b64encode(buf.getvalue()).decode("ascii")
