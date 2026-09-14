"""The History tab's data: the arctic-history project, a sibling repository.

The research crew writes the database on grid; the ship pulls a snapshot and
the fetched files with that project's ``tools/history-sync.sh pull``. This
module only finds the package and hands its names on, so the build calls
``history.publish(root)`` and the server reads requests and pages as before.

The project is looked for at ``$ARCTIC_HISTORY_ROOT`` (set in the installation's
``/etc/underway/site.env``), then ``/data/dev/arctic-history``, where grid keeps it
too. Without it the History tab is simply absent:
``publish`` returns None and the page hides the tab's map layer.
"""

from __future__ import annotations

import logging
import os
import sys
from pathlib import Path

log = logging.getLogger(__name__)

ROOT = Path(os.environ.get("ARCTIC_HISTORY_ROOT", "/data/dev/arctic-history")).expanduser()
if ROOT.is_dir() and str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

try:
    from arctic_history import *              # noqa: F401,F403
    from arctic_history import HISTORY_DB, HISTORY_DIR, answer_request, connect, list_requests   # noqa: F401
    from arctic_history import publish as _publish
    AVAILABLE = True

    def publish(root: Path, db: Path | None = None):
        """Publish the layer; if the database cannot be read this minute (a
        pull is replacing it, say), keep the last good publish rather than
        blank the tab for a build."""
        import json
        last = root / "data" / "history" / "manifest-entry.json"
        try:
            entry = _publish(root, db)
        except Exception as e:                  # noqa: BLE001
            if last.is_file():
                log.warning("history publish failed (%s); keeping the last good publish", e)
                return json.loads(last.read_text())
            log.warning("history publish failed (%s); no earlier publish to keep", e)
            return None
        if entry:
            thumbnails(root)
            provenance(root, db)
            last.parent.mkdir(parents=True, exist_ok=True)
            last.write_text(json.dumps(entry))
        return entry

    def provenance(root: Path, db: Path | None = None) -> dict | None:
        """The provenance page's material (``provenance.json``): the project's
        own account, ``db/history/PROVENANCE.md``, as Markdown, and the
        figures counted from the database now: what is in it, how the crew's
        runs and the review passes went, what the person answered, and what
        the local models annotated. Counted here rather than published by
        the project so the figures are the build's, not a snapshot's."""
        import json
        import sqlite3
        from datetime import datetime, timezone
        path = Path(db) if db else HISTORY_DB
        if not Path(path).is_file():
            return None
        c = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
        def one(sql, default=0):
            try:
                return c.execute(sql).fetchone()[0] or default
            except sqlite3.Error:
                return default
        def rows(sql):
            try:
                return c.execute(sql).fetchall()
            except sqlite3.Error:
                return []
        try:
            out = {
                "generated": datetime.now(timezone.utc).isoformat(timespec="seconds"),
                "counts": {
                    "topics": one("SELECT count(*) FROM topics"),
                    "pages": one("SELECT count(*) FROM pages WHERE kind = 'page'"),
                    "pages_draft": one("SELECT count(*) FROM pages WHERE kind = 'page' AND status = 'draft'"),
                    "page_versions": one("SELECT count(*) FROM page_history"),
                    "artifacts": one("SELECT count(*) FROM artifacts"),
                    "artifacts_by_type": dict(rows("SELECT type, count(*) FROM artifacts GROUP BY type ORDER BY 2 DESC")),
                    "licences": dict(rows("SELECT licence, count(*) FROM artifacts WHERE licence != '' GROUP BY licence ORDER BY 2 DESC")),
                    "waypoints": sum(len(json.loads(w or "[]") or []) for (w,) in rows("SELECT waypoints FROM artifacts WHERE type = 'track'") if w and w.startswith("[")),
                    "artifacts_local": one("SELECT count(*) FROM artifacts WHERE local_file != ''"),
                    "artifacts_linked": one("SELECT count(*) FROM artifacts WHERE source_url != ''"),
                    "sources": one("SELECT count(*) FROM sources"),
                    "sources_primary": one("SELECT count(*) FROM sources WHERE primary_src = 1"),
                    "sources_local": one("SELECT count(*) FROM sources WHERE local_file != ''"),
                    "languages": one("SELECT count(DISTINCT language) FROM sources WHERE language != ''"),
                    "people": one("SELECT count(*) FROM people"),
                    "people_indigenous": one("SELECT count(*) FROM people WHERE indigenous = 1"),
                    "places": one("SELECT count(*) FROM places"),
                    "places_inuktitut": one("SELECT count(*) FROM places WHERE inuktitut != ''"),
                    "events": one("SELECT count(*) FROM events"),
                    "dates": one("SELECT count(*) FROM dates"),
                    "vessels": one("SELECT count(*) FROM vessels"),
                    "animals": one("SELECT count(*) FROM animals"),
                    "links": one("SELECT count(*) FROM links WHERE target != ''"),
                    "links_wanted": one("SELECT count(*) FROM links WHERE target = ''"),
                },
                "work": {
                    "worklog": one("SELECT count(*) FROM worklog"),
                    "runs": one("SELECT count(DISTINCT who) FROM worklog WHERE who != ''"),
                    "review_entries": one("SELECT count(*) FROM worklog WHERE who LIKE 'review%' OR who LIKE '%qa%'"),
                    "first": one("SELECT min(at) FROM worklog", ""),
                    "last": one("SELECT max(at) FROM worklog", ""),
                    "requests": dict(rows("SELECT status, count(*) FROM requests GROUP BY status")),
                    "request_kinds": dict(rows("SELECT kind, count(*) FROM requests GROUP BY kind")),
                },
                "machine": {
                    "keywords": one("SELECT count(*) FROM keywords WHERE source LIKE 'llm:%'"),
                    "keyword_artifacts": one("SELECT count(DISTINCT artifact_id) FROM keywords WHERE source LIKE 'llm:%'"),
                    "keyword_models": sorted({r[0][4:] for r in rows("SELECT DISTINCT source FROM keywords WHERE source LIKE 'llm:%'")}),
                    "ontology_model": one("SELECT model FROM ontology", ""),
                    "faces": one("SELECT count(*) FROM faces WHERE kind = 'person'"),
                    "faces_identified": one("SELECT count(*) FROM faces WHERE kind = 'person' AND person != ''"),
                    "animals_detected": one("SELECT count(*) FROM faces WHERE kind != 'person'"),
                    "animals_identified": one("SELECT count(*) FROM faces WHERE kind != 'person' AND person != ''"),
                    "detectors": sorted({r[0].split(";")[0].split(":")[0].strip() for r in rows("SELECT DISTINCT method FROM faces") if r[0]}),
                    "rejected_by_operator": one("SELECT count(*) FROM faces WHERE method LIKE '%rejected by the operator%'"),
                },
            }
        finally:
            c.close()
        try:                                    # the codes' phrases, from the project once it has them
            from arctic_history import licence_label
            out["counts"]["licence_labels"] = {k: licence_label(k) for k in out["counts"]["licences"]}
        except ImportError:
            out["counts"]["licence_labels"] = {}
        account = Path(path).parent / "PROVENANCE.md"
        out["text"] = account.read_text() if account.is_file() else ""
        dst = root / "data" / "history" / "provenance.json"
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_text(json.dumps(out, ensure_ascii=False))
        return out

    def thumbnails(root: Path, width: int = 480) -> int:
        """A small JPEG beside every published picture, so a page of a hundred
        cards does not load a hundred full-size scans; artifacts.json gains
        a ``thumb`` on each. One is made when its picture is new or changed."""
        import json
        from PIL import Image, ImageOps
        Image.MAX_IMAGE_PIXELS = None          # our own archive's scans, some of them half a gigapixel
        out = root / "data" / "history"
        arts_file = out / "artifacts.json"
        if not arts_file.is_file():
            return 0
        data = json.loads(arts_file.read_text())
        tdir = out / "thumbs"
        tdir.mkdir(parents=True, exist_ok=True)
        made = 0
        for a in data.get("artifacts", []):
            if a.get("type") not in ("image", "map") or not a.get("url"):
                continue
            if not a["url"].lower().endswith((".jpg", ".jpeg", ".png", ".gif", ".tif", ".tiff", ".webp", ".bmp")):
                continue                        # a PDF or a page: the card links to it instead
            src = root / a["url"]
            dst = tdir / f"{a['id']}.jpg"
            if not src.is_file():
                continue
            if not dst.is_file() or dst.stat().st_mtime < src.stat().st_mtime:
                try:
                    with Image.open(src) as im:
                        # a photograph carries its orientation in EXIF, which the
                        # browser honours on the full picture: the thumbnail must
                        # be turned the same way or the card shows it on its side
                        im = ImageOps.exif_transpose(im).convert("RGB")
                        im.thumbnail((width, width * 2))
                        im.save(dst, "JPEG", quality=82, optimize=True)
                    made += 1
                except Exception as e:              # noqa: BLE001
                    log.warning("no thumbnail for %s: %s", src.name, e)
                    continue
            a["thumb"] = f"data/history/thumbs/{dst.name}"
        arts_file.write_text(json.dumps(data, ensure_ascii=False))
        return made
except ImportError:
    AVAILABLE = False
    HISTORY_DIR = ROOT / "db" / "history"
    HISTORY_DB = HISTORY_DIR / "history.sqlite"

    def publish(root: Path, db: Path | None = None):    # noqa: ARG001
        return None

    def connect(*a, **k):
        raise FileNotFoundError(f"the arctic-history project is not at {ROOT}")

    def list_requests(*a, **k):
        return []

    def answer_request(*a, **k):
        raise FileNotFoundError(f"the arctic-history project is not at {ROOT}")

    log.info("arctic-history not found at %s; the History tab stays empty", ROOT)
