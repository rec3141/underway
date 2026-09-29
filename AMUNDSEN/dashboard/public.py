"""What the public copy of the dashboard may carry.

The web copy (``tools/publish-web.sh``, on grid) releases only what Amundsen
Science has already released itself. This module rewrites grid's mirror of
the web root in place before it goes to the web server:

* the schedule's whiteboard note is the ship's internal notice board, so it
  is emptied, and a "latest change" that quotes it is dropped.

Every rewrite is atomic (a temporary file beside the target, then a rename),
so an interrupted run leaves the previous file standing.
"""

from __future__ import annotations

import json
import os
import tempfile
from pathlib import Path


def _write(path: Path, data) -> None:
    fd, tmp = tempfile.mkstemp(prefix=f".{path.name}-", suffix=".tmp", dir=path.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(data, stream, separators=(",", ":"))
        os.chmod(tmp, path.stat().st_mode & 0o777 if path.exists() else 0o644)
        os.replace(tmp, path)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def strip_whiteboard(root: Path, manifest: dict) -> bool:
    """Empty the whiteboard in ``data/calendar.json`` and drop the manifest's
    latest-change note when it is the whiteboard's. True when anything changed."""
    changed = False
    update = (manifest.get("calendar") or {}).get("update")
    if isinstance(update, dict) and update.get("kind") == "whiteboard":
        del manifest["calendar"]["update"]
        changed = True
    path = root / "data/calendar.json"
    if path.is_file():
        cal = json.loads(path.read_text())
        schedule = cal.get("schedule")
        if isinstance(schedule, dict) and schedule.get("whiteboard"):
            schedule["whiteboard"] = ""
            _write(path, cal)
            changed = True
    return changed
