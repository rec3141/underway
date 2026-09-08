"""The History tab's data: the arctic-history project, a sibling repository.

The research crew writes the database on grid; the ship pulls a snapshot and
the fetched files with that project's ``tools/history-sync.sh pull``. This
module only finds the package and hands its names on, so the build calls
``history.publish(root)`` and the server reads requests and pages as before.

The project is looked for at ``$ARCTIC_HISTORY_ROOT``, then
``~/Desktop/arctic-history``. Without it the History tab is simply absent:
``publish`` returns None and the page hides the tab's map layer.
"""

from __future__ import annotations

import logging
import os
import sys
from pathlib import Path

log = logging.getLogger(__name__)

ROOT = Path(os.environ.get("ARCTIC_HISTORY_ROOT", Path.home() / "Desktop" / "arctic-history")).expanduser()
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
            last.parent.mkdir(parents=True, exist_ok=True)
            last.write_text(json.dumps(entry))
        return entry
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
