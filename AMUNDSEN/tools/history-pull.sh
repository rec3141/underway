#!/bin/bash
# Pull the history layer from grid, where the research crew maintains it.
# The ship is a reader: the code comes by git, the two databases as consistent
# snapshots, the files by rsync. Nothing here ever writes back.
#
#   tools/history-pull.sh          pull code, databases and files
#   tools/history-pull.sh status   row counts here and on grid
#
# The databases are the English research record and the reviewed translation
# release that the wiki publisher overlays on it. They travel together so the
# publisher's source-hash recheck sees a matching pair.
#
# Where the clone is and where it comes from are the installation's settings
# (ARCTIC_HISTORY_ROOT, ARCTIC_HISTORY_REMOTE as host:path, UNDERWAY_PYTHON in
# /etc/underway/site.env); the timer's job gives this script no other
# environment.
set -euo pipefail
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
if [[ -r $SITE ]]; then set -a; . "$SITE"; set +a; fi
LOCAL=${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}
ORIGIN=${ARCTIC_HISTORY_REMOTE:-grid:/data/dev/arctic-history}
GRID=${ORIGIN%%:*}
REMOTE=${ORIGIN#*:}
PY=${PY:-${UNDERWAY_PYTHON:-python3}}

# what the installed release offers the publisher, per locale. No release, or
# one this build cannot read, is not an error: the wiki is published in
# English and the publisher says so in its own log
locales() {
  [[ -f $LOCAL/db/history/translations.sqlite ]] || { echo "translations  none (English only)"; return 0; }
  "$PY" - "$LOCAL/db/history/translations.sqlite" <<'PYEOF' || echo "translations  unreadable (English only)"
import sqlite3, sys
db = sqlite3.connect('file:%s?mode=ro' % sys.argv[1], uri=True)
default = dict(db.execute('SELECT target_locale,profile_id FROM locale_defaults'))
rows = db.execute("""SELECT target_locale,COUNT(*) FROM candidates
    WHERE review_status='approved' AND validation_status='valid' GROUP BY 1""").fetchall()
for locale, n in rows or [('none', 0)]:
    print('translations  %-6s %6d approved  %s' % (locale, n, default.get(locale, 'no default profile')))
PYEOF
}

case "${1:-pull}" in
  pull)
    echo "== code"
    if ! git -C "$LOCAL" pull -q --ff-only origin main; then
      echo "the code did not pull; the database and files are left as they are so they match the code" >&2
      exit 1
    fi
    git -C "$LOCAL" log --oneline -1
    echo "== database snapshot"
    ssh "$GRID" "cd $REMOTE && python3 - <<'PYEOF'
import sqlite3
from pathlib import Path
src = sqlite3.connect('db/history/history.sqlite'); dst = sqlite3.connect('db/history/history.snapshot.sqlite')
src.backup(dst); dst.close(); src.close()
# the reviewed translation release, when the crew has published one. The
# working sidecar it is released from stays on grid and never comes here
if Path('db/history/translations.sqlite').is_file():
    src = sqlite3.connect('file:db/history/translations.sqlite?mode=ro', uri=True)
    dst = sqlite3.connect('db/history/translations.snapshot.sqlite')
    src.backup(dst); dst.close(); src.close()
    print('reviewed translations')
PYEOF"
    rsync -a "$GRID:$REMOTE/db/history/history.snapshot.sqlite" "$LOCAL/db/history/history.sqlite.new"
    # a release is a database, not optional media, so it comes whenever grid
    # has one. No release on grid leaves the ship's installed release alone;
    # an empty release is how the crew withdraws every locale
    if ssh "$GRID" "test -f $REMOTE/db/history/translations.sqlite"; then
      rsync -a "$GRID:$REMOTE/db/history/translations.snapshot.sqlite" "$LOCAL/db/history/translations.sqlite.new"
    fi
    # the swap waits for a running build to finish (the build holds this lock),
    # so no build reads a database while it changes underneath. Both move
    # inside one lock: the publisher rechecks every translation's source hash
    # against the research database, and a half-applied pair would drop the
    # mismatched fields back to English until the next pull
    HERE=$(cd "$(dirname "$0")/.." && pwd)
    mkdir -p "$HERE/cache"
    (
      flock -w 900 9 || { echo "a build has held the lock for 15 minutes; not swapping the databases" >&2; exit 1; }
      rm -f "$LOCAL/db/history/history.sqlite-wal" "$LOCAL/db/history/history.sqlite-shm"
      mv "$LOCAL/db/history/history.sqlite.new" "$LOCAL/db/history/history.sqlite"
      if [[ -f $LOCAL/db/history/translations.sqlite.new ]]; then
        rm -f "$LOCAL/db/history/translations.sqlite-wal" "$LOCAL/db/history/translations.sqlite-shm"
        mv "$LOCAL/db/history/translations.sqlite.new" "$LOCAL/db/history/translations.sqlite"
      fi
    ) 9>"$HERE/cache/.run.lock"
    echo "== files"
    # only the fetched files: everything else under db/history is git's, and
    # the clone must stay a clean checkout of grid's repository. The crew is
    # fetching while this runs: a download in progress (*.part) is skipped,
    # and a file that vanishes between the listing and the copy (rsync 24)
    # is a warning, not a failure; it comes with the next pull
    rc=0
    rsync -a --info=stats1 --exclude '*.part' --include '*/' --include '*/files/**' --exclude '*' \
      "$GRID:$REMOTE/db/history/" "$LOCAL/db/history/" 2>&1 | { grep -E "Number of (regular files transferred|created)|Total transferred|vanished|error" || true; } || rc=$?
    if [ "$rc" -ne 0 ] && [ "$rc" -ne 24 ]; then echo "the files did not sync (rsync exit $rc)" >&2; exit "$rc"; fi
    find "$LOCAL/db/history" -mindepth 1 -type d -empty -delete 2>/dev/null || true
    "$PY" "$LOCAL/tools/history-db.py" stats | head -4
    locales
    echo "the next dashboard build publishes it"
    ;;
  status)
    echo "--- ship"; "$PY" "$LOCAL/tools/history-db.py" stats; locales
    echo "--- grid"; ssh "$GRID" "cd $REMOTE && python3 tools/history-db.py stats"
    ;;
  *) echo "usage: history-pull.sh [pull|status]"; exit 1 ;;
esac
