#!/bin/bash
# Pull the history layer from grid, where the research crew maintains it.
# The ship is a reader: the code comes by git, the database as a consistent
# snapshot, the files by rsync. Nothing here ever writes back.
#
#   tools/history-pull.sh          pull code, database and files
#   tools/history-pull.sh status   row counts here and on grid
set -euo pipefail
LOCAL=${ARCTIC_HISTORY_ROOT:-$HOME/Desktop/arctic-history}
GRID=grid
REMOTE=/data/dev/arctic-history
PY=${PY:-/opt/miniforge3/bin/python3}

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
src = sqlite3.connect('db/history/history.sqlite'); dst = sqlite3.connect('db/history/history.snapshot.sqlite')
src.backup(dst); dst.close(); src.close()
PYEOF"
    rsync -a "$GRID:$REMOTE/db/history/history.snapshot.sqlite" "$LOCAL/db/history/history.sqlite.new"
    mv "$LOCAL/db/history/history.sqlite.new" "$LOCAL/db/history/history.sqlite"
    rm -f "$LOCAL/db/history/history.sqlite-wal" "$LOCAL/db/history/history.sqlite-shm"
    echo "== files"
    # only the fetched files: everything else under db/history is git's, and
    # the clone must stay a clean checkout of grid's repository
    rsync -a --info=stats1 --include '*/' --include '*/files/**' --exclude '*' \
      "$GRID:$REMOTE/db/history/" "$LOCAL/db/history/" | { grep -E "Number of (regular files transferred|created)|Total transferred" || true; }
    find "$LOCAL/db/history" -mindepth 1 -type d -empty -delete 2>/dev/null || true
    "$PY" "$LOCAL/tools/history-db.py" stats | head -4
    echo "the next dashboard build publishes it"
    ;;
  status)
    echo "--- ship"; "$PY" "$LOCAL/tools/history-db.py" stats
    echo "--- grid"; ssh "$GRID" "cd $REMOTE && python3 tools/history-db.py stats"
    ;;
  *) echo "usage: history-pull.sh [pull|status]"; exit 1 ;;
esac
