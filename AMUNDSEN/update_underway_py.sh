#!/usr/bin/env bash
# Regenerate the live leg of the underway dashboard, in place in the web root.
#
# Ingest is incremental and every file is written atomically, so the builder
# works directly on the published directory: no staging copy, no rsync, and a
# leg processed on request from the page is left alone.
set -euo pipefail

WEBROOT="/mnt/ship/Share/2026/2026_LEG_03/Collins/underway_dashboard"
PROJECT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"    # the AMUNDSEN directory this script lives in
PYTHON="/opt/miniforge3/bin/python3"                    # the interpreter with pandas/plotly

# one run at a time; the timer fires every 10 minutes regardless
mkdir -p "$PROJECT/cache"
exec 9>"$PROJECT/cache/.run.lock"
flock -n 9 || { echo "Another run is in progress; exiting."; exit 0; }

umask 002
export TZ=America/Toronto

cd "$PROJECT"
# every leg is ingested (incrementally) and the dashboard is built from all of them
nice -n 19 ionice -c3 "$PYTHON" -m underway build --root "$WEBROOT" \
    --title "CCGS Amundsen — Underway" \
    --link "Event log & schedule calendar|https://calendar.google.com/calendar/embed?src=7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb%40group.calendar.google.com&ctz=America%2FToronto" \
    --link "High-surprise events calendar|https://calendar.google.com/calendar/embed?src=d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d%40group.calendar.google.com&ctz=America%2FToronto" \
    "$@"

echo "[OK] $(date -u '+%F %TZ') all legs rebuilt in $WEBROOT"
