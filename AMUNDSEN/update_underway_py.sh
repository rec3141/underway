#!/usr/bin/env bash
# Regenerate the underway dashboard.
#
# Two modes, chosen by UNDERWAY_LOCAL (set in the systemd unit's Environment=):
#   unset  — read the CIFS shares directly and write into the web root on the
#            Share drive (the original arrangement)
#   1      — first mirror the shares onto local disk with tools/mirror-share.sh,
#            then build from the mirror into a local web root. Every read and
#            write is then local; the share is touched only by one rsync pass.
#
# Ingest is incremental and every file is written atomically, so the builder
# works directly on the published directory: no staging copy.
set -euo pipefail

PROJECT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"    # the AMUNDSEN directory this script lives in
PYTHON="/opt/miniforge3/bin/python3"                    # the interpreter with pandas/plotly

if [[ ${UNDERWAY_LOCAL:-0} == 1 ]]; then
    MIRROR=${UNDERWAY_MIRROR:-/data/ship}
    WEBROOT=${UNDERWAY_WEBROOT:-/data/underway/www}
    export UNDERWAY_DATA_ROOT="$MIRROR/Data" UNDERWAY_SHARE_ROOT="$MIRROR/Share"
else
    WEBROOT=${UNDERWAY_WEBROOT:-/mnt/ship/Share/2026/2026_LEG_03/Collins/underway_dashboard}
fi

# one run at a time; the timer fires every minute regardless
mkdir -p "$PROJECT/cache"
exec 9>"$PROJECT/cache/.run.lock"
flock -n 9 || { echo "Another run is in progress; exiting."; exit 0; }

umask 002
export TZ=America/Toronto

if [[ ${UNDERWAY_LOCAL:-0} == 1 ]]; then
    # a failed mirror pass is not fatal: the build proceeds on what is mirrored.
    # The underway files are mirrored every run; the cast, MVP and archive
    # trees (a CIFS stat per file) every ten minutes, or when asked for.
    mode=quick
    [[ ${UNDERWAY_MIRROR_FULL:-0} == 1 || $(( 10#$(date +%M) % 10 )) -eq 0 ]] && mode=full
    "$PROJECT/tools/mirror-share.sh" "$MIRROR" "$mode" || echo "mirror pass had errors; building from the existing mirror"
fi

cd "$PROJECT"
nice -n 19 ionice -c3 "$PYTHON" -m dashboard build --root "$WEBROOT" \
    --title "CCGS Amundsen — Underway" \
    --link "Amundsen Schedule (event log & operations)|https://calendar.google.com/calendar/embed?src=d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d%40group.calendar.google.com&ctz=America%2FToronto" \
    --link "Underway Updates (surprise episodes)|https://calendar.google.com/calendar/embed?src=7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb%40group.calendar.google.com&ctz=America%2FToronto" \
    "$@"

echo "[OK] $(date -u '+%F %TZ') all legs rebuilt in $WEBROOT"
