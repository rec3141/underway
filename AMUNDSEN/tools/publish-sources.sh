#!/usr/bin/env bash
# Ship -> grid: site inputs, excluding CTD CNVs and generated cast HTML.
# The dedicated remote source mirror is pruned to match these filters.
# publish-sources.sh [--dry-run]
set -euo pipefail
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
if [[ -r $SITE ]]; then set -a; . "$SITE"; set +a; fi
SOURCE=${UNDERWAY_MIRROR:-/data/ship}
REMOTE=${UNDERWAY_PUBLISH_REMOTE:-grid:/data/underway_server/www}
REMOTE_SOURCE=${UNDERWAY_PUBLISH_SOURCE_REMOTE:-${REMOTE%/*}/source}
DB=${UNDERWAY_DB_DIR:-${UNDERWAY_HOME:-/data/underway_server}/db}
RS=(rsync -az --delete-delay --delete-excluded --delay-updates --partial-dir=.rsync-partial --timeout=120 --stats)
if [[ ${1:-} == --dry-run ]]; then RS+=(--dry-run --itemize-changes); elif [[ $# -gt 0 ]]; then echo 'usage: publish-sources.sh [--dry-run]' >&2; exit 2; fi
[[ $REMOTE_SOURCE == *:/*/source ]] || { echo "Remote source must be a dedicated absolute .../source directory" >&2; exit 1; }
[[ -d $SOURCE/Data/FULL_CSV && -d $SOURCE/Share ]] || { echo "No FULL_CSV source at $SOURCE" >&2; exit 1; }
# Serialize manual seeds and scheduled source pushes on the ship.
exec 8>"$DB/.source-push.lock"
flock 8
# Destination creation is the only remote write during a dry run.
printf -v remote_cmd 'mkdir -p %q %q' "${REMOTE_SOURCE#*:}" "${REMOTE_SOURCE#*:}/runtime"
ssh "${REMOTE_SOURCE%%:*}" "$remote_cmd"
echo "== Source observations: $SOURCE -> $REMOTE_SOURCE"
"${RS[@]}" --filter 'P /runtime/***' --exclude '*.tmp' --exclude '*.part' --exclude '*.html' \
  --include '/Data/' --include '/Data/FULL_CSV/***' --include '/Data/TSG/***' \
  --exclude '*.cnv' --exclude '*.CNV' \
  --include '/Data/EventLog/***' --include '/Data/Rosette/***' --include '/Data/MVP/***' \
  --include '/Share/***' --exclude '*' "$SOURCE/" "$REMOTE_SOURCE/"
echo '== Recorded live observations (no accounts, subscriptions or credentials)'
"${RS[@]}" --include '/live_scrape/***' --include '/provisional_tsg.csv' --exclude '*' \
  "$DB/" "$REMOTE_SOURCE/runtime/"
