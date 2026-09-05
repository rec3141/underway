#!/usr/bin/env bash
# Uses the same lock for generation and copying, so daily copies are consistent.
set -euo pipefail
: "${UNDERWAY_CAMERA_OUTPUT:?Set a dedicated local camera product directory}"
mkdir -p "$UNDERWAY_CAMERA_OUTPUT"
exec 9>"$UNDERWAY_CAMERA_OUTPUT/.job.lock"
flock -n 9 || exit 0
case "${1:-build}" in
  build)
    : "${UNDERWAY_CAMERA_SOURCE:?Set the camera leg directory}"
    "${UNDERWAY_CAMERA_PYTHON:-python3}" -m dashboard.cameras \
      --source "$UNDERWAY_CAMERA_SOURCE" --output "$UNDERWAY_CAMERA_OUTPUT" \
      --hours "${UNDERWAY_CAMERA_HOURS:-24}" --interval "${UNDERWAY_CAMERA_INTERVAL:-120}"
    ;;
  sync)
    : "${UNDERWAY_CAMERA_SHARE:?Set the existing destination photos directory}"
    [[ -d "$UNDERWAY_CAMERA_SHARE" ]] || { echo 'Destination unavailable; not creating a mount placeholder' >&2; exit 1; }
    [[ -s "$UNDERWAY_CAMERA_OUTPUT/latest.mp4" && -s "$UNDERWAY_CAMERA_OUTPUT/latest.json" ]] || exit 1
    "${UNDERWAY_CAMERA_PYTHON:-python3}" -c 'import json,sys; from datetime import datetime,timezone; m=json.load(open(sys.argv[1])); age=(datetime.now(timezone.utc)-datetime.fromisoformat(m["end_utc"])).total_seconds(); sys.exit(0 if 0 <= age <= 26*3600 else "Timelapse stale; not copying as a new daily product")' "$UNDERWAY_CAMERA_OUTPUT/latest.json"
    # Dedicated filenames; no --delete and no original images copied.
    day=$(date -u +%Y%m%d)
    timeout 600 rsync -rt --whole-file --timeout=60 -- \
      "$UNDERWAY_CAMERA_OUTPUT/latest.mp4" "$UNDERWAY_CAMERA_SHARE/underway-camera-$day.mp4"
    timeout 60 rsync -rt --whole-file --timeout=60 -- \
      "$UNDERWAY_CAMERA_OUTPUT/latest.json" "$UNDERWAY_CAMERA_SHARE/underway-camera-$day.json"
    ;;
  *) echo 'Usage: camera-job.sh build|sync' >&2; exit 2 ;;
esac
