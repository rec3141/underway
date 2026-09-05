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
      --interval "${UNDERWAY_CAMERA_INTERVAL:-120}"
    ;;
  sync)
    : "${UNDERWAY_CAMERA_SHARE:?Set the existing destination timelapse directory}"
    [[ -d "$UNDERWAY_CAMERA_SHARE" ]] || { echo 'Destination unavailable; not creating a mount placeholder' >&2; exit 1; }
    [[ -s "$UNDERWAY_CAMERA_OUTPUT/full-leg.mp4" && -s "$UNDERWAY_CAMERA_OUTPUT/full-leg.json" ]] || exit 1
    # Dedicated subdirectory; no --delete and no original images copied.
    timeout 600 rsync -rt --whole-file --timeout=60 \
      --exclude='.*' --include='*/' --include='*.mp4' --include='*.json' --exclude='*' \
      -- "$UNDERWAY_CAMERA_OUTPUT/" "$UNDERWAY_CAMERA_SHARE/"
    ;;
  *) echo 'Usage: camera-job.sh build|sync' >&2; exit 2 ;;
esac
