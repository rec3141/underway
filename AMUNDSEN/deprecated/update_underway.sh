#!/usr/bin/env bash
set -euo pipefail
testing=${1:-""} #use --testing for testing

# --- paths (edit these two if needed) ---
INPUT_DIR="/mnt/ship/Data/FULL_CSV/2026_LEG_03/"
PREV_DIRS=("/mnt/ship/Share/2025/2025_LEG_04")   # additional seasons; one --indir each   # where ACSD_YYYYMMDD.csv live
WEBROOT="/mnt/ship/Share/2026/2026_LEG_03/Collins/underway_dashboard/"   # published on the Share drive
# this gets mounted to /var/www/html/underway
OUTPUT_DIR=$(mktemp -d)
STATION_CSV="/mnt/ship/Data/Rosette/2026_LEG_03/Logs/2026_03_CTD_logbook.csv"
RSCRIPT="/usr/bin/Rscript"
DASH_R="/home/cryomics/Desktop/underway/AMUNDSEN/underway_dashboard_v6.R"

# persistent cache lives beside output
CACHE_DIR="/home/cryomics/Desktop/underway/AMUNDSEN/cache"
mkdir -p "$CACHE_DIR" "$OUTPUT_DIR"


# avoid overlapping runs (unit is also set to be non-parallel, this is belt & suspenders)
exec 9>"$CACHE_DIR/.run.lock"
flock -n 9 || { echo "Another run is in progress; exiting."; exit 0; }

# Be nice to the box + ensure files end up world-readable
umask 002
export TZ=America/Toronto
export GCAL_SERVICE_JSON="/home/cryomics/Desktop/underway/AMUNDSEN/.noble-vortex-471516-d6-7c784035a438.json"

# Generate dashboard (rolling windows + surprise panel)
PREV_ARGS=""; for d in "${PREV_DIRS[@]}"; do PREV_ARGS="$PREV_ARGS --indir $d"; done
CMD="nice -n 19 ionice -c3 $RSCRIPT $DASH_R --indir $INPUT_DIR$PREV_ARGS --outdir $OUTPUT_DIR --stations $STATION_CSV $testing"

eval $CMD
rsync -av ${OUTPUT_DIR}/ ${WEBROOT}

echo "[OK] $(date -u) report generated"
