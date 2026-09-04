#!/usr/bin/env bash
set -euo pipefail

# --- config (paths must be absolute)
INPUT_DIR="/mnt/ship/Data/FULL_CSV/2026_LEG_03"
# additional seasons to include; repeat --indir for each
PREV_DIRS=("/mnt/ship/Share/2025/2025_LEG_04")
OUTPUT_DIR=$(mktemp -d)
STATION_CSV="/mnt/ship/Data/Rosette/2026_LEG_03/Logs/2026_03_CTD_logbook.csv"
SCRIPT="/home/cryomics/Desktop/underway/AMUNDSEN/underway_dashboard_v6.R"

# persistent cache lives beside output
CACHE_DIR="/home/cryomics/Desktop/underway/AMUNDSEN/cache"
mkdir -p "$CACHE_DIR" "$OUTPUT_DIR"

# avoid overlapping runs (unit is also set to be non-parallel, this is belt & suspenders)
exec 9>"$CACHE_DIR/.run.lock"
flock -n 9 || { echo "Another run is in progress; exiting."; exit 0; }

# env
export TZ="America/Toronto"
export R_LIBS_USER="${R_LIBS_USER:-/home/cryomics/R/x86_64-pc-linux-gnu-library/%v}"
# Optional: secrets via environment file (see service unit below)
export GCAL_SERVICE_JSON="/home/cryomics/Desktop/underway/AMUNDSEN/.noble-vortex-471516-d6-7c784035a438.json"

# run
PREV_ARGS=(); for d in "${PREV_DIRS[@]}"; do PREV_ARGS+=(--indir "$d"); done

/usr/bin/Rscript "$SCRIPT" \
  --indir "$INPUT_DIR" \
  "${PREV_ARGS[@]}" \
  --outdir "$OUTPUT_DIR" \
  --stations "$STATION_CSV"

echo "Run finished at $(date)"

