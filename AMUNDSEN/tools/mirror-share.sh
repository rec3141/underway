#!/usr/bin/env bash
# Mirror the parts of the ship's shares the dashboard reads onto local disk.
#
#   mirror-share.sh [DEST]        default DEST=/data/ship
#
# The build then runs entirely against the mirror (UNDERWAY_DATA_ROOT=DEST/Data,
# UNDERWAY_SHARE_ROOT=DEST/Share). One rsync pass per build does all the
# network I/O in bulk; every other access on the share costs a CIFS round trip
# per call, which is what made per-file reads and stats so slow.
#
# Only what is needed is copied: ACSD day files, CTD logbooks and plots, the
# SeaBird .cnv files, MVP .m1 profiles, and the event logs. Nothing is deleted
# from the mirror when it vanishes from the share; a file that disappears
# upstream is usually a mount hiccup, not a retraction.
set -euo pipefail

DEST=${1:-/data/ship}
SRC_DATA=${UNDERWAY_MIRROR_DATA:-/mnt/ship/Data}
SRC_SHARE=${UNDERWAY_MIRROR_SHARE:-/mnt/ship/Share}
LOG=${DEST}/mirror.log
mkdir -p "$DEST/Data" "$DEST/Share"

# one mirror at a time
exec 8>"$DEST/.mirror.lock"
flock -n 8 || { echo "mirror already running"; exit 0; }

# rsync over a CIFS mount: size+mtime comparison, no checksums; whole files
# (delta transfer only adds CPU here); modest timeouts so a dropped mount fails
# fast instead of hanging the build behind it
RS=(rsync -rt --whole-file --modify-window=2 --timeout=60 --prune-empty-dirs --info=stats1)

ts() { date '+%F %T'; }
run() {   # run <label> <src> <dst> <filter...>
  local label=$1 src=$2 dst=$3; shift 3
  if [[ ! -d $src ]]; then echo "$(ts) $label: source missing ($src)" | tee -a "$LOG"; return 0; fi
  mkdir -p "$dst"
  local out rc=0
  out=$("${RS[@]}" "$@" "$src/" "$dst/" 2>&1) || rc=$?
  if [[ $rc -eq 0 ]]; then
    echo "$(ts) $label: $(echo "$out" | grep -E 'Number of regular files transferred|Total transferred file size' | sed -E 's/ +/ /g' | tr '\n' ';')" | tee -a "$LOG"
  else
    echo "$(ts) $label: rsync exit $rc — $(echo "$out" | grep -v '^$' | tail -1)" | tee -a "$LOG"
  fi
}

# --- Data share ---------------------------------------------------------------
run "FULL_CSV"  "$SRC_DATA/FULL_CSV"             "$DEST/Data/FULL_CSV" \
    --include='*/' --include='ACSD_????????.csv' --exclude='*'
run "Rosette"   "$SRC_DATA/Rosette"              "$DEST/Data/Rosette" \
    --include='*/' --include='Logs/*CTD_logbook.csv' --include='plots/*_raw_data.html' --exclude='*'
run "CTD cnv"   "$SRC_DATA/external_proprietary/CTD" "$DEST/Data/external_proprietary/CTD" \
    --include='*.cnv' --include='*.CNV' --exclude='*'
run "MVP"       "$SRC_DATA/MVP"                  "$DEST/Data/MVP" \
    --include='*/' --include='*.m1' --exclude='*'
run "EventLog"  "$SRC_DATA/EventLog"             "$DEST/Data/EventLog" \
    --include='*/' --include='Eventlog_*.xls' --include='Eventlog_*.xlsx' --exclude='*'

# --- archived seasons on the Share drive: Share/<year>/<leg>/ACSD_*.csv --------
if [[ -d $SRC_SHARE ]]; then
  for ydir in "$SRC_SHARE"/[0-9][0-9][0-9][0-9]; do
    [[ -d $ydir ]] || continue
    y=$(basename "$ydir")
    run "Share/$y" "$ydir" "$DEST/Share/$y" \
        --include='/*_LEG_*/' --include='/*_LEG_*/ACSD_????????.csv' --exclude='*'
  done
fi
echo "$(ts) mirror pass complete" | tee -a "$LOG"
