#!/bin/bash
# Pull the vector tile sets from grid, where they are cut: the coastline
# (coast/) and the geographic names (names/), and any other directory with a
# metadata.json under grid's tiles directory. The ship reaches grid; grid
# cannot reach the ship, so the copy runs here. Nothing writes back.
#
#   tools/tiles-pull.sh            pull every tile set grid has
#   tools/tiles-pull.sh names      pull one set
#   tools/tiles-pull.sh status     what is here and what is on grid
#
# Where the tiles live is the installation's setting (UNDERWAY_TILES_DIR in
# /etc/underway/site.env, default /data/gis/tiles); grid's is the same path
# unless UNDERWAY_TILES_REMOTE says otherwise (host:path). Each set lands
# whole: rsync fills a sibling directory and the swap is one rename, so a
# build never reads a half-copied pyramid. The next build finds the new
# metadata.json and the map takes the layer from there; the GEBCO raster
# pyramid is not pulled (it is rendered on the ship, see the README).
set -euo pipefail
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
if [[ -r $SITE ]]; then set -a; . "$SITE"; set +a; fi
LOCAL=${UNDERWAY_TILES_DIR:-/data/gis/tiles}
ORIGIN=${UNDERWAY_TILES_REMOTE:-grid:/data/gis/tiles}
GRID=${ORIGIN%%:*}
REMOTE=${ORIGIN#*:}

remote_sets() { ssh "$GRID" "ls -d $REMOTE/*/metadata.json 2>/dev/null" | sed 's|.*/\([^/]*\)/metadata.json|\1|'; }
stamp() { [[ -f $1/metadata.json ]] && date -r "$1/metadata.json" +%F\ %R || echo "-"; }

case "${1:-all}" in
  status)
    echo "--- $LOCAL (ship)"
    for d in "$LOCAL"/*/; do [[ -f $d/metadata.json ]] && printf "  %-8s %s  %s\n" "$(basename "$d")" "$(stamp "$d")" "$(du -sh "$d" | cut -f1)"; done
    echo "--- $ORIGIN (grid)"
    ssh "$GRID" "cd $REMOTE && for d in */; do [ -f \$d/metadata.json ] && printf '  %-8s %s  %s\n' \"\${d%/}\" \"\$(date -r \$d/metadata.json +%F\ %R)\" \"\$(du -sh \$d | cut -f1)\"; done"
    ;;
  all|*)
    if [[ ${1:-all} == all ]]; then sets=$(remote_sets); else sets=$1; fi
    [[ -n $sets ]] || { echo "grid has no tile sets under $REMOTE" >&2; exit 1; }
    mkdir -p "$LOCAL"
    for s in $sets; do
      echo "== $s"
      # rsync straight into the live directory would leave a half set if the link
      # drops: fill a sibling (hard-linking what is already here, so an unchanged
      # set costs no space and no transfer) and swap it in when whole
      link=(); [[ -d $LOCAL/$s ]] && link=(--link-dest="$LOCAL/$s/")
      rsync -a --delete "${link[@]}" "$GRID:$REMOTE/$s/" "$LOCAL/$s.new/"
      if [[ -d $LOCAL/$s ]]; then mv "$LOCAL/$s" "$LOCAL/$s.old"; fi
      mv "$LOCAL/$s.new" "$LOCAL/$s"
      rm -rf "$LOCAL/$s.old"
      printf "  %s: %s tiles, %s, cut %s\n" "$s" "$(find "$LOCAL/$s" -name '*.pbf' | wc -l)" "$(du -sh "$LOCAL/$s" | cut -f1)" "$(stamp "$LOCAL/$s")"
    done
    echo "the next dashboard build picks the sets up from their metadata.json"
    ;;
esac
