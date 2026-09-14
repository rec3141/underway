#!/usr/bin/env bash
# The map's GEBCO relief pyramid as the ship carries it, in two runs of
# make_gebco_tiles.sh into one staging directory that is then swapped in:
# the globe at zooms 0-8, and the western Arctic and Labrador Sea box at
# zoom 9 on top. The dashboard's build reads the finished directory and
# puts each run into the map as its own source (raster_pyramid in build.py).
#
#   tools/rerender-gebco.sh ZIP [OUTDIR]
#
# ZIP     gebco_2024_sub_ice_topo_geotiff.zip (BODC/CEDA, 4.4 GB)
# OUTDIR  the pyramid, default $UNDERWAY_TILES_DIR/gebco (/data/gis/tiles/gebco)
#
# Settings, from the environment:
#   LAND, LAND_LAYER, LAND_BBOX   the OSM land polygons that shape the shore
#                                 inside their box (see make_gebco_tiles.sh)
#   GDAL_PYTHON                   the interpreter with GDAL's Python bindings
#   TMPDIR                        room for the intermediate rasters: tens of GB
#   ARCTIC_BBOX                   the z9 box, lon/lat (default -150 45 -15 86)
# The previous pyramid stays beside the new one as OUTDIR.old until the next run.
set -euo pipefail
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ZIP=${1:?zip}
OUT=${2:-${UNDERWAY_TILES_DIR:-/data/gis/tiles}/gebco}
read -r -a ARCTIC <<<"${ARCTIC_BBOX:--150 45 -15 86}"
STAGE=$OUT.new
rm -rf "$STAGE"; mkdir -p "$STAGE"
echo "== the globe, zooms 0-8 -> $STAGE"
"$HERE/make_gebco_tiles.sh" "$ZIP" "$STAGE" -180 -90 180 90 0-8
echo "== the Arctic box, zoom 9 -> $STAGE"
"$HERE/make_gebco_tiles.sh" "$ZIP" "$STAGE" "${ARCTIC[@]}" 9-9
rm -rf "$OUT.old"
[[ -d $OUT ]] && mv "$OUT" "$OUT.old"
mv "$STAGE" "$OUT"
touch "$OUT"                                   # the directory's mtime versions the tiles' URLs
echo "done: $OUT, $(find "$OUT" -name '*.png' | wc -l) tiles, $(du -sh "$OUT" | cut -f1)"
