#!/usr/bin/env bash
# Build the chart of where the map's depths came from: GEBCO's Type Identifier
# grid as a tile pyramid, in the same colours the dashboard's depth dot uses.
#
#   make_survey_tiles.sh TID_NC OUTDIR [XMIN YMIN XMAX YMAX] [ZMIN-ZMAX]
#
# TID_NC  GEBCO_2024_TID.nc (the Type Identifier release, netCDF)
# OUTDIR  where the {z}/{x}/{y}.png pyramid goes, normally
#         $UNDERWAY_TILES_DIR/survey, which the map's Bathy button then offers
#
# Only about a quarter of this water has ever been sounded, and what has is
# very largely ship tracks, so this chart is mostly a picture of where ships
# have been. It is drawn flat, without a hillshade: the classes are
# categorical, and shading them would imply a surface they do not describe.
set -euo pipefail
TID=${1:?tid netcdf}; OUT=${2:?outdir}
BBOX=(${3:--150} ${4:-45} ${5:--15} ${6:-86})
ZOOMS=${7:-2-8}
ZMAX=${ZOOMS##*-}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/survey.XXXXXX"); trap 'rm -rf "$WORK"' EXIT
export GDAL_CACHEMAX=4096 GDAL_NUM_THREADS=ALL_CPUS GDAL_PAM_ENABLED=NO
PY=${GDAL_PYTHON:-/usr/bin/python3}
CO=(-co COMPRESS=DEFLATE -co TILED=YES -co BIGTIFF=YES)

read -r TE_XMIN TE_YMIN TE_XMAX TE_YMAX RES < <($PY - "${BBOX[@]}" "$ZMAX" <<'PYEOF'
import math, sys
xmin, ymin, xmax, ymax = map(float, sys.argv[1:5]); zmax = int(sys.argv[5])
M = 20037508.342789244; R = 6378137.0; LIM = 85.0511287798
mx = lambda lon: lon / 180 * M
my = lambda lat: R * math.log(math.tan(math.pi / 4 + math.radians(max(-LIM, min(LIM, lat))) / 2))
res = 2 * M / (256 * 2 ** zmax)
lo = lambda v: math.floor((v + M) / res + 1e-6) * res - M
hi = lambda v: math.ceil((v + M) / res - 1e-6) * res - M
print(f"{lo(mx(xmin)):.6f} {lo(my(ymin)):.6f} {hi(mx(xmax)):.6f} {hi(my(ymax)):.6f} {res:.9f}")
PYEOF
)
echo "reprojecting the type identifiers to web mercator at z$ZMAX"
# mode, not average: these are classes, and a mean of two of them means nothing
gdalwarp -q -t_srs EPSG:3857 -te "$TE_XMIN" "$TE_YMIN" "$TE_XMAX" "$TE_YMAX" -tr "$RES" "$RES" \
    -r mode -ot Byte -multi -wo NUM_THREADS=ALL_CPUS -wm 4096 "${CO[@]}" \
    "NETCDF:${TID}:tid" "$WORK/tid.tif"

# the dashboard's own legend: the surveyed kinds bright, the guessed ones the
# quiet blues, land dark. Each class is a flat band of its own.
cat > "$WORK/ramp.txt" <<'RAMPEOF'
0 48 48 52
9.99 48 48 52
10 255 167 38
10.99 255 167 38
11 255 241 118
11.99 255 241 118
12 240 98 146
15.99 240 98 146
16 141 214 78
17.99 141 214 78
40 21 60 110
40.99 21 60 110
41 46 110 160
41.99 46 110 160
42 186 104 200
45.99 186 104 200
70 80 92 128
72.99 80 92 128
90 154 160 170
255 154 160 170
RAMPEOF
echo "colouring"
gdaldem color-relief -q -alpha "${CO[@]}" "$WORK/tid.tif" "$WORK/ramp.txt" "$WORK/survey.tif"
echo "tiling zooms $ZOOMS -> $OUT"
mkdir -p "$OUT"
gdal2tiles.py -q --xyz -z "$ZOOMS" -w none -r near --processes="$(nproc)" "$WORK/survey.tif" "$OUT"
printf 'done: %s, %s tiles, %s\n' "$OUT" "$(find "$OUT" -name '*.png' | wc -l)" "$(du -sh "$OUT" | cut -f1)"
