#!/usr/bin/env bash
# Build a shaded relief (bathymetry + land) tile pyramid from the GEBCO 2024 GeoTIFF release.
#
#   make_gebco_tiles.sh ZIP OUTDIR [XMIN YMIN XMAX YMAX] [ZMIN-ZMAX]
#
# ZIP     gebco_2024_sub_ice_topo_geotiff.zip (eight global tiles inside)
# OUTDIR  where the {z}/{x}/{y}.png pyramid goes; serve it from the web root
# bbox    lon/lat, default the western Arctic and Labrador Sea
# zooms   default 2-9 (~300 m/px at 75 N at z9 — about GEBCO's own 15" grid)
#
# The pyramid is Web Mercator (EPSG:3857), which is what the dashboard's
# MapLibre map uses. Depth is rendered as a colour ramp with a hillshade
# blended in; land carries a hypsometric ramp under the same hillshade.
# Needs GDAL >= 3.4 (gdalbuildvrt, gdalwarp, gdaldem, gdal2tiles.py).
set -euo pipefail

ZIP=${1:?zip}; OUT=${2:?outdir}
BBOX=(${3:--150} ${4:-45} ${5:--15} ${6:-86})
ZOOMS=${7:-2-9}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gebco.XXXXXX"); trap 'rm -rf "$WORK"' EXIT
export GDAL_CACHEMAX=2048 GDAL_NUM_THREADS=ALL_CPUS

# the release is eight 90x90-degree tiles; only unpack the ones the box touches
echo "unpacking"
pat=()
for hemi in "n90.0_s0.0" "n0.0_s-90.0"; do
  [[ $hemi == n90* && ${BBOX[3]%.*} -le 0 ]] && continue
  [[ $hemi == n0.0* && ${BBOX[1]%.*} -ge 0 ]] && continue
  for lonband in "w-180.0_e-90.0" "w-90.0_e0.0" "w0.0_e90.0" "w90.0_e180.0"; do
    w=${lonband#w}; w=${w%%_*}; e=${lonband##*_e}
    (( $(printf '%.0f' "${BBOX[2]}") <= ${w%.*} || $(printf '%.0f' "${BBOX[0]}") >= ${e%.*} )) && continue
    pat+=("*_${hemi}_${lonband}.tif")
  done
done
unzip -q -o "$ZIP" "${pat[@]}" -d "$WORK/src"
echo "  tiles: $(ls "$WORK/src" | tr '\n' ' ')"
gdalbuildvrt -q "$WORK/global.vrt" "$WORK"/src/*.tif

echo "clipping to lon ${BBOX[0]}..${BBOX[2]}, lat ${BBOX[1]}..${BBOX[3]}"
gdal_translate -q -projwin "${BBOX[0]}" "${BBOX[3]}" "${BBOX[2]}" "${BBOX[1]}" \
    -co COMPRESS=DEFLATE -co TILED=YES "$WORK/global.vrt" "$WORK/region.tif"

echo "reprojecting to web mercator"
gdalwarp -q -t_srs EPSG:3857 -r bilinear -multi -co COMPRESS=DEFLATE -co TILED=YES \
    "$WORK/region.tif" "$WORK/region_3857.tif"

# depth ramp: pale shelf to dark abyss; land a muted hypsometric ramp (olive
# lowlands to pale high ground) so it reads under the hillshade without
# competing with the track colours
cat > "$WORK/ramp.txt" <<'EOF'
-6000  6 14 34
-4000  10 26 58
-3000  16 40 82
-2500  22 52 100
-2000  28 66 118
-1500  36 82 136
-1000  46 100 154
-750   58 116 168
-500   72 132 180
-300   90 150 192
-200   108 166 202
-100   130 182 212
-50    152 198 222
-20    176 212 230
0      196 224 236
0.01   58 72 62
150    74 88 70
400    98 104 80
800    124 118 94
1200   148 140 116
1800   176 170 152
2500   206 204 198
3500   232 232 232
EOF
echo "colour relief + hillshade"
gdaldem color-relief -q -alpha "$WORK/region_3857.tif" "$WORK/ramp.txt" "$WORK/color.tif"
gdaldem hillshade -q -z 2.5 -az 315 -alt 40 -compute_edges "$WORK/region_3857.tif" "$WORK/shade.tif"

# blend: multiply the colour by the hillshade (0.55..1.0) so slopes read.
# The system interpreter is the one with GDAL's Python bindings (python3-gdal).
/usr/bin/python3 - "$WORK" <<'PY'
import sys, numpy as np
from osgeo import gdal
w = sys.argv[1]
c = gdal.Open(f"{w}/color.tif"); s = gdal.Open(f"{w}/shade.tif")
sh = s.GetRasterBand(1).ReadAsArray().astype(np.float32) / 255.0
k = 0.45 + 0.65 * sh          # 0.45..1.1: shadows darken, lit slopes brighten a little
drv = gdal.GetDriverByName("GTiff")
out = drv.Create(f"{w}/shaded.tif", c.RasterXSize, c.RasterYSize, 4, gdal.GDT_Byte,
                 ["COMPRESS=DEFLATE", "TILED=YES", "PHOTOMETRIC=RGB", "ALPHA=YES"])
out.SetGeoTransform(c.GetGeoTransform()); out.SetProjection(c.GetProjection())
for b in range(1, 4):
    out.GetRasterBand(b).WriteArray(np.clip(c.GetRasterBand(b).ReadAsArray() * k, 0, 255).astype(np.uint8))
out.GetRasterBand(4).WriteArray(c.GetRasterBand(4).ReadAsArray())
out.FlushCache()
PY

echo "tiling zooms $ZOOMS -> $OUT"
mkdir -p "$OUT"
gdal2tiles.py -q --xyz -z "$ZOOMS" -w none -r bilinear --processes="$(nproc)" "$WORK/shaded.tif" "$OUT"
echo "done: $(find "$OUT" -name '*.png' | wc -l) tiles, $(du -sh "$OUT" | cut -f1)"
