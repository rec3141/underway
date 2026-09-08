#!/usr/bin/env bash
# Build a shaded relief (bathymetry + land) tile pyramid from the GEBCO 2024 GeoTIFF release.
#
#   make_gebco_tiles.sh ZIP OUTDIR [XMIN YMIN XMAX YMAX] [ZMIN-ZMAX]
#
# ZIP     gebco_2024_sub_ice_topo_geotiff.zip (eight global tiles inside)
# OUTDIR  where the {z}/{x}/{y}.png pyramid goes; the server maps /static/tiles/ onto its parent
# bbox    lon/lat, default the western Arctic and Labrador Sea; -180 -90 180 90 is the globe
# zooms   default 2-9. The raster is warped at ZMAX's resolution, so pick ZMAX by
#         what GEBCO's 15" grid can support: z8 (610 m/px) for the globe, z9 for
#         the Arctic, where Mercator stretches a 15" cell to 1-2 km on the tile grid.
#
# The pyramid is Web Mercator (EPSG:3857), which is what the dashboard's
# MapLibre map uses; the warp is snapped to the tile grid so tiles are pixel
# exact. Depth is rendered as a colour ramp with a hillshade blended in; land
# carries a hypsometric ramp under the same hillshade. The hillshade is taken
# on ground metres, not Mercator metres: each row is scaled by cos(latitude)
# first, so a slope looks the same at the equator as at 80 N and a global
# build joins a regional one without a seam in the shading.
# Needs GDAL >= 3.4 (gdalbuildvrt, gdalwarp, gdaldem, gdal2tiles.py).
set -euo pipefail

ZIP=${1:?zip}; OUT=${2:?outdir}
BBOX=(${3:--150} ${4:-45} ${5:--15} ${6:-86})
ZOOMS=${7:-2-9}
ZMAX=${ZOOMS##*-}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gebco.XXXXXX"); trap 'rm -rf "$WORK"' EXIT
export GDAL_CACHEMAX=4096 GDAL_NUM_THREADS=ALL_CPUS GDAL_PAM_ENABLED=NO   # no .aux.xml beside each tile
PY=/usr/bin/python3            # the system interpreter is the one with GDAL's Python bindings (python3-gdal)
CO=(-co COMPRESS=DEFLATE -co TILED=YES -co BIGTIFF=YES)

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

# the target window: the box in Mercator metres, latitude clamped to the
# projection's limit, snapped outward to the tile grid at ZMAX
read -r TE_XMIN TE_YMIN TE_XMAX TE_YMAX RES < <($PY - "${BBOX[@]}" "$ZMAX" <<'PY'
import math, sys
xmin, ymin, xmax, ymax = map(float, sys.argv[1:5]); zmax = int(sys.argv[5])
M = 20037508.342789244; R = 6378137.0; LIM = 85.0511287798
mx = lambda lon: lon / 180 * M
my = lambda lat: R * math.log(math.tan(math.pi / 4 + math.radians(max(-LIM, min(LIM, lat))) / 2))
res = 2 * M / (256 * 2 ** zmax)
lo = lambda v: math.floor((v + M) / res + 1e-6) * res - M
hi = lambda v: math.ceil((v + M) / res - 1e-6) * res - M
print(f"{lo(mx(xmin)):.6f} {lo(my(ymin)):.6f} {hi(mx(xmax)):.6f} {hi(my(ymax)):.6f} {res:.9f}")
PY
)
echo "reprojecting lon ${BBOX[0]}..${BBOX[2]}, lat ${BBOX[1]}..${BBOX[3]} to web mercator at z$ZMAX (${RES%.*} m/px)"
gdalwarp -q -t_srs EPSG:3857 -te "$TE_XMIN" "$TE_YMIN" "$TE_XMAX" "$TE_YMAX" -tr "$RES" "$RES" \
    -r bilinear -multi -wo NUM_THREADS=ALL_CPUS -wm 4096 "${CO[@]}" \
    "$WORK/global.vrt" "$WORK/region_3857.tif"

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
echo "colour relief"
gdaldem color-relief -q -alpha "${CO[@]}" "$WORK/region_3857.tif" "$WORK/ramp.txt" "$WORK/color.tif"

# Rasters here run to billions of pixels, so the Python steps work in strips
# of rows and never hold a whole band.
echo "ground-scaled elevation + hillshade"
$PY - "$WORK" <<'PY'
import sys, math, numpy as np
from osgeo import gdal
gdal.UseExceptions()
w = sys.argv[1]
src = gdal.Open(f"{w}/region_3857.tif"); gt = src.GetGeoTransform()
out = gdal.GetDriverByName("GTiff").Create(f"{w}/ground.tif", src.RasterXSize, src.RasterYSize, 1, gdal.GDT_Float32,
                                           ["COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"])
out.SetGeoTransform(gt); out.SetProjection(src.GetProjection())
R = 6378137.0; step = 1024
for y0 in range(0, src.RasterYSize, step):
    n = min(step, src.RasterYSize - y0)
    ymerc = gt[3] + (np.arange(y0, y0 + n) + 0.5) * gt[5]
    cosphi = np.cos(np.arctan(np.sinh(ymerc / R))).astype(np.float32)
    h = src.GetRasterBand(1).ReadAsArray(0, y0, src.RasterXSize, n).astype(np.float32)
    out.GetRasterBand(1).WriteArray(h * cosphi[:, None], 0, y0)
out.FlushCache()
PY
gdaldem hillshade -q -z 1.2 -az 315 -alt 40 -compute_edges "${CO[@]}" "$WORK/ground.tif" "$WORK/shade.tif"

# blend: multiply the colour by the hillshade so slopes read
echo "blending"
$PY - "$WORK" <<'PY'
import sys, numpy as np
from osgeo import gdal
gdal.UseExceptions()
w = sys.argv[1]
c = gdal.Open(f"{w}/color.tif"); s = gdal.Open(f"{w}/shade.tif")
out = gdal.GetDriverByName("GTiff").Create(f"{w}/shaded.tif", c.RasterXSize, c.RasterYSize, 4, gdal.GDT_Byte,
                                           ["COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES", "PHOTOMETRIC=RGB", "ALPHA=YES"])
out.SetGeoTransform(c.GetGeoTransform()); out.SetProjection(c.GetProjection())
step = 1024
for y0 in range(0, c.RasterYSize, step):
    n = min(step, c.RasterYSize - y0)
    sh = s.GetRasterBand(1).ReadAsArray(0, y0, c.RasterXSize, n).astype(np.float32) / 255.0
    k = 0.45 + 0.65 * sh          # 0.45..1.1: shadows darken, lit slopes brighten a little
    for b in range(1, 4):
        v = c.GetRasterBand(b).ReadAsArray(0, y0, c.RasterXSize, n)
        out.GetRasterBand(b).WriteArray(np.clip(v * k, 0, 255).astype(np.uint8), 0, y0)
    out.GetRasterBand(4).WriteArray(c.GetRasterBand(4).ReadAsArray(0, y0, c.RasterXSize, n), 0, y0)
out.FlushCache()
PY
rm -f "$WORK/color.tif" "$WORK/shade.tif" "$WORK/ground.tif" "$WORK/region_3857.tif"

echo "tiling zooms $ZOOMS -> $OUT"
mkdir -p "$OUT"
gdal2tiles.py -q --xyz -z "$ZOOMS" -w none -r bilinear --processes="$(nproc)" "$WORK/shaded.tif" "$OUT"
echo "done: $(find "$OUT" -name '*.png' | wc -l) tiles, $(du -sh "$OUT" | cut -f1)"
