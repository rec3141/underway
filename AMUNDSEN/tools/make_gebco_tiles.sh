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
# With LAND set to an OGR source of land polygons (layer LAND_LAYER, default
# "land"; the OSM polygons the coastline tiles are cut from), the shore comes
# from the polygons instead of GEBCO's zero contour within LAND_BBOX (lon/lat,
# default the bbox): a strait the polygons keep open stays open in the
# picture, and GEBCO only colours the depth inside it.
# Needs GDAL >= 3.4 (gdalbuildvrt, gdalwarp, gdaldem, gdal2tiles.py) on the
# PATH and its Python bindings in /usr/bin/python3 or the interpreter GDAL_PYTHON names.
set -euo pipefail

ZIP=${1:?zip}; OUT=${2:?outdir}
BBOX=(${3:--150} ${4:-45} ${5:--15} ${6:-86})
ZOOMS=${7:-2-9}
ZMAX=${ZOOMS##*-}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gebco.XXXXXX"); trap 'rm -rf "$WORK"' EXIT
export GDAL_CACHEMAX=4096 GDAL_NUM_THREADS=ALL_CPUS GDAL_PAM_ENABLED=NO   # no .aux.xml beside each tile
PY=${GDAL_PYTHON:-/usr/bin/python3}   # the interpreter with GDAL's Python bindings (python3-gdal), or a conda env's, named in GDAL_PYTHON
# a conda env's GDAL finds its PROJ database and GDAL data only when told (activating the env would): tell it
prefix=${PY%/bin/*}
[[ -z ${PROJ_DATA:-} && -f $prefix/share/proj/proj.db ]] && export PROJ_DATA=$prefix/share/proj
[[ -z ${GDAL_DATA:-} && -d $prefix/share/gdal ]] && export GDAL_DATA=$prefix/share/gdal
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

# the colour ramp's input: the elevation as warped, or, with land polygons,
# the elevation nudged to the right side of zero where the polygons disagree
# with it (a cell the polygons call water is at least 1 m deep; a cell they
# call land is at least 1 m high) within the polygons' box
COLOR_SRC="$WORK/region_3857.tif"
if [[ -n ${LAND:-} ]]; then
  LB=(${LAND_BBOX:-${BBOX[@]}})
  echo "shore from $LAND within lon ${LB[0]}..${LB[2]}, lat ${LB[1]}..${LB[3]}"
  gdal_rasterize -q -init 255 -burn 1 -l "${LAND_LAYER:-land}" -te "$TE_XMIN" "$TE_YMIN" "$TE_XMAX" "$TE_YMAX" -tr "$RES" "$RES" \
      -ot Byte "${CO[@]}" "$LAND" "$WORK/mask.tif"
  $PY - "$WORK" "${LB[@]}" <<'PYEOF'
import sys, math, numpy as np
from osgeo import gdal
gdal.UseExceptions()
w = sys.argv[1]; lon0, lat0, lon1, lat1 = map(float, sys.argv[2:6])
M = 20037508.342789244; R = 6378137.0; LIM = 85.0511287798
mx = lambda lon: lon / 180 * M
my = lambda lat: R * math.log(math.tan(math.pi / 4 + math.radians(max(-LIM, min(LIM, lat))) / 2))
src = gdal.Open(f"{w}/region_3857.tif"); msk = gdal.Open(f"{w}/mask.tif"); gt = src.GetGeoTransform()
out = gdal.GetDriverByName("GTiff").Create(f"{w}/adj.tif", src.RasterXSize, src.RasterYSize, 1, gdal.GDT_Int16,
                                           ["COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"])
out.SetGeoTransform(gt); out.SetProjection(src.GetProjection())
# the polygons' box in cells: outside it the mask says nothing (255) and GEBCO's own zero contour stands
c0 = max(0, int((mx(lon0) - gt[0]) / gt[1])); c1 = min(src.RasterXSize, int(math.ceil((mx(lon1) - gt[0]) / gt[1])))
r0 = max(0, int((gt[3] - my(lat1)) / -gt[5])); r1 = min(src.RasterYSize, int(math.ceil((gt[3] - my(lat0)) / -gt[5])))
step = 1024
for y0 in range(0, src.RasterYSize, step):
    n = min(step, src.RasterYSize - y0)
    h = src.GetRasterBand(1).ReadAsArray(0, y0, src.RasterXSize, n)
    m = msk.GetRasterBand(1).ReadAsArray(0, y0, src.RasterXSize, n)
    ya, yb = max(r0, y0), min(r1, y0 + n)
    if ya < yb:
        box = np.zeros(m.shape, bool); box[ya - y0:yb - y0, c0:c1] = True
        m[box & (m == 255)] = 0                       # inside the box, not land is water
    h = np.where((m == 1) & (h <= 0), 1, h); h = np.where((m == 0) & (h > 0), -1, h)
    out.GetRasterBand(1).WriteArray(h.astype(np.int16), 0, y0)
out.FlushCache()
PYEOF
  COLOR_SRC="$WORK/adj.tif"
fi

# GEBCO samples 15 arc-seconds in both directions, so on the ground its cells
# are as many times taller than wide as 1/cos(latitude): 88 m by 461 m at
# 79 N. Drawn as they are, a single shallow cell becomes a tall thin streak
# and the eye reads north-south ridges that are not there. Each row is
# averaged across that many cells first, which shows the seabed at the
# resolution the grid actually has, and leaves the equator untouched.
echo "matching the grid's east-west and north-south detail"
$PY - "$WORK" "$COLOR_SRC" <<'PYEOF'
import sys, numpy as np
from osgeo import gdal
gdal.UseExceptions()
w, source = sys.argv[1], sys.argv[2]
src = gdal.Open(source); gt = src.GetGeoTransform()
out = gdal.GetDriverByName("GTiff").Create(f"{w}/even.tif", src.RasterXSize, src.RasterYSize, 1, gdal.GDT_Int16,
                                           ["COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"])
out.SetGeoTransform(gt); out.SetProjection(src.GetProjection())
R = 6378137.0
CELL = 15 / 3600 * 110574.0          # a GEBCO cell north to south, in ground metres
step = 1024
for y0 in range(0, src.RasterYSize, step):
    n = min(step, src.RasterYSize - y0)
    ymerc = gt[3] + (np.arange(y0, y0 + n) + 0.5) * gt[5]
    cosphi = np.cos(np.arctan(np.sinh(ymerc / R)))
    band = src.GetRasterBand(1).ReadAsArray(0, y0, src.RasterXSize, n).astype(np.float32)
    # a cell stands this many pixels tall here; the row is blurred across that
    # much, so a feature needs the same width either way to survive
    widths = np.clip(np.rint(CELL / cosphi / gt[1]), 1, 501).astype(int)
    for width in np.unique(widths):
        rows = np.nonzero(widths == width)[0]
        if width < 3:
            continue
        sigma = width / 2.355                       # the window is the full width at half maximum
        reach = int(np.ceil(3 * sigma))
        kernel = np.exp(-0.5 * (np.arange(-reach, reach + 1) / sigma) ** 2)
        kernel /= kernel.sum()
        padded = np.pad(band[rows], ((0, 0), (reach, reach)), mode="edge")
        blurred = np.empty_like(band[rows])
        for k, weight in enumerate(kernel):
            part = padded[:, k:k + band.shape[1]] * weight
            blurred = part if k == 0 else blurred + part
        band[rows] = blurred
    out.GetRasterBand(1).WriteArray(np.rint(band).astype(np.int16), 0, y0)
out.FlushCache()
PYEOF
COLOR_SRC="$WORK/even.tif"

# depth ramp: Crameri's oslo, which is perceptually uniform and keeps its
# order in greyscale and to a colour-blind eye, from dark abyss to lit shelf,
# with a hard step at the 100 m isobath into a desaturated shoal band so that
# the line a ship keeps off reads at a glance. Land carries a muted
# hypsometric ramp (olive lowlands to pale high ground) so it sits under the
# hillshade without competing with the track colours.
cat > "$WORK/ramp.txt" <<'EOF'
-6000  12 26 40
-5000  15 34 55
-4000  18 44 72
-3000  24 60 96
-2500  29 70 112
-2000  34 80 130
-1500  45 95 150
-1000  62 113 172
-750   80 128 188
-500   100 143 196
-300   118 155 202
-200   134 167 208
-100.01 150 180 214
-100   150 174 170
-50    170 192 184
-20    186 205 194
-0.01  198 214 202
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
gdaldem color-relief -q -alpha "${CO[@]}" "$COLOR_SRC" "$WORK/ramp.txt" "$WORK/color.tif"

# Rasters here run to billions of pixels, so the Python steps work in strips
# of rows and never hold a whole band.
echo "ground-scaled elevation + hillshade"
$PY - "$WORK" "$COLOR_SRC" <<'PY'
import sys, math, numpy as np
from osgeo import gdal
gdal.UseExceptions()
w = sys.argv[1]
src = gdal.Open(sys.argv[2]); gt = src.GetGeoTransform()
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
# lit from several sides: one azimuth over a grid this anisotropic puts a
# grain on the water that reads as ridges
gdaldem hillshade -q -z 1.2 -alt 40 -multidirectional -compute_edges "${CO[@]}" "$WORK/ground.tif" "$WORK/shade.tif"

# blend: multiply the colour by the hillshade so slopes read
echo "blending"
$PY - "$WORK" "$COLOR_SRC" <<'PY'
import sys, numpy as np
from osgeo import gdal
gdal.UseExceptions()
w = sys.argv[1]
c = gdal.Open(f"{w}/color.tif"); s = gdal.Open(f"{w}/shade.tif")
# the isobaths a reader takes their bearings from: the 100 m line the routes
# keep off, then the shelf break and the basins, each drawn where neighbouring
# cells fall on either side of the depth
elev = gdal.Open(sys.argv[2])
LEVELS = {100: .55, 200: .78, 500: .82, 1000: .85, 2000: .88}
out = gdal.GetDriverByName("GTiff").Create(f"{w}/shaded.tif", c.RasterXSize, c.RasterYSize, 4, gdal.GDT_Byte,
                                           ["COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES", "PHOTOMETRIC=RGB", "ALPHA=YES"])
out.SetGeoTransform(c.GetGeoTransform()); out.SetProjection(c.GetProjection())
step = 1024
for y0 in range(0, c.RasterYSize, step):
    n = min(step, c.RasterYSize - y0)
    sh = s.GetRasterBand(1).ReadAsArray(0, y0, c.RasterXSize, n).astype(np.float32) / 255.0
    k = 0.45 + 0.65 * sh          # 0.45..1.1: shadows darken, lit slopes brighten a little
    rows = min(n + 1, c.RasterYSize - y0)
    depth = -elev.GetRasterBand(1).ReadAsArray(0, y0, c.RasterXSize, rows).astype(np.float32)
    for level, shade in LEVELS.items():
        side = depth > level
        line = np.zeros(side.shape, bool)
        line[:, :-1] |= side[:, :-1] != side[:, 1:]
        if rows > 1:
            line[:-1] |= side[:-1] != side[1:]
        k = np.where(line[:n] & (depth[:n] > 0), k * shade, k)
    for b in range(1, 4):
        v = c.GetRasterBand(b).ReadAsArray(0, y0, c.RasterXSize, n)
        out.GetRasterBand(b).WriteArray(np.clip(v * k, 0, 255).astype(np.uint8), 0, y0)
    out.GetRasterBand(4).WriteArray(c.GetRasterBand(4).ReadAsArray(0, y0, c.RasterXSize, n), 0, y0)
out.FlushCache()
PY
rm -f "$WORK/color.tif" "$WORK/shade.tif" "$WORK/ground.tif" "$WORK/region_3857.tif" "$WORK/adj.tif" "$WORK/mask.tif"

echo "tiling zooms $ZOOMS -> $OUT"
mkdir -p "$OUT"
gdal2tiles.py -q --xyz -z "$ZOOMS" -w none -r bilinear --processes="$(nproc)" "$WORK/shaded.tif" "$OUT"
echo "done: $(find "$OUT" -name '*.png' | wc -l) tiles, $(du -sh "$OUT" | cut -f1)"
