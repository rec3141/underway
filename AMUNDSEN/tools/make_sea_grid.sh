#!/usr/bin/env bash
# Build the grid the sea route walks (dashboard/searoute.py): GEBCO on a polar
# stereographic plane, carrying per cell whether it holds water and what the
# elevation there is.
#
#   make_sea_grid.sh ZIP OUTDIR [XMIN YMIN XMAX YMAX] [METRES]
#
# GDAL_PYTHON names the interpreter carrying GDAL's Python bindings and numpy
# (default /usr/bin/python3, as the tile build uses).
#
# ZIP      gebco_2024_sub_ice_topo_geotiff.zip (the tile pyramid's source)
# OUTDIR   where elevation.npy, water.npy and grid.json go (UNDERWAY_SEA_GRID,
#          default sea-grid/ beside the tile pyramid)
# bbox     lon/lat, default the western Arctic and Labrador Sea
# metres   cell size on the plane, default 250
#
# The plane is EPSG:3413 (NSIDC north polar stereographic, the Arctic
# standard). It is conformal, so the scale is the same in every direction at a
# point and varies only with latitude; the router divides by that scale to get
# ground distances, which a lon/lat grid cannot do without stretching cells.
#
# A cell is water when the lowest GEBCO sample in it is below sea level, so a
# channel narrower than a cell stays open; an islet narrower than a cell drops
# out, which is the right side to err on for a distance estimate. The stored
# elevation is a separate bilinear sample, so a depth read off it is the
# seabed thereabouts rather than the deepest corner. Both arrays are plain
# .npy, so the server memory-maps them and touches only the window it walks.
# Needs GDAL (gdalbuildvrt, gdalwarp) and numpy.
set -euo pipefail
ZIP=${1:?zip}; OUT=${2:?outdir}
BBOX=(${3:--150} ${4:-45} ${5:--15} ${6:-86})
METRES=${7:-250}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/seagrid.XXXXXX"); trap 'rm -rf "$WORK"' EXIT
export GDAL_CACHEMAX=4096 GDAL_NUM_THREADS=ALL_CPUS GDAL_PAM_ENABLED=NO
PY=${GDAL_PYTHON:-/usr/bin/python3}

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
echo "unpacking ${pat[*]}"
unzip -q -o "$ZIP" "${pat[@]}" -d "$WORK/src"
gdalbuildvrt -q "$WORK/global.vrt" "$WORK"/src/*.tif

# The box on the plane: the lon/lat box's own corners are not its extent in a
# polar projection, so the boundary is walked and its extreme x and y taken.
read -r X0 Y0 X1 Y1 < <($PY - "${BBOX[@]}" "$METRES" <<'PYEOF'
import math, sys
from osgeo import osr
xmin, ymin, xmax, ymax, metres = map(float, sys.argv[1:6])
src = osr.SpatialReference(); src.ImportFromEPSG(4326); src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
dst = osr.SpatialReference(); dst.ImportFromEPSG(3413)
to = osr.CoordinateTransformation(src, dst)
step = 0.5
edge = [(xmin + i * step, y) for y in (ymin, ymax) for i in range(int((xmax - xmin) / step) + 1)]
edge += [(x, ymin + i * step) for x in (xmin, xmax) for i in range(int((ymax - ymin) / step) + 1)]
xs, ys = zip(*[to.TransformPoint(lon, lat)[:2] for lon, lat in edge])
out = [math.floor(min(xs) / metres) * metres, math.floor(min(ys) / metres) * metres,
       math.ceil(max(xs) / metres) * metres, math.ceil(max(ys) / metres) * metres]
print(" ".join(f"{v:.0f}" for v in out))
PYEOF
)
echo "plane box $X0 $Y0 $X1 $Y1 ($(( (X1 - X0) / 1000 )) x $(( (Y1 - Y0) / 1000 )) km)"
mkdir -p "$OUT"
for pass in min bilinear; do
  echo "warping to EPSG:3413 at ${METRES} m, $pass"
  gdalwarp -q -t_srs EPSG:3413 -te "$X0" "$Y0" "$X1" "$Y1" -tr "$METRES" "$METRES" \
      -r "$pass" -ot Int16 -dstnodata 32767 -of GTiff -co COMPRESS=DEFLATE -co TILED=YES -co BIGTIFF=YES \
      -multi -wo NUM_THREADS=ALL_CPUS -wm 4096 "$WORK/global.vrt" "$WORK/$pass.tif"
done
$PY - "$WORK/min.tif" "$WORK/bilinear.tif" "$OUT" <<'PY'
import json, sys
import numpy as np
from osgeo import gdal
gdal.UseExceptions()
lowest, sampled, out = sys.argv[1], sys.argv[2], sys.argv[3]
src = gdal.Open(sampled)
gt = src.GetGeoTransform()
rows, cols = src.RasterYSize, src.RasterXSize
elev = np.lib.format.open_memmap(f"{out}/elevation.npy", mode="w+", dtype=np.int16, shape=(rows, cols))
water = np.lib.format.open_memmap(f"{out}/water.npy", mode="w+", dtype=np.uint8, shape=(rows, (cols + 7) // 8))
low = gdal.Open(lowest)
step = 2048
for y in range(0, rows, step):
    n = min(step, rows - y)
    band = src.GetRasterBand(1).ReadAsArray(0, y, cols, n)
    deep = low.GetRasterBand(1).ReadAsArray(0, y, cols, n)
    band[band == 32767] = 0                       # off the edge of GEBCO: flat, and land by the rule below
    elev[y:y + n] = band
    water[y:y + n] = np.packbits((deep < 0) & (deep != 32767), axis=1)
elev.flush(); water.flush()
json.dump({"crs": "EPSG:3413", "x0": gt[0], "y0": gt[3], "metres": gt[1], "rows": rows, "cols": cols,
           "source": "GEBCO 2024 sub-ice, 15 arc-second"}, open(f"{out}/grid.json", "w"), indent=1)
print(f"{out}: {cols} x {rows} cells at {gt[1]:.0f} m, "
      f"{np.unpackbits(np.asarray(water[::37]), axis=1)[:, :cols].mean() * 100:.0f}% water, "
      f"elevation {int(elev[::37].min())}..{int(elev[::37].max())} m")
PY
