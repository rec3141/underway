#!/usr/bin/env bash
# Build the grid the sea-route estimate (dashboard/searoute.py) walks and
# reads depths from: a lon/lat grid over a box carrying, per cell, whether it
# holds water and what GEBCO's elevation there is.
#
#   make_sea_mask.sh ZIP OUT.npz [XMIN YMIN XMAX YMAX] [DLON DLAT]
#
# ZIP     gebco_2024_sub_ice_topo_geotiff.zip (the tile pyramid's source)
# OUT     the .npz the server reads (UNDERWAY_SEA_MASK, default
#         $UNDERWAY_TILES_DIR/sea-mask.npz)
# bbox    lon/lat, default the western Arctic and Labrador Sea
# cell    degrees, default 0.04 by 0.02 (about 1 by 2 km at 78 N)
#
# A cell is water when the lowest GEBCO sample in it is below sea level, so
# a strait one sample wide stays open; an islet narrower than a cell drops
# out, which is the right side to err on for a distance estimate. The stored
# elevation is a separate bilinear sample near the cell's middle, so a depth
# read off it is the seabed thereabouts rather than the deepest corner.
# Needs GDAL (gdalbuildvrt, gdalwarp) and numpy.
set -euo pipefail
ZIP=${1:?zip}; OUT=${2:?out.npz}
BBOX=(${3:--150} ${4:-45} ${5:--15} ${6:-86})
DLON=${7:-0.04}; DLAT=${8:-0.02}
WORK=$(mktemp -d "${TMPDIR:-/tmp}/seamask.XXXXXX"); trap 'rm -rf "$WORK"' EXIT
export GDAL_CACHEMAX=2048 GDAL_NUM_THREADS=ALL_CPUS GDAL_PAM_ENABLED=NO

# the release is eight 90x90-degree tiles; only unpack the ones the box touches
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
echo "sampling lon ${BBOX[0]}..${BBOX[2]}, lat ${BBOX[1]}..${BBOX[3]} at $DLON x $DLAT degrees"
for pass_name in min bilinear; do
  gdalwarp -q -te "${BBOX[@]}" -tr "$DLON" "$DLAT" -r "$pass_name" -ot Int16 -of ENVI -multi -wo NUM_THREADS=ALL_CPUS \
      "$WORK/global.vrt" "$WORK/$pass_name.bil"
done
python3 - "$WORK/min.bil" "$WORK/bilinear.bil" "$OUT" "${BBOX[@]}" "$DLON" "$DLAT" <<'PY'
import re, sys
import numpy as np
lowest, sampled, out, xmin, ymin, xmax, ymax, dlon, dlat = sys.argv[1], sys.argv[2], sys.argv[3], *map(float, sys.argv[4:])
def grid(path):
    hdr = open(path[:-4] + ".hdr").read()
    ncol, nrow = (int(re.search(rf"{k}\s*=\s*(\d+)", hdr).group(1)) for k in ("samples", "lines"))
    order = ">" if re.search(r"byte order\s*=\s*1", hdr) else "<"
    return np.fromfile(path, dtype=order + "i2").reshape(nrow, ncol)[::-1]   # row 0 at the south edge, like the lat axis
water = grid(lowest) < 0
elev = grid(sampled)
np.savez_compressed(out, water=np.packbits(water, axis=1), elev=elev, shape=np.array(water.shape),
                    lon0=xmin, lat0=ymin, dlon=dlon, dlat=dlat)
print(f"{out}: {water.shape[1]} x {water.shape[0]} cells, {water.mean() * 100:.0f}% water, "
      f"elevation {elev.min()}..{elev.max()} m")
PY
