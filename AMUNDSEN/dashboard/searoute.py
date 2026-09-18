"""Distances from the ship to a point on the map: by air (great circle) and
by sea, the shortest walk over water on a coarse lon/lat grid.

The grid is the mask ``tools/make_sea_mask.sh`` builds from GEBCO (a byte per
cell, 1 for water) at ``UNDERWAY_SEA_MASK`` (default sea-mask.npz beside the
tile pyramid). Without the file the sea distance is simply not available.

A route is Dijkstra over the cells of a window round the two points, on 16
moves (the 8 neighbours and the 8 knight's moves, so a route bends in 22.5
degree steps rather than 45). Long routes walk a pooled grid, so the window
never holds more than ``MAX_CELLS`` cells: a pooled cell is water when any
cell in it is, which keeps straits open at the price of skipping an islet.
The answer is an estimate for planning, not a track to steer.
"""
from __future__ import annotations

import math
import os
import threading
from functools import lru_cache
from pathlib import Path

import numpy as np

MASK_PATH = Path(os.environ.get("UNDERWAY_SEA_MASK",
                                os.path.join(os.environ.get("UNDERWAY_TILES_DIR", "/data/gis/tiles"), "sea-mask.npz")))
R_KM = 6371.0088
MAX_CELLS = 200_000          # a window's cells after pooling: keeps a route under a second
MARGIN_DEG = 1.5             # the window reaches at least this far beyond the endpoints
MARGIN_FRAC = 0.4            # and at least this fraction of their span
SNAP_CELLS = 8               # a point on land moves to the nearest water within this many cells
# the moves: the 8 neighbours and the 8 knight's moves, each pair once
# (Dijkstra walks them both ways), with the cells a move passes between: a
# diagonal needs one of its two flanking cells open (no squeezing between two
# land corners), a knight's move needs both cells under its line open (no
# hopping a one-cell shore)
MOVES = [((0, 1), []), ((1, 0), []),
         ((1, 1), [(0, 1), (1, 0)]), ((1, -1), [(0, -1), (1, 0)]),
         ((1, 2), [(0, 1), (1, 1)]), ((1, -2), [(0, -1), (1, -1)]),
         ((2, 1), [(1, 0), (1, 1)]), ((2, -1), [(1, 0), (1, -1)])]


def air_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in kilometres."""
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp, dl = p2 - p1, math.radians(lon2 - lon1)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * R_KM * math.asin(min(1.0, math.sqrt(a)))


class Mask:
    """A lon/lat grid of water cells: row 0 at ``lat0`` (the south edge), column 0 at ``lon0``."""

    def __init__(self, water, lon0, lat0, dlon, dlat):
        self.water = np.asarray(water, dtype=bool)
        self.lon0, self.lat0, self.dlon, self.dlat = float(lon0), float(lat0), float(dlon), float(dlat)
        self.nrow, self.ncol = self.water.shape

    @classmethod
    def load(cls, path: Path):
        with np.load(path) as z:
            nrow, ncol = (int(v) for v in z["shape"])
            water = np.unpackbits(z["water"], axis=1, count=ncol)[:nrow]
            return cls(water, z["lon0"], z["lat0"], z["dlon"], z["dlat"])

    def cell(self, lat, lon):
        """The (row, column) holding a point, or None outside the grid."""
        i = int(math.floor((lat - self.lat0) / self.dlat))
        j = int(math.floor((lon - self.lon0) / self.dlon))
        return (i, j) if 0 <= i < self.nrow and 0 <= j < self.ncol else None


_mask: Mask | None = None
_mask_key = None
_lock = threading.Lock()


def mask() -> Mask | None:
    """The mask on disk, loaded once and again when the file changes; None without one."""
    global _mask, _mask_key
    try:
        key = (MASK_PATH, MASK_PATH.stat().st_mtime_ns)
    except OSError:
        return None
    with _lock:
        if _mask_key != key:
            _mask = Mask.load(MASK_PATH)
            _mask_key = key
            _route_cached.cache_clear()
        return _mask


def _snap(water, i, j):
    """(i, j) itself when it is water, else the nearest water cell within SNAP_CELLS; None when none."""
    if water[i, j]:
        return i, j
    best = None
    for r in range(1, SNAP_CELLS + 1):
        i0, i1 = max(0, i - r), min(water.shape[0], i + r + 1)
        j0, j1 = max(0, j - r), min(water.shape[1], j + r + 1)
        ii, jj = np.nonzero(water[i0:i1, j0:j1])
        if ii.size:
            d = (ii + i0 - i) ** 2 + (jj + j0 - j) ** 2
            n = int(np.argmin(d))
            best = (int(ii[n] + i0), int(jj[n] + j0))
            break
    return best


def _pool(water, k):
    """Any-water pooling by k: the grid shrinks by k in each direction (edges padded with land)."""
    if k == 1:
        return water
    h, w = water.shape
    H, W = -(-h // k), -(-w // k)
    padded = np.zeros((H * k, W * k), dtype=bool)
    padded[:h, :w] = water
    return padded.reshape(H, k, W, k).any(axis=(1, 3))


def _graph(water, lat_of_row, dlat_km, dlon_km_at):
    """Edges between water cells over MOVES as a sparse matrix of kilometres."""
    from scipy.sparse import coo_matrix
    H, W = water.shape
    rows, cols, wts = [], [], []
    cos = np.cos(np.radians(lat_of_row))
    for (di, dj), between in MOVES:
        i_lo, i_hi = max(0, -di), H - max(0, di)
        j_lo, j_hi = max(0, -dj), W - max(0, dj)
        if i_lo >= i_hi or j_lo >= j_hi:
            continue
        shifted = lambda a, b: water[i_lo + a:i_hi + a, j_lo + b:j_hi + b]  # noqa: E731
        here = shifted(0, 0) & shifted(di, dj)
        if between:
            passes = [shifted(a, b) for a, b in between]
            here &= (passes[0] & passes[1]) if abs(di) + abs(dj) == 3 else (passes[0] | passes[1])
        ii, jj = np.nonzero(here)
        if not ii.size:
            continue
        ii = ii + i_lo
        jj = jj + j_lo
        mean_cos = (cos[ii] + cos[ii + di]) / 2
        w = np.hypot(di * dlat_km, dj * dlon_km_at * mean_cos)
        rows.append(ii * W + jj)
        cols.append((ii + di) * W + (jj + dj))
        wts.append(w)
    n = H * W
    if not rows:
        return coo_matrix((n, n)).tocsr()
    return coo_matrix((np.concatenate(wts), (np.concatenate(rows), np.concatenate(cols))), shape=(n, n)).tocsr()


def route(lat1, lon1, lat2, lon2) -> dict:
    """Both distances from (lat1, lon1) to (lat2, lon2), and the sea route's path.

    ``sea_km`` and ``path`` are None when there is no route: ``reason`` says why.
    """
    for v in (lat1, lon1, lat2, lon2):
        if not isinstance(v, (int, float)) or not math.isfinite(v):
            raise ValueError("Coordinates must be finite numbers")
    if not (-90 <= lat1 <= 90 and -90 <= lat2 <= 90 and -180 <= lon1 <= 180 and -180 <= lon2 <= 180):
        raise ValueError("Coordinates out of range")
    out = {"air_km": round(air_km(lat1, lon1, lat2, lon2), 2), "sea_km": None, "path": None, "reason": None}
    m = mask()
    if m is None:
        out["reason"] = "no sea mask on this server"
        return out
    sea = _route_cached(round(lat1, 3), round(lon1, 3), round(lat2, 3), round(lon2, 3))
    if isinstance(sea, str):
        out["reason"] = sea
    else:
        out["sea_km"], out["path"] = sea
    return out


@lru_cache(maxsize=512)
def _route_cached(lat1, lon1, lat2, lon2):
    m = mask()
    a, b = m.cell(lat1, lon1), m.cell(lat2, lon2)
    if a is None or b is None:
        return "outside the charted area"
    if a == b:
        return round(air_km(lat1, lon1, lat2, lon2), 2), [[lat1, lon1], [lat2, lon2]]
    # first a window round the endpoints (a margin of the longer span on
    # every side, so a route may swing well off the straight line), then,
    # when that holds no route, the whole grid, pooled coarser
    span = max(abs(a[0] - b[0]) * m.dlat, abs(a[1] - b[1]) * m.dlon)      # degrees
    mi = max(MARGIN_DEG, MARGIN_FRAC * span) / m.dlat
    mj = max(MARGIN_DEG, MARGIN_FRAC * span) / m.dlon
    window = (max(0, int(min(a[0], b[0]) - mi)), min(m.nrow, int(max(a[0], b[0]) + mi) + 1),
              max(0, int(min(a[1], b[1]) - mj)), min(m.ncol, int(max(a[1], b[1]) + mj) + 1))
    found = _walk(m, (lat1, lon1), (lat2, lon2), a, b, window)
    if found == NO_ROUTE and window != (0, m.nrow, 0, m.ncol):
        found = _walk(m, (lat1, lon1), (lat2, lon2), a, b, (0, m.nrow, 0, m.ncol))
    return found


NO_ROUTE = "no sea route within the charted area"


def _walk(m, p1, p2, a, b, window):
    """The shortest water walk from cell a (holding p1) to cell b (holding p2) inside a window of the grid."""
    (lat1, lon1), (lat2, lon2) = p1, p2
    i0, i1, j0, j1 = window
    k = max(1, math.ceil(math.sqrt((i1 - i0) * (j1 - j0) / MAX_CELLS)))
    water = _pool(m.water[i0:i1, j0:j1], k)
    src = _snap(water, (a[0] - i0) // k, (a[1] - j0) // k)
    dst = _snap(water, (b[0] - i0) // k, (b[1] - j0) // k)
    if src is None:
        return "the ship's position is not on charted water"
    if dst is None:
        return "the point is on land"
    H, W = water.shape
    # a pooled cell's middle, in the full grid the window was cut from
    centre = lambda i, j: (m.lat0 + (i0 + (i + 0.5) * k) * m.dlat, m.lon0 + (j0 + (j + 0.5) * k) * m.dlon)  # noqa: E731
    lat_of_row = m.lat0 + (i0 + (np.arange(H) + 0.5) * k) * m.dlat
    dlat_km = m.dlat * k * math.pi / 180 * R_KM
    dlon_km = m.dlon * k * math.pi / 180 * R_KM
    graph = _graph(water, lat_of_row, dlat_km, dlon_km)
    from scipy.sparse.csgraph import dijkstra
    s, t = src[0] * W + src[1], dst[0] * W + dst[1]
    dist, pred = dijkstra(graph, directed=False, indices=s, return_predecessors=True,
                          limit=4 * air_km(lat1, lon1, lat2, lon2) + 400)
    if not math.isfinite(dist[t]):
        return NO_ROUTE
    # the cells walked, back from the goal, kept where the heading changes
    cells = []
    n = t
    while n >= 0 and n != s:
        cells.append(n)
        n = pred[n]
    cells.append(s)
    cells.reverse()
    pts = [[lat1, lon1]]
    prev_step = None
    for q in range(1, len(cells)):
        step = (cells[q] // W - cells[q - 1] // W, cells[q] % W - cells[q - 1] % W)
        if step != prev_step and q > 1:
            lat, lon = centre(*divmod(cells[q - 1], W))
            pts.append([round(lat, 4), round(lon, 4)])
        prev_step = step
    pts.append([lat2, lon2])
    # the graph's length, plus the walks from the points to their cells' centres
    c1, c2 = centre(*src), centre(*dst)
    total = float(dist[t]) + air_km(lat1, lon1, *c1) + air_km(lat2, lon2, *c2)
    return round(total, 2), pts
