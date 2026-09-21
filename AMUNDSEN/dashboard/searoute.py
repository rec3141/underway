"""What lies between the ship and a point on the map: the distance by air (a
great circle) and by sea, the route drawn on the map, and the seabed at the
point.

The grid is what ``tools/make_sea_grid.sh`` builds from GEBCO: a polar
stereographic plane (EPSG:3413, the Arctic standard) carrying, per 250 m cell,
whether it holds water and the elevation there. Both arrays are memory-mapped,
so a route touches only the window it walks. Without the grid there is no sea
distance and no depth.

The route is the fast-marching solution of the eikonal equation: arrival time
spreads from the ship through the water at a speed this module sets, and the
route to any point is the steepest way back down that field. Unlike a walk
from cell to cell it is not confined to a lattice of headings, so open water
comes out straight rather than as a staircase of 22.5 degree legs.

Two things set the speed. The projection is conformal, so a metre on the plane
is a metre on the ground divided by the scale at that point; the speed carries
that factor, which makes arrival time a true ground distance. Water shallower
than ``SHALLOW_M`` is then slowed, which holds a route off the coast. That is a
survey margin, not a keel margin: a ship of this draft clears far less water
than 100 m, but only a small share of this coast is surveyed to modern
standards, and the shallows are where the chart is least trustworthy.

The answer is an estimate for planning, not a track to steer.
"""
from __future__ import annotations

import json
import math
import os
import threading
from functools import lru_cache
from pathlib import Path

import numpy as np

GRID_DIR = Path(os.environ.get("UNDERWAY_SEA_GRID",
                               os.path.join(os.environ.get("UNDERWAY_TILES_DIR", "/data/gis/tiles"), "sea-grid")))
R_KM = 6371.0088
SHALLOW_M = 100.0            # water shallower than this is held against a route
SHALLOW_SPEED = 0.3          # how much of open-water speed is left at the shore
MARGIN_CELLS = 400           # the window reaches at least this far beyond the two points
MARGIN_FRAC = 0.45           # and at least this fraction of their separation
SNAP_CELLS = 40              # a point on land moves to the nearest water within this many cells
MAX_CELLS = 60_000_000       # a window bigger than this is refused rather than made to wait

# ---------------------------------------------------------------- the plane
# WGS84 polar stereographic, true scale at 70 N, central meridian 45 W
# (EPSG:3413; Snyder 21-33 and 21-34), matching the grid the tool builds.
A, F = 6378137.0, 1 / 298.257223563
E = math.sqrt(2 * F - F * F)
LAT_TS, LON0 = math.radians(70.0), math.radians(-45.0)


def _t(phi):
    s = math.sin(phi)
    return math.tan(math.pi / 4 - phi / 2) / ((1 - E * s) / (1 + E * s)) ** (E / 2)


def _m(phi):
    s = math.sin(phi)
    return math.cos(phi) / math.sqrt(1 - (E * s) ** 2)


SCALE = A * _m(LAT_TS) / _t(LAT_TS)


def forward(lat, lon):
    """Longitude and latitude to metres on the plane."""
    phi, lam = math.radians(lat), math.radians(lon) - LON0
    rho = SCALE * _t(phi)
    return rho * math.sin(lam), -rho * math.cos(lam)


def inverse(x, y):
    """Metres on the plane back to latitude and longitude."""
    t = math.hypot(x, y) / SCALE
    phi = math.pi / 2 - 2 * math.atan(t)
    for _ in range(8):
        s = math.sin(phi)
        phi = math.pi / 2 - 2 * math.atan(t * ((1 - E * s) / (1 + E * s)) ** (E / 2))
    return math.degrees(phi), math.degrees(LON0 + math.atan2(x, -y))


# the scale of the plane depends only on how far a point is from the pole, so
# one table over that distance serves every cell of a window
_RHO = np.linspace(0.0, 9e6, 4001)
_K = np.empty_like(_RHO)
for _i, _r in enumerate(_RHO):
    _t_r = _r / SCALE
    _phi = math.pi / 2 - 2 * math.atan(_t_r)
    for _ in range(8):
        _s = math.sin(_phi)
        _phi = math.pi / 2 - 2 * math.atan(_t_r * ((1 - E * _s) / (1 + E * _s)) ** (E / 2))
    _K[_i] = _r / (A * _m(_phi)) if _r else _m(LAT_TS) / _t(LAT_TS) * 0 + SCALE / A * _t(math.pi / 2 - 1e-9) / 1e-9 * 0 + 1.0
_K[0] = _K[1]                                    # at the pole itself the ratio is a limit, not a quotient


def air_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in kilometres."""
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp, dl = p2 - p1, math.radians(lon2 - lon1)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * R_KM * math.asin(min(1.0, math.sqrt(a)))


class Grid:
    """The water and elevation arrays on the plane, memory-mapped."""

    def __init__(self, directory: Path):
        head = json.loads((directory / "grid.json").read_text())
        self.x0, self.y0, self.metres = head["x0"], head["y0"], head["metres"]
        self.rows, self.cols = head["rows"], head["cols"]
        self.source = head.get("source", "")
        self.elev = np.load(directory / "elevation.npy", mmap_mode="r")
        self.packed = np.load(directory / "water.npy", mmap_mode="r")

    def cell(self, lat, lon):
        """The (row, column) holding a point, or None off the grid."""
        x, y = forward(lat, lon)
        j = int((x - self.x0) / self.metres)
        i = int((self.y0 - y) / self.metres)
        return (i, j) if 0 <= i < self.rows and 0 <= j < self.cols else None

    def lonlat(self, i, j):
        """The latitude and longitude at the middle of a cell, row and column being floats."""
        return inverse(self.x0 + (j + 0.5) * self.metres, self.y0 - (i + 0.5) * self.metres)

    def water(self, i0, i1, j0, j1):
        """The water flags of a window, unpacked."""
        return np.unpackbits(np.asarray(self.packed[i0:i1]), axis=1, count=self.cols)[:, j0:j1].astype(bool)

    def elevation(self, lat, lon):
        """GEBCO's elevation in metres at a point (negative below sea level), or None off the grid."""
        cell = self.cell(lat, lon)
        return None if cell is None else int(self.elev[cell])

    def scale(self, i0, i1, j0, j1):
        """The plane's scale over a window: a metre here is this many metres on the plane."""
        x = self.x0 + (np.arange(j0, j1) + 0.5) * self.metres
        y = self.y0 - (np.arange(i0, i1) + 0.5) * self.metres
        return np.interp(np.hypot(x[None, :], y[:, None]), _RHO, _K)


_grid: Grid | None = None
_grid_key = None
_lock = threading.Lock()


def grid() -> Grid | None:
    """The grid on disk, loaded once and again when it changes; None without one."""
    global _grid, _grid_key
    try:
        key = (GRID_DIR, (GRID_DIR / "grid.json").stat().st_mtime_ns)
    except OSError:
        return None
    with _lock:
        if _grid_key != key:
            _grid = Grid(GRID_DIR)
            _grid_key = key
            _route_cached.cache_clear()
        return _grid


def _check(*values):
    for v in values:
        if not isinstance(v, (int, float)) or not math.isfinite(v):
            raise ValueError("Coordinates must be finite numbers")
    for lat, lon in zip(values[::2], values[1::2]):
        if not (-90 <= lat <= 90 and -180 <= lon <= 180):
            raise ValueError("Coordinates out of range")


def place(lat, lon) -> dict:
    """What is known about a point on its own: the elevation of the ground there."""
    _check(lat, lon)
    g = grid()
    return {"elev_m": None if g is None else g.elevation(lat, lon)}


def route(lat1, lon1, lat2, lon2) -> dict:
    """Both distances from (lat1, lon1) to (lat2, lon2), the sea route's path, and the point's elevation.

    ``sea_km`` and ``path`` are None when there is no route: ``reason`` says why.
    """
    _check(lat1, lon1, lat2, lon2)
    out = {"air_km": round(air_km(lat1, lon1, lat2, lon2), 2), "sea_km": None, "path": None, "reason": None, "elev_m": None}
    g = grid()
    if g is None:
        out["reason"] = "no sea grid on this server"
        return out
    out["elev_m"] = g.elevation(lat2, lon2)
    sea = _route_cached(round(lat1, 3), round(lon1, 3), round(lat2, 3), round(lon2, 3))
    if isinstance(sea, str):
        out["reason"] = sea
    else:
        out["sea_km"], out["path"] = sea
    return out


def _snap(water, i, j):
    """(i, j) itself when it is water, else the nearest water cell within SNAP_CELLS; None when none."""
    if water[i, j]:
        return i, j
    for r in range(4, SNAP_CELLS + 1, 4):
        i0, i1 = max(0, i - r), min(water.shape[0], i + r + 1)
        j0, j1 = max(0, j - r), min(water.shape[1], j + r + 1)
        ii, jj = np.nonzero(water[i0:i1, j0:j1])
        if ii.size:
            d = (ii + i0 - i) ** 2 + (jj + j0 - j) ** 2
            n = int(np.argmin(d))
            return int(ii[n] + i0), int(jj[n] + j0)
    return None


def _clear(water, a, b):
    """Whether the straight segment between two cell centres stays on water."""
    (y0, x0), (y1, x1) = a, b
    steps = int(max(abs(y1 - y0), abs(x1 - x0)) * 2) + 1
    ys = np.linspace(y0, y1, steps)
    xs = np.linspace(x0, x1, steps)
    return bool(water[np.clip(ys.astype(int), 0, water.shape[0] - 1), np.clip(xs.astype(int), 0, water.shape[1] - 1)].all())


def _pull(water, points):
    """The polyline with every point a straight run of water can skip taken out."""
    out = [points[0]]
    i = 0
    while i < len(points) - 1:
        j = len(points) - 1
        while j > i + 1 and not _clear(water, points[i], points[j]):
            j -= 1
        out.append(points[j])
        i = j
    return out


@lru_cache(maxsize=256)
def _route_cached(lat1, lon1, lat2, lon2):
    g = grid()
    a, b = g.cell(lat1, lon1), g.cell(lat2, lon2)
    if a is None or b is None:
        return "outside the charted area"
    try:
        import skfmm
    except ImportError:
        return "no router on this server"
    span = max(abs(a[0] - b[0]), abs(a[1] - b[1]))
    pad = int(max(MARGIN_CELLS, MARGIN_FRAC * span))
    i0, i1 = max(0, min(a[0], b[0]) - pad), min(g.rows, max(a[0], b[0]) + pad + 1)
    j0, j1 = max(0, min(a[1], b[1]) - pad), min(g.cols, max(a[1], b[1]) + pad + 1)
    if (i1 - i0) * (j1 - j0) > MAX_CELLS:
        return "too far for this server to work out"
    water = g.water(i0, i1, j0, j1)
    src = _snap(water, a[0] - i0, a[1] - j0)
    dst = _snap(water, b[0] - i0, b[1] - j0)
    if src is None:
        return "the ship's position is not on charted water"
    if dst is None:
        return "the point is on land"

    # the speed: the plane's own scale, slowed where the water is shallow
    depth = -np.asarray(g.elev[i0:i1, j0:j1], dtype=np.float32)
    shallow = np.clip(depth / SHALLOW_M, 0.0, 1.0)
    speed = g.scale(i0, i1, j0, j1).astype(np.float32) * (SHALLOW_SPEED + (1.0 - SHALLOW_SPEED) * shallow)
    phi = np.ones(water.shape, dtype=np.float64)
    phi[src] = -1.0
    time = skfmm.travel_time(np.ma.MaskedArray(phi, ~water), np.ma.MaskedArray(speed, ~water),
                             dx=g.metres / 1000.0, order=2)
    field = np.asarray(np.ma.filled(time, np.inf), dtype=np.float64)   # an all-water window comes back unmasked
    if not math.isfinite(field[dst]):
        return "no sea route within the charted area"

    cells = _descend(field, water, dst, src)
    pulled = _pull(water, cells)
    points = [g.lonlat(i0 + i, j0 + j) for i, j in pulled][::-1]
    points[0] = (lat1, lon1) if water[src] and _clear(water, (a[0] - i0, a[1] - j0), src) else points[0]
    points[-1] = (lat2, lon2) if _clear(water, (b[0] - i0, b[1] - j0), dst) else points[-1]
    km = sum(air_km(*points[k], *points[k + 1]) for k in range(len(points) - 1))
    return round(km, 2), [[round(la, 4), round(lo, 4)] for la, lo in points]


def _descend(field, water, start, goal):
    """The cells from the goal back down the arrival time to its source."""
    rows, cols = field.shape
    path = [start]
    i, j = start
    seen = set()
    for _ in range(4 * (rows + cols)):
        if (i, j) == goal:
            break
        best, value = None, field[i, j]
        for di in (-1, 0, 1):
            for dj in (-1, 0, 1):
                ii, jj = i + di, j + dj
                if 0 <= ii < rows and 0 <= jj < cols and water[ii, jj] and field[ii, jj] < value and (ii, jj) not in seen:
                    best, value = (ii, jj), field[ii, jj]
        if best is None:
            break
        seen.add(best)
        i, j = best
        path.append(best)
    if path[-1] != goal:
        path.append(goal)
    return path


if __name__ == "__main__":
    import sys
    print(json.dumps(route(*(float(v) for v in sys.argv[1:5])), indent=1))
