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

Three things set the speed. The projection is conformal, so a metre on the
plane is a metre on the ground divided by the scale at that point; the speed
carries that factor, which makes arrival time a true ground distance. Water
shallower than ``SHALLOW_M`` is then slowed, and so is water within
``COAST_KM`` of land. Both are survey margins rather than keel margins: a ship
of this draft clears far less water than 100 m, but only a small share of this
coast is surveyed to modern standards, and it is the shallows and the shore
that the chart knows least about.

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
SHALLOW_SPEED = 0.3          # how much of open-water speed is left in water with no depth under it
COAST_KM = 5.0               # and water closer than this to land is held against it too
COAST_SPEED = 0.35           # how much of open-water speed is left against the shore
MARGIN_CELLS = 400           # the window reaches at least this far beyond the two points
MARGIN_FRAC = 0.45           # and at least this fraction of their separation
SNAP_CELLS = 40              # a point on land moves to the nearest water within this many cells
TRACE_STEP = 1.0             # how far, in cells, the walk back down the arrival time goes at a time
TRACE_TOLERANCE = 0.15       # how far, in cells, a drawn leg may sit from the walk it stands for
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


def _offshore(water, metres):
    """How far each water cell is from land, as a share of COAST_KM, capped at 1.

    The distance is measured inside the window; land beyond its edge is not
    seen, which the padding round the two points makes harmless.
    """
    from scipy import ndimage
    reach = COAST_KM * 1000.0 / metres
    if not (~water).any():
        return np.ones(water.shape, dtype=np.float32)
    return np.clip(ndimage.distance_transform_edt(water) / reach, 0.0, 1.0).astype(np.float32)


def _slopes(field):
    """The arrival time's gradient, one-sided where the other side is land."""
    known = np.where(np.isfinite(field), field, np.nan)
    def along(axis):
        back = known - np.roll(known, 1, axis=axis)
        fwd = np.roll(known, -1, axis=axis) - known
        edge = [slice(None)] * 2
        edge[axis] = 0
        back[tuple(edge)] = np.nan
        edge[axis] = -1
        fwd[tuple(edge)] = np.nan
        middle = (back + fwd) / 2                       # a central difference where both sides are water
        one = np.where(np.isnan(fwd), back, fwd)
        return np.nan_to_num(np.where(np.isnan(middle), one, middle))
    return along(0), along(1)


def _trace(field, water, start, goal):
    """The way down the arrival time from a point back to its source.

    Fast marching leaves a field whose gradient points back along the route,
    so the line is walked by following that gradient rather than by stepping
    from cell to cell: it bends where the route bends and runs straight where
    the route is straight. Where the gradient dies — in a channel a cell or
    two wide, or against a shore — it takes the best neighbouring cell, which
    exists while the field decreases.
    """
    rows, cols = field.shape
    gy, gx = _slopes(field)

    def sample(grid, y, x):
        i, j = int(y), int(x)
        if not (0 <= i < rows - 1 and 0 <= j < cols - 1):
            return 0.0
        fy, fx = y - i, x - j
        block = grid[i:i + 2, j:j + 2]
        return float(block[0, 0] * (1 - fy) * (1 - fx) + block[1, 0] * fy * (1 - fx)
                     + block[0, 1] * (1 - fy) * fx + block[1, 1] * fy * fx)

    def afloat(y, x):
        i, j = int(y), int(x)
        return 0 <= i < rows and 0 <= j < cols and water[i, j]

    def neighbour(y, x):
        i, j = int(y), int(x)
        best, value = None, field[i, j] if afloat(y, x) else math.inf
        for di in (-1, 0, 1):
            for dj in (-1, 0, 1):
                ii, jj = i + di, j + dj
                if 0 <= ii < rows and 0 <= jj < cols and water[ii, jj] and field[ii, jj] < value:
                    best, value = (ii + .5, jj + .5), field[ii, jj]
        return best

    here = np.array([goal[0] + .5, goal[1] + .5])
    home = np.array([start[0] + .5, start[1] + .5])
    path = [tuple(here)]
    for _ in range(8 * (rows + cols)):
        if np.hypot(*(here - home)) < 1.5:
            break
        moved = False
        for step in (TRACE_STEP, TRACE_STEP / 2, TRACE_STEP / 4):
            g = np.array([sample(gy, *here), sample(gx, *here)])
            size = math.hypot(*g)
            if size < 1e-12:
                break
            middle = here - step / 2 * g / size                      # the midpoint's heading, not this one's
            if afloat(*middle):
                g = np.array([sample(gy, *middle), sample(gx, *middle)])
                size = math.hypot(*g) or size
            ahead = here - step * g / size
            if afloat(*ahead) and _afloat(water, tuple(here), tuple(ahead)):
                here = ahead
                moved = True
                break
        if not moved:
            step = neighbour(*here)
            if step is None:
                break
            here = np.array(step)
        path.append(tuple(here))
    path.append(tuple(home))
    return path


def _afloat(water, a, b):
    """Whether the straight leg between two points stays on water."""
    steps = int(max(abs(b[0] - a[0]), abs(b[1] - a[1])) * 2) + 2
    ys = np.clip(np.linspace(a[0], b[0], steps).astype(int), 0, water.shape[0] - 1)
    xs = np.clip(np.linspace(a[1], b[1], steps).astype(int), 0, water.shape[1] - 1)
    return bool(water[ys, xs].all())


def _drawn(points, water, tolerance):
    """The walked line as legs to draw: close to the walk, and never across land.

    The tolerance is a fraction of a cell, so a route keeps the curve the
    field gave it; only the points a straight run genuinely accounts for go.
    """
    out = [points[0]]
    for a, b in zip(_simplify(points, tolerance), _simplify(points, tolerance)[1:]):
        if _afloat(water, a, b):
            out.append(b)
        else:
            out.extend(_between(points, a, b))   # the shortcut would clip a corner: keep the walk
    return out


def _between(points, a, b):
    """The walked points from a to b, b included, as they were walked."""
    try:
        i = points.index(a)
        j = points.index(b, i)
    except ValueError:
        return [b]
    return points[i + 1:j + 1]


def _simplify(points, tolerance):
    """The polyline with the points a straight line already accounts for left out."""
    if len(points) < 3:
        return list(points)
    a, b = np.asarray(points[0]), np.asarray(points[-1])
    line = b - a
    span = math.hypot(*line)
    offsets = np.asarray(points) - a
    away = (np.abs(offsets[:, 0] * line[1] - offsets[:, 1] * line[0]) / span if span
            else np.hypot(offsets[:, 0], offsets[:, 1]))
    worst = int(np.argmax(away))
    if away[worst] <= tolerance:
        return [points[0], points[-1]]
    return _simplify(points[:worst + 1], tolerance)[:-1] + _simplify(points[worst:], tolerance)


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

    # the speed: the plane's own scale, slowed in shallow water and near land
    depth = -np.asarray(g.elev[i0:i1, j0:j1], dtype=np.float32)
    shallow = np.clip(depth / SHALLOW_M, 0.0, 1.0)
    speed = g.scale(i0, i1, j0, j1).astype(np.float32) * (SHALLOW_SPEED + (1.0 - SHALLOW_SPEED) * shallow)
    speed *= COAST_SPEED + (1.0 - COAST_SPEED) * _offshore(water, g.metres)
    phi = np.ones(water.shape, dtype=np.float64)
    phi[src] = -1.0
    time = skfmm.travel_time(np.ma.MaskedArray(phi, ~water), np.ma.MaskedArray(speed, ~water),
                             dx=g.metres / 1000.0, order=2)
    field = np.asarray(np.ma.filled(time, np.inf), dtype=np.float64)   # an all-water window comes back unmasked
    if not math.isfinite(field[dst]):
        return "no sea route within the charted area"

    walked = _drawn(_trace(field, water, src, dst), water, TRACE_TOLERANCE)
    points = [g.lonlat(i0 + y - .5, j0 + x - .5) for y, x in walked][::-1]
    km = sum(air_km(*points[k], *points[k + 1]) for k in range(len(points) - 1))
    return round(km, 2), [[round(la, 4), round(lo, 4)] for la, lo in points]


if __name__ == "__main__":
    import sys
    print(json.dumps(route(*(float(v) for v in sys.argv[1:5])), indent=1))
