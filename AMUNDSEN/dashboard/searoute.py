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

A window too wide to solve cell by cell is solved in blocks instead, so a
route across the whole grid comes back loosely rather than not at all. The
blocks carry the rule the grid was built by: a block holds water when any
cell in it does, and takes the lowest elevation in it. That keeps a channel a
cell wide open, at the price of a line that cuts corners by about a block.

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
SHALLOW_SPEED = 0.6          # how much of open-water speed is left in water with no depth under it
COAST_KM = 5.0               # and water closer than this to land is held against it too
COAST_SPEED = 0.6            # how much of open-water speed is left against the shore
MARGIN_CELLS = 400           # the window reaches at least this far beyond the two points
MARGIN_FRAC = 0.45           # and at least this fraction of their separation
MARGIN_TRIES = 5             # a window with no route in it is widened this many times before giving up
SNAP_CELLS = 40              # a point on land moves to the nearest water within this many cells
TRACE_STEP = 1.0             # how far, in cells, the walk back down the arrival time goes at a time
TRACE_TOLERANCE = 0.15       # how far, in cells, a drawn leg may sit from the walk it stands for
DRAW_LEG_CELLS = 400         # and how much walking one drawn leg may stand for, in cells
MAX_CELLS = 60_000_000       # the most one solve works over; a wider window is coarsened to fit
MAX_STEP = 8                 # and this is as coarse as a solve is allowed to get, in cells a side
BAND_CELLS = 8_000_000       # how much of a window is read at a time while it is being coarsened

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


# What GEBCO says a depth rests on, by its Type Identifier. Only the surveyed
# kinds are ground truth; the rest is filled in between soundings or guessed
# from satellite gravity, and in this water that is about three quarters of it.
KINDS = {10: ("singlebeam", True), 11: ("multibeam", True), 12: ("seismic", True), 13: ("soundings", True),
         14: ("chart sounding", True), 15: ("lidar", True), 16: ("from imagery", True), 17: ("surveys combined", True),
         40: ("predicted from gravity", False), 41: ("interpolated", False), 42: ("depth model", False),
         43: ("unknown", False), 44: ("unknown", False), 45: ("from imagery", False),
         70: ("pre-gridded", False), 71: ("pre-gridded", False), 72: ("pre-gridded", False), 90: ("steering points", False)}
SURVEYED = {code for code, (_, measured) in KINDS.items() if measured}
FOLLOW = 1.35                # how much faster a route reckons surveyed water
FOLLOW_KM = 2.0              # and how far its credit reaches from the swath


def _blocks(a, step, reduce):
    """An array with every ``step`` by ``step`` block reduced to one value.

    A window rarely divides evenly, so the last row and column of blocks are
    filled out by repeating the edge, which no reduction is disturbed by.
    """
    if step == 1:
        return a
    down, right = (-a.shape[0]) % step, (-a.shape[1]) % step
    if down or right:
        a = np.pad(a, ((0, down), (0, right)), mode="edge")
    a = a.reshape(a.shape[0] // step, step, a.shape[1] // step, step)
    return reduce(reduce(a, axis=3), axis=1)


class Grid:
    """The water, elevation and provenance arrays on the plane, memory-mapped."""

    def __init__(self, directory: Path):
        head = json.loads((directory / "grid.json").read_text())
        self.x0, self.y0, self.metres = head["x0"], head["y0"], head["metres"]
        self.rows, self.cols = head["rows"], head["cols"]
        self.source = head.get("source", "")
        self.elev = np.load(directory / "elevation.npy", mmap_mode="r")
        self.packed = np.load(directory / "water.npy", mmap_mode="r")
        kinds = directory / "source.npy"
        self.kinds = np.load(kinds, mmap_mode="r") if kinds.exists() else None

    def cell(self, lat, lon):
        """The (row, column) holding a point, or None off the grid."""
        x, y = forward(lat, lon)
        j = int((x - self.x0) / self.metres)
        i = int((self.y0 - y) / self.metres)
        return (i, j) if 0 <= i < self.rows and 0 <= j < self.cols else None

    def lonlat(self, i, j):
        """The latitude and longitude at the middle of a cell, row and column being floats."""
        return inverse(self.x0 + (j + 0.5) * self.metres, self.y0 - (i + 0.5) * self.metres)

    def window(self, i0, i1, j0, j1, step=1):
        """A window's water flags and elevation, in blocks of ``step`` cells.

        A block holds water when any cell in it does and takes the lowest
        elevation in it: the rule the grid itself was built by, one level up.
        The window is read a band of rows at a time, so coarsening a wide one
        costs the band rather than the whole of it.
        """
        shape = (-(-(i1 - i0) // step), -(-(j1 - j0) // step))
        water = np.empty(shape, dtype=bool)
        elev = np.empty(shape, dtype=np.float32)
        band = max(1, BAND_CELLS // max(1, j1 - j0) // step)      # in blocks of rows
        for k in range(0, shape[0], band):
            lo, hi = i0 + k * step, min(i1, i0 + (k + band) * step)
            rows = -(-(hi - lo) // step)
            wet = np.unpackbits(np.asarray(self.packed[lo:hi]), axis=1, count=self.cols)[:, j0:j1].astype(bool)
            water[k:k + rows] = _blocks(wet, step, np.max)
            elev[k:k + rows] = _blocks(np.asarray(self.elev[lo:hi, j0:j1], dtype=np.float32), step, np.min)
        return water, elev

    def surveyed(self, i0, i1, j0, j1, step=1):
        """Which of a window's cells rest on a measured sounding; None without the grid for it."""
        if self.kinds is None:
            return None
        shape = (-(-(i1 - i0) // step), -(-(j1 - j0) // step))
        out = np.empty(shape, dtype=bool)
        band = max(1, BAND_CELLS // max(1, j1 - j0) // step)
        for k in range(0, shape[0], band):
            lo, hi = i0 + k * step, min(i1, i0 + (k + band) * step)
            known = np.isin(np.asarray(self.kinds[lo:hi, j0:j1]), list(SURVEYED))
            out[k:k + -(-(hi - lo) // step)] = _blocks(known, step, np.max)
        return out

    def wet(self, i, j):
        """Whether one cell of the full grid holds water, read as the one bit it is."""
        return bool(self.packed[i, j >> 3] >> (7 - (j & 7)) & 1)

    def elevation(self, lat, lon):
        """GEBCO's elevation in metres at a point (negative below sea level), or None off the grid."""
        cell = self.cell(lat, lon)
        return None if cell is None else int(self.elev[cell])

    def kind(self, lat, lon):
        """What the depth at a point rests on: (code, name, whether it was surveyed), or None."""
        cell = self.cell(lat, lon)
        if self.kinds is None or cell is None:
            return None
        code = int(self.kinds[cell])
        name, measured = KINDS.get(code, ("unknown", False))
        return {"code": code, "name": name, "surveyed": measured}

    def scale(self, i0, i1, j0, j1, step=1):
        """The plane's scale over a window: a metre here is this many metres on the plane."""
        x = self.x0 + (np.arange(j0, j1, step) + step / 2) * self.metres
        y = self.y0 - (np.arange(i0, i1, step) + step / 2) * self.metres
        return np.interp(np.hypot(x[None, :], y[:, None]), _RHO, _K)


_grid: Grid | None = None
_grid_key = None
_lock = threading.Lock()
# The widest solve allowed holds several gigabytes while it runs, so only one
# runs at a time: the server answers every other request while it waits.
_solving = threading.Lock()


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
    """What is known about a point on its own: the ground there, and what that rests on."""
    _check(lat, lon)
    g = grid()
    return {"elev_m": None if g is None else g.elevation(lat, lon),
            "kind": None if g is None else g.kind(lat, lon)}


def route(lat1, lon1, lat2, lon2) -> dict:
    """Both distances from (lat1, lon1) to (lat2, lon2), the sea route's path, and the point's elevation.

    ``sea_km`` and ``path`` are None when there is no route: ``reason`` says
    why. ``cell_km`` is how coarse the solve that found it was, which is the
    grid's own 250 m unless the two points were far enough apart to need
    blocks.
    """
    _check(lat1, lon1, lat2, lon2)
    out = {"air_km": round(air_km(lat1, lon1, lat2, lon2), 2), "sea_km": None, "path": None, "reason": None,
           "elev_m": None, "kind": None, "cell_km": None}
    g = grid()
    if g is None:
        out["reason"] = "no sea grid on this server"
        return out
    out["elev_m"] = g.elevation(lat2, lon2)
    out["kind"] = g.kind(lat2, lon2)
    sea = _route_cached(round(lat1, 3), round(lon1, 3), round(lat2, 3), round(lon2, 3))
    if isinstance(sea, str):
        out["reason"] = sea
    else:
        out["sea_km"], out["path"], out["cell_km"] = sea
    return out


def _snap(water, i, j, limit=SNAP_CELLS):
    """(i, j) itself when it is water, else the nearest water cell within ``limit``; None when none."""
    if water[i, j]:
        return i, j
    for r in range(4, limit + 1, 4):
        i0, i1 = max(0, i - r), min(water.shape[0], i + r + 1)
        j0, j1 = max(0, j - r), min(water.shape[1], j + r + 1)
        ii, jj = np.nonzero(water[i0:i1, j0:j1])
        if ii.size:
            d = (ii + i0 - i) ** 2 + (jj + j0 - j) ** 2
            n = int(np.argmin(d))
            return int(ii[n] + i0), int(jj[n] + j0)
    return None


def _followed(g, i0, i1, j0, j1, step=1):
    """How much faster each cell counts for having been surveyed.

    Almost all the surveyed water here is ship tracks, and they join up: the
    largest run of them holds most of the surveyed water in the archipelago.
    Crediting a swath and the couple of kilometres either side of it lets a
    route follow those tracks where they go its way, without hunting for them.
    """
    surveyed = g.surveyed(i0, i1, j0, j1, step)
    if surveyed is None or not surveyed.any():
        return np.float32(1.0)
    from scipy import ndimage
    reach = FOLLOW_KM * 1000.0 / (g.metres * step)
    near = np.clip(1.0 - ndimage.distance_transform_edt(~surveyed) / reach, 0.0, 1.0)
    return (1.0 + (FOLLOW - 1.0) * near).astype(np.float32)


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
    arrived = False
    for _ in range(8 * (rows + cols)):
        if np.hypot(*(here - home)) < 1.5:
            arrived = True
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
    if not arrived:
        return None                              # the caller falls back to the walk that cannot fail
    path.append(tuple(home))
    return path


def _crawl(field, water, start, goal):
    """The cells from the goal back to the source, one neighbour at a time.

    Fast marching leaves a field that strictly decreases towards its source
    over the water it reached, so a step down always exists: this arrives
    wherever following the gradient wanders off, at the price of a staircase
    that the drawing then smooths.
    """
    rows, cols = field.shape
    path = [(goal[0] + .5, goal[1] + .5)]
    i, j = goal
    for _ in range(4 * (rows + cols)):
        if (i, j) == start:
            return path + [(start[0] + .5, start[1] + .5)]
        best, value = None, field[i, j]
        for di in (-1, 0, 1):
            for dj in (-1, 0, 1):
                ii, jj = i + di, j + dj
                if 0 <= ii < rows and 0 <= jj < cols and water[ii, jj] and field[ii, jj] < value:
                    best, value = (ii, jj), field[ii, jj]
        if best is None:
            return None
        i, j = best
        path.append((i + .5, j + .5))
    return None


def _afloat(water, a, b):
    """Whether the straight leg between two points stays on water."""
    steps = int(max(abs(b[0] - a[0]), abs(b[1] - a[1])) * 2) + 2
    ys = np.clip(np.linspace(a[0], b[0], steps).astype(int), 0, water.shape[0] - 1)
    xs = np.clip(np.linspace(a[1], b[1], steps).astype(int), 0, water.shape[1] - 1)
    return bool(water[ys, xs].all())


def _drawn(points, water, tolerance, leg=DRAW_LEG_CELLS):
    """The walked line as legs to draw: close to the walk, and never across land.

    The tolerance is a fraction of a cell, so a route keeps the curve the
    field gave it; only the points a straight run genuinely accounts for go.
    A leg may stand for at most ``leg`` cells of walking, so a stretch the
    water mask is wrong about cannot turn into one long jump.
    """
    walk = _spaced(points)
    simple = _simplify(walk, tolerance)
    out = [simple[0]]
    for a, b in zip(simple, simple[1:]):
        if _afloat(water, a, b) and math.hypot(b[0] - a[0], b[1] - a[1]) <= leg:
            out.append(b)
        else:
            out.extend(_between(walk, a, b))     # the shortcut is not safe: keep the walk
    return out


def _spaced(points):
    """The walk with the points it stood still for taken out."""
    out = [points[0]]
    for p in points[1:]:
        if math.hypot(p[0] - out[-1][0], p[1] - out[-1][1]) >= TRACE_STEP / 2:
            out.append(p)
    if out[-1] != points[-1]:
        out.append(points[-1])
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


def _dry(g, a, b):
    """Whether the leg between two points crosses land the full grid knows about."""
    (x1, y1), (x2, y2) = forward(*a), forward(*b)
    steps = max(3, int(2 * math.hypot(x2 - x1, y2 - y1) / g.metres) + 1)
    for t in np.linspace(0.0, 1.0, steps):
        j = int((x1 + (x2 - x1) * t - g.x0) / g.metres)
        i = int((g.y0 - (y1 + (y2 - y1) * t)) / g.metres)
        if 0 <= i < g.rows and 0 <= j < g.cols and not g.wet(i, j):
            return True
    return False


def _mend(g, points):
    """A route with the legs the full grid calls dry solved again at full resolution.

    A block counts as water when any cell in it does, which keeps a channel a
    cell wide open but also dissolves an isthmus narrower than a block. A leg
    is short, so solving one again costs a small window, and a route over open
    water has no such leg to solve.
    """
    out = [points[0]]
    for a, b in zip(points, points[1:]):
        out.extend(_leg(g, a, b) or [b])
    return out


def _leg(g, a, b):
    """One leg solved cell by cell, or None when it is afloat already or cannot be."""
    if not _dry(g, a, b):
        return None
    ca, cb = g.cell(*a), g.cell(*b)
    if ca is None or cb is None:
        return None
    pad = MARGIN_CELLS
    for _ in range(MARGIN_TRIES):
        i0, i1 = max(0, min(ca[0], cb[0]) - pad), min(g.rows, max(ca[0], cb[0]) + pad + 1)
        j0, j1 = max(0, min(ca[1], cb[1]) - pad), min(g.cols, max(ca[1], cb[1]) + pad + 1)
        if (i1 - i0) * (j1 - j0) > MAX_CELLS:
            return None
        with _solving:
            answer = _solve(g, a, b, ca, cb, (i0, i1, j0, j1))
        if not isinstance(answer, str):
            return answer[1][1:]
        if answer != "no sea route within the charted area":
            return None
        pad *= 2
    return None


def _step(cells):
    """The block a window of this many cells has to be solved in; None when even
    the coarsest allowed is too much."""
    step = max(1, math.isqrt(max(0, cells - 1) // MAX_CELLS) + 1) if cells > MAX_CELLS else 1
    while step <= MAX_STEP and cells > MAX_CELLS * step * step:
        step += 1
    return step if step <= MAX_STEP else None


@lru_cache(maxsize=256)
def _route_cached(lat1, lon1, lat2, lon2, mend=True):
    g = grid()
    a, b = g.cell(lat1, lon1), g.cell(lat2, lon2)
    if a is None or b is None:
        return "outside the charted area"
    try:
        import skfmm                                          # noqa: F401
    except ImportError:
        return "no router on this server"
    # A route may have to leave the box the two points make by a long way:
    # Jones Sound to Norwegian Bay is a few hundred kilometres apart and a
    # thousand around. When the window holds no route, it is widened and tried
    # again rather than reported as none.
    span = max(abs(a[0] - b[0]), abs(a[1] - b[1]))
    pad = int(max(MARGIN_CELLS, MARGIN_FRAC * span))
    answer = "no sea route within the charted area"
    for _ in range(MARGIN_TRIES):
        i0, i1 = max(0, min(a[0], b[0]) - pad), min(g.rows, max(a[0], b[0]) + pad + 1)
        j0, j1 = max(0, min(a[1], b[1]) - pad), min(g.cols, max(a[1], b[1]) + pad + 1)
        # a window wider than one solve works over is solved in blocks: a long
        # route is mostly open water, where a block costs it little
        step = _step((i1 - i0) * (j1 - j0))
        if step is None:
            return answer if answer != "no sea route within the charted area" else "too far for this server to work out"
        with _solving:
            answer = _solve(g, (lat1, lon1), (lat2, lon2), a, b, (i0, i1, j0, j1), step)
        if isinstance(answer, str):
            if answer != "no sea route within the charted area":
                return answer
        else:
            if mend and step > 1:
                points = _mend(g, answer[1])
                km = sum(air_km(*points[k], *points[k + 1]) for k in range(len(points) - 1))
                answer = round(km, 2), points, answer[2]
            return answer
        whole = i0 == 0 and j0 == 0 and i1 == g.rows and j1 == g.cols
        if whole:
            return answer
        pad *= 2
    return answer


def _solve(g, from_ll, to_ll, a, b, window, step=1):
    """One fast-marching solve over a window of the grid, in blocks of ``step`` cells."""
    import skfmm
    (lat1, lon1), (lat2, lon2) = from_ll, to_ll
    i0, i1, j0, j1 = window
    metres = g.metres * step
    snap = max(4, round(SNAP_CELLS / step))
    water, ground = g.window(i0, i1, j0, j1, step)
    src = _snap(water, (a[0] - i0) // step, (a[1] - j0) // step, snap)
    dst = _snap(water, (b[0] - i0) // step, (b[1] - j0) // step, snap)
    if src is None:
        return "the ship's position is not on charted water"
    if dst is None:
        return "the point is on land"

    # the speed: the plane's own scale, slowed in shallow water and near land
    shallow = np.clip(-ground / SHALLOW_M, 0.0, 1.0)
    speed = g.scale(i0, i1, j0, j1, step).astype(np.float32) * (SHALLOW_SPEED + (1.0 - SHALLOW_SPEED) * shallow)
    speed *= COAST_SPEED + (1.0 - COAST_SPEED) * _offshore(water, metres)
    speed *= _followed(g, i0, i1, j0, j1, step)
    phi = np.ones(water.shape, dtype=np.float64)
    phi[src] = -1.0
    time = skfmm.travel_time(np.ma.MaskedArray(phi, ~water), np.ma.MaskedArray(speed, ~water),
                             dx=metres / 1000.0, order=2)
    field = np.asarray(np.ma.filled(time, np.inf), dtype=np.float64)   # an all-water window comes back unmasked
    if not math.isfinite(field[dst]):
        return "no sea route within the charted area"

    steps = _trace(field, water, src, dst) or _crawl(field, water, src, dst)
    if steps is None:
        return "no sea route within the charted area"
    walked = _drawn(steps, water, TRACE_TOLERANCE, max(4, round(DRAW_LEG_CELLS / step)))
    # the line reaches the ship and the mark themselves, but only where the
    # water does: a point snapped across a headland keeps the walk's own end
    # the bridge stands for the snap to water and nothing more: a long one
    # would be a leap across whatever the mask is wrong about
    ends = [((b[0] - i0) // step + .5, (b[1] - j0) // step + .5),
            ((a[0] - i0) // step + .5, (a[1] - j0) // step + .5)]
    for k, end in ((0, ends[0]), (-1, ends[1])):
        reach = math.hypot(end[0] - walked[k][0], end[1] - walked[k][1])
        if reach <= 2 * snap and _afloat(water, end, walked[k]):
            walked[k] = end
    # back to the plane: a block's middle sits half a block into it
    points = [g.lonlat(i0 + (y - .5) * step + (step - 1) / 2, j0 + (x - .5) * step + (step - 1) / 2)
              for y, x in walked][::-1]
    km = sum(air_km(*points[k], *points[k + 1]) for k in range(len(points) - 1))
    return round(km, 2), [[round(la, 4), round(lo, 4)] for la, lo in points], round(metres / 1000.0, 3)


if __name__ == "__main__":
    import sys
    print(json.dumps(route(*(float(v) for v in sys.argv[1:5])), indent=1))
