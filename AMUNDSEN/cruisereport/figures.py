"""Report figures as PNG, drawn offline with matplotlib.

* ``station_map``  the leg's track, the selected operations by instrument, land
  and bathymetry from the underway dashboard's Natural Earth GeoJSON, on a
  Lambert azimuthal equal-area projection centred on the stations (a plain
  lat/lon plot is stretched fourfold at 76°N).
* ``profiles``     CTD downcasts, one panel per variable, casts shaded light to
  dark in time order (one hue; the station names go in the caption table).
* ``ts_diagram``   bottle-level temperature against salinity, coloured by depth.
* ``underway``     stacked panels (never two y-axes) of the ship's record through
  the selected period, with the operations marked.

Colours are the dataviz reference palette's light-mode steps: categorical slots
in fixed order for instruments (marker shape doubles the encoding), the blue
ramp for anything ordered.
"""

from __future__ import annotations

import io
import json
import math
import sqlite3
from functools import lru_cache

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.collections import LineCollection, PolyCollection  # noqa: E402
from matplotlib.colors import LinearSegmentedColormap  # noqa: E402
from pyproj import Transformer  # noqa: E402

from . import activities, ctd, underway  # noqa: E402
from .config import GEO_DIR, SHIP_TZ  # noqa: E402

CATEGORICAL = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7",
               "#e34948"]
MARKERS = ["o", "s", "^", "D", "v", "P", "X", "h"]
# A log's points: open ink shapes, one shape per log, so they never take an
# instrument's colour; a white ring keeps them readable over the track.
LOG_MARKERS = ["o", "s", "D", "^", "v", "p"]
BLUES = LinearSegmentedColormap.from_list(
    "blues", ["#b7d3f6", "#6da7ec", "#2a78d6", "#1c5cab", "#0d366b"])
INK, INK2, GRID = "#0b0b0b", "#52514e", "#e4e3df"
LAND, LAND_EDGE, WATER = "#e9e7e1", "#b9b7af", "#fcfcfb"
DPI = 200
# The ship track by time: dark purple (start of the leg) to orange (now), stopping
# short of plasma's pale yellow, which vanishes against the water.
TRACK_CMAP = LinearSegmentedColormap.from_list("track", plt.cm.plasma(np.linspace(0.0, 0.82, 64)))

plt.rcParams.update({
    "font.family": "DejaVu Sans", "font.size": 8.5, "axes.edgecolor": INK2,
    "axes.labelcolor": INK, "xtick.color": INK2, "ytick.color": INK2, "axes.linewidth": 0.6,
    "axes.spines.top": False, "axes.spines.right": False, "legend.frameon": False,
})


def _png(fig) -> bytes:
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=DPI, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    return buf.getvalue()


def _empty(msg: str) -> bytes:
    fig, ax = plt.subplots(figsize=(6, 1.2))
    ax.axis("off")
    ax.text(0.5, 0.5, msg, ha="center", va="center", color=INK2)
    return _png(fig)


# --- map ----------------------------------------------------------------------

@lru_cache(maxsize=8)
def _geo(name: str) -> list[tuple[list, dict]]:
    """(list of rings as Nx2 lon/lat arrays, properties) per feature."""
    data = json.loads((GEO_DIR / name).read_text())
    out = []
    for f in data["features"]:
        g = f["geometry"]
        if g["type"] == "Polygon":
            rings = [np.asarray(g["coordinates"][0])]
        elif g["type"] == "MultiPolygon":
            rings = [np.asarray(p[0]) for p in g["coordinates"]]
        elif g["type"] == "LineString":
            rings = [np.asarray(g["coordinates"])]
        elif g["type"] == "Point":
            rings = [np.asarray([g["coordinates"]])]
        else:
            continue
        out.append((rings, f.get("properties") or {}))
    return out


def _track(leg: str, t0: str | None = None, t1: str | None = None, step: int = 30) -> np.ndarray:
    """Ship positions, every ``step``-th record (5 min): rows of lon, lat, epoch seconds."""
    p = underway.db_path(leg)
    if not p.is_file():
        return np.empty((0, 3))
    con = sqlite3.connect(f"file:{p}?mode=ro", uri=True)
    try:
        cols = dict(con.execute("SELECT key, col FROM columns").fetchall())
        la, lo = cols.get("posmv — latitude (deg n)"), cols.get("posmv — longitude (deg e)")
        if not la or not lo:
            return np.empty((0, 3))
        q = f"SELECT {lo}, {la}, t FROM obs WHERE rowid % {step} = 0"
        args: list = []
        if t0 and t1:
            q += " AND t BETWEEN ? AND ?"
            args = [int(pd.Timestamp(t0).tz_localize("UTC").timestamp()),
                    int(pd.Timestamp(t1).tz_localize("UTC").timestamp())]
        arr = np.array(con.execute(q + " ORDER BY t", args).fetchall(), dtype=float)
    finally:
        con.close()
    return arr[np.isfinite(arr).all(axis=1)] if len(arr) else np.empty((0, 3))


def ship_track(leg: str) -> np.ndarray:
    """The leg's ship positions every minute or so: rows of lon, lat, epoch seconds."""
    return _track(leg, step=6)


def station_map(leg: str, rows: list[dict], *, label_stations: bool = True, colour: str = "time",
                logs: list[dict] | None = None) -> bytes:
    """The selected operations and the points of any ``logs`` ({"label",
    "points": [(lon, lat, sample id or None)]}) over the leg's whole ship
    track; the view takes in all of them. The track is coloured by date, or by
    ``colour``, an underway panel's values along it (grey where it has none).
    A log's sample ids label its points, one label per place."""
    logs = [lg for lg in logs or [] if lg["points"]]
    pts = [(r["lon"], r["lat"]) for r in rows if r.get("lat") is not None] + \
        [(x, y) for lg in logs for x, y, _ in lg["points"]]
    if not pts:
        return _empty("No positions for the selected operations or logs.")
    lon0 = float(np.median([p[0] for p in pts]))
    lat0 = float(np.median([p[1] for p in pts]))
    tr = Transformer.from_crs("EPSG:4326",
                              f"+proj=laea +lat_0={lat0} +lon_0={lon0} +units=km", always_xy=True)
    trk = _track(leg)
    txy = np.column_stack(tr.transform(trk[:, 0], trk[:, 1])) if len(trk) else np.empty((0, 2))
    xy = np.vstack([np.array([tr.transform(*p) for p in pts]), txy])
    span = max(np.ptp(xy[:, 0]), np.ptp(xy[:, 1]), 40.0)
    pad = span * 0.12 + 10
    x0, x1 = xy[:, 0].min() - pad, xy[:, 0].max() + pad
    y0, y1 = xy[:, 1].min() - pad, xy[:, 1].max() + pad
    # Lon/lat box that covers the view, to skip transforming far-away features.
    corners = [tr.transform(x, y, direction="INVERSE") for x in (x0, x1, (x0 + x1) / 2)
               for y in (y0, y1, (y0 + y1) / 2)]
    blon = (min(c[0] for c in corners) - 5, max(c[0] for c in corners) + 5)
    blat = (min(c[1] for c in corners) - 2, max(c[1] for c in corners) + 2)

    def proj_rings(name, filt=None):
        out = []
        for rings, props in _geo(name):
            if filt and not filt(props):
                continue
            for ring in rings:
                if ring[:, 0].max() < blon[0] or ring[:, 0].min() > blon[1] \
                        or ring[:, 1].max() < blat[0] or ring[:, 1].min() > blat[1]:
                    continue
                px, py = tr.transform(ring[:, 0], ring[:, 1])
                out.append((np.column_stack([px, py]), props))
        return out

    w = 6.3
    h = min(8.0, max(3.5, w * (y1 - y0) / (x1 - x0)))
    fig, ax = plt.subplots(figsize=(w, h))
    ax.set_facecolor(WATER)

    depths = sorted({p.get("depth") for _, p in _geo("bathymetry.geojson") if p.get("depth")})
    shades = BLUES(np.linspace(0.0, 0.35, len(depths))) if depths else []
    for d, c in zip(depths, shades):
        polys = [r for r, _ in proj_rings("bathymetry.geojson", lambda p, d=d: p.get("depth") == d)]
        if polys:
            ax.add_collection(PolyCollection(polys, facecolors=c, edgecolors="none", alpha=0.35,
                                             zorder=0))
    land = [r for r, _ in proj_rings("land.geojson")] + \
        [r for r, _ in proj_rings("minor_islands.geojson")]
    ax.add_collection(PolyCollection(land, facecolors=LAND, edgecolors=LAND_EDGE,
                                     linewidths=0.4, zorder=1))

    track_line, track_label, circular = None, "Ship track (UTC date)", False
    if len(trk) > 1:
        seg = np.stack([txy[:-1], txy[1:]], axis=1)
        keep = np.hypot(*(txy[1:] - txy[:-1]).T) <= 50        # no line over data gaps
        times = pd.to_datetime(trk[:, 2], unit="s")
        cmap = TRACK_CMAP
        if colour == "time":
            vals = matplotlib.dates.date2num(times)
        else:
            from . import underway_panels as UP
            pn = next((p for p in UP.catalog() if p["id"] == colour), None)
            s = UP.series(leg, colour, times.min(), times.max()) if pn else pd.Series(dtype=float)
            reach = pd.Timedelta(minutes=90 if (pn or {}).get("source") == "hourly" else 20)
            vals = s.sort_index().reindex(times, method="nearest", tolerance=reach).to_numpy(dtype=float) \
                if len(s) else np.full(len(times), np.nan)
            circular = bool((pn or {}).get("circular"))
            # A circular scale with no pale ends (twilight's vanish near north on the water).
            cmap = plt.cm.hsv if circular else plt.cm.viridis
            track_label = f"{(pn or {}).get('label', colour)}" + (" · hourly" if (pn or {}).get("source") == "hourly" else "")
        mid = (vals[:-1] + vals[1:]) / 2
        if circular:                                          # no mean across north: take the start's
            mid = vals[:-1]
        have = keep & np.isfinite(mid)
        if (keep & ~have).any():
            ax.add_collection(LineCollection(seg[keep & ~have], colors=INK2, alpha=0.35, linewidths=0.8,
                                             linestyles=(0, (2, 1.5)), zorder=2))
        if have.any():
            track_line = LineCollection(seg[have], cmap=cmap, linewidths=1.0, zorder=2)
            track_line.set_array(mid[have])
            if circular:
                track_line.set_clim(0, 360)
            ax.add_collection(track_line)

    groups = list(dict.fromkeys(r["group"] for r in rows if r.get("lat") is not None))
    for i, g in enumerate(groups):
        sel = np.array([(r["lon"], r["lat"]) for r in rows if r["group"] == g and r.get("lat") is not None])
        gx, gy = tr.transform(sel[:, 0], sel[:, 1])
        ax.scatter(gx, gy, s=34, marker=MARKERS[i % len(MARKERS)],
                   color=CATEGORICAL[i % len(CATEGORICAL)], edgecolors="white", linewidths=0.8,
                   zorder=4 + i * 0.01, label=activities.LABELS.get(g, g))

    for j, lg in enumerate(logs):
        lx, ly = tr.transform(np.array([p[0] for p in lg["points"]]), np.array([p[1] for p in lg["points"]]))
        m = LOG_MARKERS[j % len(LOG_MARKERS)]
        ax.scatter(lx, ly, s=40, marker=m, facecolors="none", edgecolors="white", linewidths=2.6,
                   zorder=5 + j * 0.01)
        ax.scatter(lx, ly, s=40, marker=m, facecolors="none", edgecolors=INK, linewidths=1.0,
                   zorder=5 + j * 0.01, label=lg["label"])

    # Labels, station names first: each is skipped when its box would touch
    # one already placed, in map units from the axes' drawn width.
    ax.set_xlim(x0, x1)
    ax.set_ylim(y0, y1)
    ax.set_aspect("equal")
    fig.canvas.draw()
    km_per_pt = (x1 - x0) / (ax.get_window_extent().width * 72 / fig.dpi)
    char_w, line_h, lpad = 6.5 * 0.6 * km_per_pt, 6.5 * 1.3 * km_per_pt, 4 * km_per_pt
    boxes: list[tuple[float, float, float, float]] = []

    def place(text: str, sx: float, sy: float, size: float = 6.5, color: str = INK, below: bool = False) -> None:
        """A label up and to the right of its point (``below``: down and to the
        right, where a station's name does not sit)."""
        h_ = line_h * size / 6.5
        bx0 = sx + lpad
        by0 = sy - lpad * 0.75 - h_ if below else sy + lpad * 0.75
        box = (bx0, by0, bx0 + len(text) * char_w * size / 6.5, by0 + h_)
        if any(box[0] < b[2] and b[0] < box[2] and box[1] < b[3] and b[1] < box[3] for b in boxes):
            return
        boxes.append(box)
        ax.annotate(text, (sx, sy), xytext=(4, -3 if below else 3), textcoords="offset points",
                    va="top" if below else "baseline", fontsize=size, color=color, zorder=6)

    if label_stations:
        seen: dict[str, tuple[float, float]] = {}
        for r in rows:
            if r.get("station") and r.get("lat") is not None and r["station"] not in seen:
                seen[r["station"]] = tr.transform(r["lon"], r["lat"])
        rank = _station_rank(leg)
        # crowded: the station table carries every name
        for name, (sx, sy) in sorted(seen.items(), key=lambda kv: rank.get(kv[0], (0, 0)), reverse=True):
            place(name, sx, sy)
    # Sample ids: a place's ids in one label (a station's many samples
    # otherwise pile up), the first few and a count of the rest.
    for j, lg in enumerate(logs):
        at: dict[tuple[float, float], list[str]] = {}
        for x, y, sid in lg["points"]:
            if sid:
                at.setdefault((round(x, 3), round(y, 3)), []).append(sid)
        for (x, y), ids in at.items():
            ids = list(dict.fromkeys(ids))
            text = ", ".join(ids[:3]) + (f" +{len(ids) - 3}" if len(ids) > 3 else "")
            place(text, *tr.transform(x, y), size=5.5, color=INK2, below=True)

    # Graticule.
    for lat in range(int(blat[0]) - 1, int(blat[1]) + 2):
        lons = np.linspace(blon[0], blon[1], 200)
        gx, gy = tr.transform(lons, np.full_like(lons, lat))
        ax.plot(gx, gy, color=GRID, lw=0.4, zorder=1.5)
    lon_step = 5 if blon[1] - blon[0] > 12 else 2 if blon[1] - blon[0] > 5 else 1
    for lon in range(int(blon[0]) // lon_step * lon_step, int(blon[1]) + lon_step, lon_step):
        lats = np.linspace(blat[0], blat[1], 200)
        gx, gy = tr.transform(np.full_like(lats, lon), lats)
        ax.plot(gx, gy, color=GRID, lw=0.4, zorder=1.5)
    _graticule_labels(ax, tr, (x0, x1, y0, y1), blon, blat, lon_step)

    ax.set_xlim(x0, x1)
    ax.set_ylim(y0, y1)
    ax.set_aspect("equal")
    ax.set_xticks([])
    ax.set_yticks([])
    for s in ax.spines.values():
        s.set_visible(True)
        s.set_color(INK2)
    # Scale bar.
    bar = _nice(span / 5)
    bx, by = x0 + (x1 - x0) * 0.04, y0 + (y1 - y0) * 0.05
    ax.plot([bx, bx + bar], [by, by], color=INK, lw=2, solid_capstyle="butt", zorder=7)
    ax.text(bx + bar / 2, by + (y1 - y0) * 0.015, f"{bar:g} km", ha="center", va="bottom",
            fontsize=7, color=INK, zorder=7)
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.02), ncol=min(4, len(groups) + len(logs) + 1),
              fontsize=7, handletextpad=0.3, columnspacing=1.0)
    if track_line is not None:
        cb = fig.colorbar(track_line, ax=ax, fraction=0.035, pad=0.02)
        cb.set_label(track_label + ("" if colour == "time" else "; dashed grey: not recorded"), fontsize=7, color=INK2)
        if colour == "time":
            cb.ax.yaxis.set_major_locator(matplotlib.dates.AutoDateLocator(maxticks=6))
            cb.ax.yaxis.set_major_formatter(matplotlib.dates.DateFormatter("%d %b"))
        elif circular:
            cb.set_ticks([0, 90, 180, 270, 360])
        cb.ax.tick_params(labelsize=6.5)
        cb.outline.set_visible(False)
    return _png(fig)


FULL_GROUPS = {"plankton_nets", "ikmt_beam", "box_core", "multicorer", "gravity_piston", "grab"}


def _station_rank(leg: str) -> dict[str, tuple[int, int]]:
    """Label priority where names would collide: full stations first.

    A full station is one where the leg's event log also has nets or coring
    (not only the operations the participant selected), then stations are
    ranked by how many instruments worked there.
    """
    from . import eventlog

    groups: dict[str, set[str]] = {}
    for op in eventlog.operations(leg):
        if op.station and op.group not in ("void", "transit", "ship"):
            groups.setdefault(op.station, set()).add(op.group)
    return {st: (int(bool(g & FULL_GROUPS)), len(g)) for st, g in groups.items()}


def _nice(v: float) -> float:
    e = 10 ** math.floor(math.log10(v))
    return min((1, 2, 5, 10), key=lambda m: abs(m * e - v)) * e


def _crossings(gx, gy, at: float, lo: float, hi: float, along_x: bool) -> list[float]:
    """Where the line (gx, gy) crosses the edge ``x = at`` (``along_x``: ``y =
    at``) between ``lo`` and ``hi``, as the coordinate along that edge."""
    u, v = (gy, gx) if along_x else (gx, gy)
    out = []
    for k in np.nonzero(np.diff(np.sign(u - at)) != 0)[0]:
        f = (at - u[k]) / (u[k + 1] - u[k])
        w = v[k] + f * (v[k + 1] - v[k])
        if lo <= w <= hi:
            out.append(float(w))
    return out


def _graticule_labels(ax, tr, box, blon, blat, lon_step):
    """Parallels labelled where they cross the left edge, meridians where they
    cross the top, every ``lon_step`` or a multiple of it wide enough that no
    two labels touch (a meridian leaving through a side is not labelled)."""
    x0, x1, y0, y1 = box
    fs = 6.5
    px = lambda x, y: ax.transData.transform((x, y))  # noqa: E731
    char_px, line_px, gap_px = fs * 0.6 * ax.figure.dpi / 72, fs * 1.3 * ax.figure.dpi / 72, 4 * ax.figure.dpi / 72

    lats = []
    for lat in range(int(blat[0]), int(blat[1]) + 1):
        lons = np.linspace(blon[0], blon[1], 400)
        gx, gy = tr.transform(lons, np.full_like(lons, lat))
        lats += [(lat, y) for y in _crossings(gx, gy, x0, y0, y1, False)]
    placed: list[float] = []
    for lat, y in lats:
        py = px(x0, y)[1]
        if all(abs(py - q) >= line_px for q in placed):
            placed.append(py)
            ax.text(x0, y, f"{lat}°N ", ha="right", va="center", fontsize=fs, color=INK2, clip_on=False)

    first = int(blon[0]) // lon_step * lon_step
    lons = []
    for lon in range(first, int(blon[1]) + lon_step, lon_step):
        la = np.linspace(blat[0], blat[1], 400)
        gx, gy = tr.transform(np.full_like(la, lon), la)
        lons += [(lon, x, f"{abs(lon)}°{'W' if lon < 0 else 'E'}")
                 for x in _crossings(gx, gy, y1, x0, x1, True)]
    lons.sort(key=lambda c: c[1])

    def fits(chosen):
        edges = [(px(x, y1)[0], len(t) * char_px) for _, x, t in chosen]
        return all(b[0] - a[0] >= (a[1] + b[1]) / 2 + gap_px for a, b in zip(edges, edges[1:]))

    for mult in (1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 45, 90):
        step = lon_step * mult
        chosen = [c for c in lons if c[0] % step == 0]
        if fits(chosen):
            break
    for _, x, t in chosen:
        ax.text(x, y1, t, ha="center", va="bottom", fontsize=fs, color=INK2, clip_on=False)


# --- profiles -----------------------------------------------------------------

PROFILE_VARS = ["Temperature", "Salinity", "Fluorescence", "Oxygen"]


def _casts(leg: str, keys: list[str]) -> list[dict]:
    cache = ctd.cast_cache(leg)
    out = []
    for k in keys:
        for kind in ("CTD", "TM"):
            c = (cache.get(k) or {}).get(kind)
            if c and c.get("p"):
                out.append(c)
                break
    return sorted(out, key=lambda c: c.get("time") or "")


def profiles(leg: str, keys: list[str], variables: list[str] | None = None,
             max_depth: float | None = None, compressed: bool = False) -> bytes:
    """One panel per variable against pressure; ``compressed`` puts pressure on
    a square-root scale (the dashboard's compressed depth), which opens up the
    upper water column, still labelled in dbar."""
    casts = _casts(leg, keys)
    if not casts:
        return _empty("No processed CTD profiles for the selected casts.")
    variables = [v for v in (variables or PROFILE_VARS)
                 if any(c["vars"].get(v) for c in casts)]
    if not variables:
        return _empty("The selected casts carry none of the requested variables.")
    fig, axes = plt.subplots(1, len(variables), figsize=(1.7 * len(variables) + 0.6, 4.2),
                             sharey=True)
    axes = np.atleast_1d(axes)
    colors = BLUES(np.linspace(0.15, 1.0, len(casts))) if len(casts) > 1 else [CATEGORICAL[0]]
    for ax, var in zip(axes, variables):
        unit = next((c["units"].get(var) for c in casts if c["units"].get(var)), "")
        for c, col in zip(casts, colors):
            vals = c["vars"].get(var)
            if not vals:
                continue
            p = np.asarray(c["p"], dtype=float)
            v = np.asarray([np.nan if x is None else x for x in vals], dtype=float)
            ax.plot(v, p, color=col, lw=1.0)
        ax.set_xlabel(f"{var}" + (f" ({unit})" if unit else ""))
        ax.xaxis.set_label_position("top")
        ax.xaxis.tick_top()
        ax.spines["top"].set_visible(True)
        ax.spines["bottom"].set_visible(False)
        ax.grid(True, color=GRID, lw=0.4)
    axes[0].set_ylabel("Pressure (dbar, square-root scale)" if compressed else "Pressure (dbar)")
    if compressed:
        axes[0].set_yscale("function", functions=(lambda v: np.sqrt(np.clip(v, 0, None)),
                                                  lambda v: np.square(v)))
    axes[0].invert_yaxis()
    if max_depth:
        axes[0].set_ylim(max_depth, 0)
    elif compressed:
        axes[0].set_ylim(axes[0].get_ylim()[0], 0)
    if compressed:
        # Ticks spread evenly on a square-root axis, not the linear 50s crowded at depth.
        bottom = axes[0].get_ylim()[0]
        axes[0].set_yticks([v for v in (0, 10, 25, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 4000, 5000)
                            if v <= bottom])
    if len(casts) > 1:
        sm = plt.cm.ScalarMappable(cmap=BLUES, norm=plt.Normalize(0, 1))
        cb = fig.colorbar(sm, ax=list(axes), orientation="horizontal", fraction=0.04, pad=0.04,
                          aspect=40)
        cb.set_ticks([0, 1])
        t0, t1 = pd.Timestamp(casts[0]["time"]), pd.Timestamp(casts[-1]["time"])
        cb.set_ticklabels([f"{casts[0]['station']}\n{t0:%d %b %H:%M}",
                           f"{casts[-1]['station']}\n{t1:%d %b %H:%M}"])
        cb.ax.tick_params(labelsize=6.5)
        cb.outline.set_visible(False)
        cb.set_label(f"{len(casts)} casts, in time order", fontsize=7, color=INK2)
    return _png(fig)


# What the T–S points can be coloured by, besides a profile variable ("var:<name>").
TS_COLOURS = {"pressure": "Pressure (dbar)", "time": "Cast time", "lat": "Latitude (°N)", "station": "Station"}


def ts_colour_options(leg: str) -> list[list[str]]:
    """[value, label] for the T–S colour choice: the fixed ones, then every
    profile variable other than temperature and salinity the leg's casts have."""
    seen: dict[str, str] = {}
    for per in ctd.cast_cache(leg).values():
        for kind in ("CTD", "TM"):
            c = per.get(kind)
            for v, vals in ((c or {}).get("vars") or {}).items():
                if vals and v not in ("Temperature", "Salinity") and v not in seen:
                    unit = (c.get("units") or {}).get(v)
                    seen[v] = f"{v} ({unit})" if unit else v
    return [[k, v] for k, v in TS_COLOURS.items()] + [[f"var:{v}", label] for v, label in sorted(seen.items())]


def ts_diagram(leg: str, keys: list[str], bottles: list[tuple[float, float]] | None = None,
               bottle_label: str = "Bottles sampled", colour: str = "pressure") -> bytes:
    """Profile points coloured by ``colour`` (TS_COLOURS, or "var:<name>" for a
    profile variable; points without it are grey); ``bottles`` (salinity,
    temperature) as small ×."""
    casts = _casts(leg, keys)
    var = colour[4:] if colour.startswith("var:") else None
    t, s, p, cv = [], [], [], []
    stations = list(dict.fromkeys(c.get("station") or c.get("label") or "?" for c in casts))
    for c in casts:
        tv, sv = c["vars"].get("Temperature"), c["vars"].get("Salinity")
        if not tv or not sv:
            continue
        other = c["vars"].get(var) if var else None
        when = matplotlib.dates.date2num(pd.Timestamp(c["time"])) if c.get("time") else np.nan
        for i, (ti, si, pi) in enumerate(zip(tv, sv, c["p"])):
            if ti is None or si is None:
                continue
            t.append(ti)
            s.append(si)
            p.append(pi)
            if var:
                x = other[i] if other and i < len(other) else None
                cv.append(np.nan if x is None else x)
            elif colour == "time":
                cv.append(when)
            elif colour == "lat":
                cv.append(np.nan if c.get("lat") is None else c["lat"])
            elif colour == "station":
                cv.append(stations.index(c.get("station") or c.get("label") or "?"))
            else:
                cv.append(np.nan if pi is None else pi)
    if not t:
        return _empty("No temperature and salinity profiles for the selected casts.")
    fig, ax = plt.subplots(figsize=(4.8, 4.0))
    s_, t_, p_, c_ = (np.asarray(x, dtype=float) for x in (s, t, p, cv))
    order = np.argsort(p_)                        # deep points drawn first, the surface on top
    s_, t_, c_ = s_[order], t_[order], c_[order]
    have = np.isfinite(c_)
    if (~have).any():
        ax.scatter(s_[~have], t_[~have], color=GRID, s=4, linewidths=0, label="not measured")
    sc = None
    if colour == "station":
        # Past the palette's eight, 20 distinct colours (they repeat only beyond 20 stations).
        palette = CATEGORICAL if len(stations) <= len(CATEGORICAL) else list(plt.cm.tab20.colors)
        for k, name in enumerate(stations):
            m = have & (c_ == k)
            if m.any():
                ax.scatter(s_[m], t_[m], color=palette[k % len(palette)], s=4, linewidths=0, label=name)
    elif have.any():
        cmap = BLUES if colour == "pressure" else plt.cm.viridis
        sc = ax.scatter(s_[have], t_[have], c=c_[have], cmap=cmap, s=4, linewidths=0)
    # Freezing line at the surface (TEOS-10 approximation, -0.0575 S).
    ss = np.linspace(min(s), max(s), 50)
    ax.plot(ss, -0.0575 * ss, color=INK2, lw=0.7, ls="--")
    ax.text(ss[0], -0.0575 * ss[0], "surface freezing point", fontsize=6.5, color=INK2,
            va="top", ha="left", rotation=0)
    if bottles:
        bs, bt = zip(*bottles)
        ax.scatter(bs, bt, marker="x", s=14, linewidths=0.8, color=INK, zorder=5,
                   label=f"{bottle_label} (n = {len(bottles)})")
    if colour == "station":
        # One entry per station, beside the plot rather than over the points.
        fig.set_size_inches(4.8 + (1.3 if len(stations) <= 14 else 2.4), 4.0)
        ax.legend(loc="upper left", bbox_to_anchor=(1.01, 1.0), fontsize=6, frameon=False, markerscale=3,
                  ncol=1 if len(stations) <= 14 else 2, handletextpad=0.2, columnspacing=0.8)
    elif bottles or (~have).any():
        ax.legend(loc="upper left", fontsize=6.5, frameon=True, framealpha=0.85, edgecolor=GRID)
    ax.set_xlabel("Practical salinity")
    ax.set_ylabel("Temperature (°C)")
    ax.grid(True, color=GRID, lw=0.4)
    if sc is not None:
        cb = fig.colorbar(sc, ax=ax, fraction=0.05, pad=0.02)
        unit = next((c["units"].get(var) for c in casts if var and c.get("units", {}).get(var)), "")
        label = TS_COLOURS.get(colour) or (f"{var} ({unit})" if unit else var) or colour
        cb.set_label(label)
        if colour == "pressure":
            cb.ax.invert_yaxis()
        if colour == "time":
            cb.ax.yaxis.set_major_formatter(matplotlib.dates.DateFormatter("%d %b"))
        cb.outline.set_visible(False)
    return _png(fig)


# --- underway ---------------------------------------------------------------

ICE_PALETTE = CATEGORICAL[:6]


def underway_series(leg: str, rows: list[dict], panels: list[str] | None = None, ship: bool = False) -> bytes:
    """Stacked panels over the selected operations' period (6 h either side).

    Panels are ordered and labelled by the Underway tab's groups; the grey
    lines mark the selected operations and nothing else. The time axis is UTC,
    or ship time (``SHIP_TZ``) with ``ship``. One image; see
    ``underway_images`` for the split into pages.
    """
    from . import underway_panels as UP

    if not rows:
        return _empty("No operations selected.")
    t0 = pd.Timestamp(min(r["start_utc"] for r in rows)) - pd.Timedelta(hours=6)
    t1 = pd.Timestamp(max(r["end_utc"] for r in rows)) + pd.Timedelta(hours=6)
    ids = panels or UP.DEFAULT
    data = UP.read(leg, t0, t1, ids)
    chosen = data["panels"]
    if not chosen:
        return _empty("None of the chosen panels is available.")
    ice_legend = any(pn["id"] == "Camera · ice composition" for pn in chosen)
    height = 1.2 * len(chosen) + 0.5 + (0.35 if ice_legend else 0)
    fig, axes = plt.subplots(len(chosen), 1, figsize=(6.3, height), sharex=True, squeeze=False)
    axes = axes[:, 0]
    # Everything is read in UTC and only drawn in ship time.
    tx = (lambda i: pd.DatetimeIndex(i).tz_localize("UTC").tz_convert(SHIP_TZ).tz_localize(None)) if ship \
        else (lambda i: i)
    starts = tx(pd.DatetimeIndex(sorted({pd.Timestamp(r["start_utc"]) for r in rows})))
    cam = data["camera"]
    for ax, pn in zip(axes, chosen):
        src = data[pn["source"]] if pn["source"] != "camera" else None
        if pn["id"] == "Camera · concentration":
            if cam.empty:
                ax.text(0.5, 0.5, "no camera products", transform=ax.transAxes, ha="center",
                        va="center", color=INK2, fontsize=7)
            else:
                ax.plot(tx(cam.index), cam["ice"], ".", ms=1.2, color=GRID, zorder=1)
                # 30-minute centred mean on a 10-minute grid: no line across photo gaps
                smooth = cam["ice"].resample("10min").mean().rolling(3, center=True, min_periods=1).mean()
                ax.plot(tx(smooth.index), smooth, color=CATEGORICAL[0], lw=0.9, zorder=2)
                ax.set_ylim(-3, 103)
        elif pn["id"] == "Camera · ice composition":
            if cam.empty:
                ax.text(0.5, 0.5, "no camera products", transform=ax.transAxes, ha="center",
                        va="center", color=INK2, fontsize=7)
            else:
                hourly = cam[UP.ICE_TYPES].resample("1h").mean()
                ax.stackplot(tx(hourly.index), *[hourly[k].fillna(0) for k in UP.ICE_TYPES],
                             colors=ICE_PALETTE, labels=UP.ICE_TYPES, linewidth=0)
                ax.set_ylim(0, 100)
        elif src is None or src.empty or pn["id"] not in src:
            ax.text(0.5, 0.5, "not recorded over this period", transform=ax.transAxes, ha="center",
                    va="center", color=INK2, fontsize=7)
        else:
            y = src[pn["id"]]
            if pn["circular"]:
                ax.plot(tx(y.index), y, ".", ms=1.6, color=CATEGORICAL[0])
                ax.set_ylim(0, 360)
                ax.set_yticks([0, 90, 180, 270, 360])
            else:
                ax.plot(tx(y.index), y, color=CATEGORICAL[0], lw=0.9,
                        drawstyle="steps-mid" if pn["source"] == "hourly" else "default")
            if pn["reverse"]:
                ax.invert_yaxis()
        for s in starts:
            ax.axvline(s, color=GRID, lw=0.6, zorder=0)
        # The panel's name goes in its title after the group; the y axis keeps only the unit.
        # Words the group already says ("Surprise · 1 h", "(camera)") are dropped from the name.
        unit = pn["unit"] or ("−log10 p" if pn["group"] == "Surprise" else "")
        name = pn["label"].removesuffix(f" ({unit})").removesuffix(" (camera)")
        name = name.removeprefix(pn["group"]).removeprefix(" · ").strip()
        name += (" · hourly" if name else "hourly") if pn["source"] == "hourly" else ""
        ax.set_title(f"{pn['group']} – {name}" if name else pn["group"], loc="left", fontsize=7.5,
                     fontweight="bold", color=INK, pad=3)
        ax.set_ylabel(unit, rotation=0, ha="right", va="center", fontsize=7)
        ax.grid(True, axis="y", color=GRID, lw=0.4)
        ax.tick_params(labelsize=6.5)
    x0, x1 = tx(pd.DatetimeIndex([t0, t1]))
    axes[-1].set_xlim(x0, x1)
    loc = matplotlib.dates.AutoDateLocator(minticks=4, maxticks=8)
    axes[-1].xaxis.set_major_locator(loc)
    axes[-1].xaxis.set_major_formatter(matplotlib.dates.ConciseDateFormatter(loc))
    axes[-1].set_xlabel(f"ship time ({SHIP_TZ})" if ship else "UTC", fontsize=7, color=INK2)
    fig.align_ylabels(axes)
    if ice_legend:
        # Below the panels, so the ice types do not narrow every axis.
        fig.legend(*next(ax for ax, pn in zip(axes, chosen)
                         if pn["id"] == "Camera · ice composition").get_legend_handles_labels(),
                   loc="lower center", ncol=len(UP.ICE_TYPES), fontsize=6, frameon=False,
                   handlelength=1.0, columnspacing=1.2)
        fig.tight_layout(h_pad=0.4, rect=(0, 0.22 / height, 1, 1))
    else:
        fig.tight_layout(h_pad=0.4)
    return _png(fig)


MAX_PANELS_PER_IMAGE = 5


def split_panels(ids: list[str]) -> list[list[str]]:
    """The chosen panels, in group order, in the fewest images of at most
    MAX_PANELS_PER_IMAGE each, sized evenly (6 panels -> 3 + 3, not 5 + 1),
    cutting between groups where that costs no more than a panel of balance."""
    from . import underway_panels as UP

    wanted = set(ids)
    ordered = [p for p in UP.catalog() if p["id"] in wanted]
    n = len(ordered)
    if n <= MAX_PANELS_PER_IMAGE:
        return [[p["id"] for p in ordered]] if ordered else []
    parts = math.ceil(n / MAX_PANELS_PER_IMAGE)
    mean = n / parts
    boundary = [0 < k < n and ordered[k]["group"] != ordered[k - 1]["group"] for k in range(n + 1)]
    # best[k][i]: least cost of the first i panels in k images; a cut inside a group costs 1
    inf = float("inf")
    best = [[inf] * (n + 1) for _ in range(parts + 1)]
    back = [[0] * (n + 1) for _ in range(parts + 1)]
    best[0][0] = 0.0
    for k in range(1, parts + 1):
        for i in range(1, n + 1):
            for size in range(1, MAX_PANELS_PER_IMAGE + 1):
                j = i - size
                if j < 0 or best[k - 1][j] == inf:
                    continue
                cost = best[k - 1][j] + (size - mean) ** 2 \
                    + (0 if i == n or boundary[i] else 1)
                if cost < best[k][i]:
                    best[k][i], back[k][i] = cost, j
    edges, i = [n], n
    for k in range(parts, 0, -1):
        i = back[k][i]
        edges.append(i)
    edges.reverse()
    return [[p["id"] for p in ordered[a:b]] for a, b in zip(edges, edges[1:])]


def underway_images(leg: str, rows: list[dict], panels: list[str] | None = None, ship: bool = False) -> list[bytes]:
    from . import underway_panels as UP

    return [underway_series(leg, rows, chunk, ship) for chunk in split_panels(panels or UP.DEFAULT)] \
        or [underway_series(leg, rows, panels, ship)]
