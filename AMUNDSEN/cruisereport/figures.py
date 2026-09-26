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
from matplotlib.collections import PolyCollection  # noqa: E402
from matplotlib.colors import LinearSegmentedColormap  # noqa: E402
from pyproj import Transformer  # noqa: E402

from . import activities, ctd, underway  # noqa: E402
from .config import GEO_DIR  # noqa: E402

CATEGORICAL = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7",
               "#e34948"]
MARKERS = ["o", "s", "^", "D", "v", "P", "X", "h"]
BLUES = LinearSegmentedColormap.from_list(
    "blues", ["#b7d3f6", "#6da7ec", "#2a78d6", "#1c5cab", "#0d366b"])
INK, INK2, GRID = "#0b0b0b", "#52514e", "#e4e3df"
LAND, LAND_EDGE, WATER = "#e9e7e1", "#b9b7af", "#fcfcfb"
DPI = 200

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


def _track(leg: str, t0: str | None, t1: str | None, step: int = 30) -> np.ndarray:
    p = underway.db_path(leg)
    if not p.is_file():
        return np.empty((0, 2))
    con = sqlite3.connect(f"file:{p}?mode=ro", uri=True)
    try:
        cols = dict(con.execute("SELECT key, col FROM columns").fetchall())
        la, lo = cols.get("posmv — latitude (deg n)"), cols.get("posmv — longitude (deg e)")
        if not la or not lo:
            return np.empty((0, 2))
        q = f"SELECT {lo}, {la} FROM obs WHERE rowid % {step} = 0"
        args: list = []
        if t0 and t1:
            q += " AND t BETWEEN ? AND ?"
            args = [int(pd.Timestamp(t0).tz_localize("UTC").timestamp()),
                    int(pd.Timestamp(t1).tz_localize("UTC").timestamp())]
        arr = np.array(con.execute(q + " ORDER BY t", args).fetchall(), dtype=float)
    finally:
        con.close()
    return arr[np.isfinite(arr).all(axis=1)] if len(arr) else np.empty((0, 2))


def station_map(leg: str, rows: list[dict], *, whole_leg_track: bool = True,
                label_stations: bool = True) -> bytes:
    pts = [(r["lon"], r["lat"]) for r in rows if r.get("lat") is not None]
    if not pts:
        return _empty("No positions for the selected operations.")
    lon0 = float(np.median([p[0] for p in pts]))
    lat0 = float(np.median([p[1] for p in pts]))
    tr = Transformer.from_crs("EPSG:4326",
                              f"+proj=laea +lat_0={lat0} +lon_0={lon0} +units=km", always_xy=True)
    xy = np.array([tr.transform(*p) for p in pts])
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

    t0 = None if whole_leg_track else min(r["start_utc"] for r in rows)
    t1 = None if whole_leg_track else max(r["end_utc"] for r in rows)
    trk = _track(leg, t0, t1)
    if len(trk):
        tx, ty = tr.transform(trk[:, 0], trk[:, 1])
        jump = np.hypot(np.diff(tx), np.diff(ty)) > 50        # break the line over data gaps
        tx, ty = np.insert(tx, np.where(jump)[0] + 1, np.nan), np.insert(ty, np.where(jump)[0] + 1, np.nan)
        ax.plot(tx, ty, color=INK2, lw=0.7, alpha=0.7, zorder=2, label="Ship track")

    groups = list(dict.fromkeys(r["group"] for r in rows if r.get("lat") is not None))
    for i, g in enumerate(groups):
        sel = np.array([(r["lon"], r["lat"]) for r in rows if r["group"] == g and r.get("lat") is not None])
        gx, gy = tr.transform(sel[:, 0], sel[:, 1])
        ax.scatter(gx, gy, s=34, marker=MARKERS[i % len(MARKERS)],
                   color=CATEGORICAL[i % len(CATEGORICAL)], edgecolors="white", linewidths=0.8,
                   zorder=4 + i * 0.01, label=activities.LABELS.get(g, g))

    if label_stations:
        seen: dict[str, tuple[float, float]] = {}
        for r in rows:
            if r.get("station") and r.get("lat") is not None and r["station"] not in seen:
                seen[r["station"]] = tr.transform(r["lon"], r["lat"])
        # Label boxes in map units, from the axes' drawn width: a name is
        # skipped when its box would touch a placed one.
        ax.set_xlim(x0, x1)
        ax.set_ylim(y0, y1)
        ax.set_aspect("equal")
        fig.canvas.draw()
        km_per_pt = (x1 - x0) / (ax.get_window_extent().width * 72 / fig.dpi)
        char_w, line_h, pad = 6.5 * 0.6 * km_per_pt, 6.5 * 1.3 * km_per_pt, 4 * km_per_pt
        boxes: list[tuple[float, float, float, float]] = []
        rank = _station_rank(leg)
        for name, (sx, sy) in sorted(seen.items(), key=lambda kv: rank.get(kv[0], (0, 0)),
                                     reverse=True):
            bx0, by0 = sx + pad, sy + pad * 0.75
            box = (bx0, by0, bx0 + len(name) * char_w, by0 + line_h)
            if any(box[0] < b[2] and b[0] < box[2] and box[1] < b[3] and b[1] < box[3] for b in boxes):
                continue           # crowded: the station table carries every name
            boxes.append(box)
            ax.annotate(name, (sx, sy), xytext=(4, 3), textcoords="offset points",
                        fontsize=6.5, color=INK, zorder=6)

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
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.02), ncol=min(4, len(groups) + 1),
              fontsize=7, handletextpad=0.3, columnspacing=1.0)
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


def _graticule_labels(ax, tr, box, blon, blat, lon_step):
    x0, x1, y0, y1 = box
    for lat in range(int(blat[0]), int(blat[1]) + 1):
        lons = np.linspace(blon[0], blon[1], 400)
        gx, gy = tr.transform(lons, np.full_like(lons, lat))
        inside = (gx > x0) & (gx < x1) & (gy > y0) & (gy < y1)
        if inside.any():
            i = np.where(inside)[0][0]
            ax.text(x0, gy[i], f"{lat}°N ", ha="right", va="center", fontsize=6.5, color=INK2,
                    clip_on=False)
    for lon in range(int(blon[0]) // lon_step * lon_step, int(blon[1]) + lon_step, lon_step):
        lats = np.linspace(blat[0], blat[1], 400)
        gx, gy = tr.transform(np.full_like(lats, lon), lats)
        inside = (gx > x0) & (gx < x1) & (gy > y0) & (gy < y1)
        if inside.any():
            i = np.where(inside)[0][-1]
            ax.text(gx[i], y1, f"{abs(lon)}°{'W' if lon < 0 else 'E'}", ha="center",
                    va="bottom", fontsize=6.5, color=INK2, clip_on=False)


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
             max_depth: float | None = None) -> bytes:
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
    axes[0].set_ylabel("Pressure (dbar)")
    axes[0].invert_yaxis()
    if max_depth:
        axes[0].set_ylim(max_depth, 0)
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


def ts_diagram(leg: str, keys: list[str], bottles: list[tuple[float, float]] | None = None,
               bottle_label: str = "Bottles sampled") -> bytes:
    """Profiles coloured by pressure; ``bottles`` (salinity, temperature) as small ×."""
    casts = _casts(leg, keys)
    t, s, p = [], [], []
    for c in casts:
        tv, sv = c["vars"].get("Temperature"), c["vars"].get("Salinity")
        if not tv or not sv:
            continue
        for ti, si, pi in zip(tv, sv, c["p"]):
            if ti is not None and si is not None:
                t.append(ti)
                s.append(si)
                p.append(pi)
    if not t:
        return _empty("No temperature and salinity profiles for the selected casts.")
    fig, ax = plt.subplots(figsize=(4.8, 4.0))
    order = np.argsort(p)
    sc = ax.scatter(np.asarray(s)[order], np.asarray(t)[order], c=np.asarray(p)[order],
                    cmap=BLUES, s=4, linewidths=0)
    # Freezing line at the surface (TEOS-10 approximation, -0.0575 S).
    ss = np.linspace(min(s), max(s), 50)
    ax.plot(ss, -0.0575 * ss, color=INK2, lw=0.7, ls="--")
    ax.text(ss[0], -0.0575 * ss[0], "surface freezing point", fontsize=6.5, color=INK2,
            va="top", ha="left", rotation=0)
    if bottles:
        bs, bt = zip(*bottles)
        ax.scatter(bs, bt, marker="x", s=14, linewidths=0.8, color=INK, zorder=5,
                   label=f"{bottle_label} (n = {len(bottles)})")
        ax.legend(loc="upper left", fontsize=7, frameon=True, framealpha=0.85, edgecolor=GRID)
    ax.set_xlabel("Practical salinity")
    ax.set_ylabel("Temperature (°C)")
    ax.grid(True, color=GRID, lw=0.4)
    cb = fig.colorbar(sc, ax=ax, fraction=0.05, pad=0.02)
    cb.set_label("Pressure (dbar)")
    cb.ax.invert_yaxis()
    cb.outline.set_visible(False)
    return _png(fig)


# --- underway ---------------------------------------------------------------

ICE_PALETTE = CATEGORICAL[:6]


def underway_series(leg: str, rows: list[dict], panels: list[str] | None = None) -> bytes:
    """Stacked panels over the selected operations' period (6 h either side).

    Panels are ordered and labelled by the Underway tab's groups; the grey
    lines mark the selected operations and nothing else. One image; see
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
    fig, axes = plt.subplots(len(chosen), 1, figsize=(6.3, 1.05 * len(chosen) + 0.5), sharex=True,
                             squeeze=False)
    axes = axes[:, 0]
    starts = sorted({pd.Timestamp(r["start_utc"]) for r in rows})
    cam = data["camera"]
    last_group = None
    for ax, pn in zip(axes, chosen):
        src = data[pn["source"]] if pn["source"] != "camera" else None
        if pn["id"] == "Camera · concentration":
            if cam.empty:
                ax.text(0.5, 0.5, "no camera products", transform=ax.transAxes, ha="center",
                        va="center", color=INK2, fontsize=7)
            else:
                ax.plot(cam.index, cam["ice"], ".", ms=1.2, color=GRID, zorder=1)
                # 30-minute centred mean on a 10-minute grid: no line across photo gaps
                smooth = cam["ice"].resample("10min").mean().rolling(3, center=True, min_periods=1).mean()
                ax.plot(smooth.index, smooth, color=CATEGORICAL[0], lw=0.9, zorder=2)
                ax.set_ylim(-3, 103)
        elif pn["id"] == "Camera · ice composition":
            if cam.empty:
                ax.text(0.5, 0.5, "no camera products", transform=ax.transAxes, ha="center",
                        va="center", color=INK2, fontsize=7)
            else:
                hourly = cam[UP.ICE_TYPES].resample("1h").mean()
                ax.stackplot(hourly.index, *[hourly[k].fillna(0) for k in UP.ICE_TYPES],
                             colors=ICE_PALETTE, labels=UP.ICE_TYPES, linewidth=0)
                ax.set_ylim(0, 100)
                ax.legend(loc="upper left", bbox_to_anchor=(1.01, 1.0), fontsize=6, frameon=False,
                          handlelength=1.0)
        elif src is None or src.empty or pn["id"] not in src:
            ax.text(0.5, 0.5, "not recorded over this period", transform=ax.transAxes, ha="center",
                    va="center", color=INK2, fontsize=7)
        else:
            y = src[pn["id"]]
            if pn["circular"]:
                ax.plot(y.index, y, ".", ms=1.6, color=CATEGORICAL[0])
                ax.set_ylim(0, 360)
                ax.set_yticks([0, 90, 180, 270, 360])
            else:
                ax.plot(y.index, y, color=CATEGORICAL[0], lw=0.9,
                        drawstyle="steps-mid" if pn["source"] == "hourly" else "default")
            if pn["reverse"]:
                ax.invert_yaxis()
        for s in starts:
            ax.axvline(s, color=GRID, lw=0.6, zorder=0)
        label = pn["label"] + (" · hourly" if pn["source"] == "hourly" else "")
        ax.set_ylabel(label, rotation=0, ha="right", va="center", fontsize=7)
        ax.grid(True, axis="y", color=GRID, lw=0.4)
        ax.tick_params(labelsize=6.5)
        if pn["group"] != last_group:
            ax.set_title(pn["group"], loc="left", fontsize=7.5, fontweight="bold", color=INK, pad=3)
            last_group = pn["group"]
    axes[-1].set_xlim(t0, t1)
    loc = matplotlib.dates.AutoDateLocator(minticks=4, maxticks=8)
    axes[-1].xaxis.set_major_locator(loc)
    axes[-1].xaxis.set_major_formatter(matplotlib.dates.ConciseDateFormatter(loc))
    axes[-1].set_xlabel("UTC", fontsize=7, color=INK2)
    fig.align_ylabels(axes)
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


def underway_images(leg: str, rows: list[dict], panels: list[str] | None = None) -> list[bytes]:
    from . import underway_panels as UP

    return [underway_series(leg, rows, chunk) for chunk in split_panels(panels or UP.DEFAULT)] \
        or [underway_series(leg, rows, panels)]
