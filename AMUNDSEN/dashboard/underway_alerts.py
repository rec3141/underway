"""Current-water threshold and model-recommendation alerts.

The alert timer reads the published six-hour window, so the figure, the map,
the threshold decision and the model all use the same aligned observations.
"""

from __future__ import annotations

import base64
import io
import json
import math
import re
from datetime import datetime, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

from .config import LOCAL_TZ, WEBROOT

LAB_PARAMETERS = (
    "SST (°C)",
    "Salinity (PSU)",
    "Excess heat (°C)",
    "Surprise · 3 h",
    "TSG flow (V)",
    "Fluorescence (µg/L)",
    "Oxygen (mL/L)",
)
AI_PARAMETER = "AI recommendation"


def frame() -> dict:
    return json.loads((WEBROOT / "data" / "w-6h.json").read_text())


def _manifest_variables() -> list[dict]:
    manifest = json.loads((WEBROOT / "data" / "manifest.json").read_text())
    return manifest.get("variables", [])


def variables() -> list[dict]:
    return [v for v in _manifest_variables() if v.get("resolved") and v.get("name") not in ("Time elapsed (h)", "Distance travelled (km)")]


def parameter_at(index: int) -> str:
    rows = _manifest_variables()
    if not 0 <= index < len(rows):
        raise ValueError("unknown underway parameter")
    name = rows[index]["name"]
    variable(name)
    return name


def variable(parameter: str) -> dict:
    row = next((v for v in variables() if v.get("name") == parameter), None)
    if row is None:
        raise ValueError("unknown underway parameter")
    return row


def latest_sample(data: dict, parameter: str) -> dict | None:
    values = data.get("vars", {}).get(parameter)
    if not values:
        return None
    meta = variable(parameter)
    low = data.get("pump_low") if meta.get("tsg") else None
    for i in range(min(len(values), len(data.get("t", []))) - 1, -1, -1):
        value = values[i]
        if value is None or not math.isfinite(float(value)) or (low and i < len(low) and low[i]):
            continue
        return {"value": float(value), "time": int(data["t"][i]),
                "lat": data.get("lat", [None] * len(values))[i], "lon": data.get("lon", [None] * len(values))[i],
                "unit": meta.get("unit") or ""}
    return None


def fresh(data: dict, now: datetime, max_age_min: int = 10) -> bool:
    times = [int(t) for t in data.get("t", []) if t is not None]
    return bool(times) and 0 <= now.timestamp() * 1000 - max(times) <= max_age_min * 60_000


def _geometry(ax, path: Path, bounds: tuple[float, float, float, float]) -> None:
    try:
        features = json.loads(path.read_text()).get("features", [])
    except (OSError, ValueError):
        return
    west, east, south, north = bounds
    for feature in features:
        geometry = feature.get("geometry") or {}
        coordinates = geometry.get("coordinates") or []
        if geometry.get("type") == "MultiPolygon":
            polygons = coordinates
        elif geometry.get("type") == "Polygon":
            polygons = [coordinates]
        else:
            continue
        for polygon in polygons:
            if not polygon:
                continue
            ring = polygon[0]
            if not ring:
                continue
            xs = [p[0] for p in ring]; ys = [p[1] for p in ring]
            if max(xs) < west or min(xs) > east or max(ys) < south or min(ys) > north:
                continue
            ax.fill(xs, ys, facecolor="#d7d2c5", edgecolor="#938f85", linewidth=.45, zorder=0)


def render(data: dict, colour_parameter: str | None = None, alert_times: tuple[int, ...] = ()) -> bytes:
    """One PNG containing a track map and all aligned Lab time series."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt
    import numpy as np

    colour_parameter = colour_parameter if colour_parameter in data.get("vars", {}) else "Fluorescence (µg/L)"
    stamps = [datetime.fromtimestamp(t / 1000, timezone.utc).astimezone(ZoneInfo(LOCAL_TZ)) if t is not None else None for t in data.get("t", [])]
    times = np.array([mdates.date2num(t) if t else np.nan for t in stamps])
    lats = np.array([np.nan if q is None else float(q) for q in data.get("lat", [])]); lons = np.array([np.nan if q is None else float(q) for q in data.get("lon", [])])
    colours = np.array([np.nan if q is None else float(q) for q in data.get("vars", {}).get(colour_parameter, [])])
    low = np.array(data.get("pump_low") or [False] * len(times), dtype=bool)
    alert_dates = [datetime.fromtimestamp(stamp / 1000, timezone.utc).astimezone(ZoneInfo(LOCAL_TZ)) for stamp in alert_times]
    alert_numbers = [mdates.date2num(stamp) for stamp in alert_dates]
    fig = plt.figure(figsize=(14, 10), dpi=120, facecolor="#f7f8fa")
    grid = fig.add_gridspec(len(LAB_PARAMETERS), 2, width_ratios=(.92, 1.55), hspace=.12, wspace=.18)
    map_ax = fig.add_subplot(grid[:, 0])

    valid_pos = np.isfinite(lats) & np.isfinite(lons)
    if valid_pos.any():
        x, y = lons[valid_pos], lats[valid_pos]
        dx = max(.08, float(np.ptp(x)) * .12); dy = max(.06, float(np.ptp(y)) * .12)
        bounds = (float(x.min() - dx), float(x.max() + dx), float(y.min() - dy), float(y.max() + dy))
        _geometry(map_ax, Path(__file__).parent / "static" / "geo" / "land.geojson", bounds)
        _geometry(map_ax, Path(__file__).parent / "static" / "geo" / "minor_islands.geojson", bounds)
        good_colour = valid_pos & np.isfinite(colours) & ~low
        map_ax.plot(x, y, color="#aab2bd", linewidth=1.2, zorder=1)
        limits = data.get("limits", {}).get(colour_parameter)
        norm = None
        if limits and all(math.isfinite(float(q)) for q in limits) and limits[1] > limits[0]:
            norm = matplotlib.colors.Normalize(float(limits[0]), float(limits[1]))
        dots = map_ax.scatter(lons[good_colour], lats[good_colour], c=colours[good_colour], cmap="viridis", norm=norm,
                              s=11, linewidths=0, zorder=2)
        if (valid_pos & low).any():
            map_ax.scatter(lons[valid_pos & low], lats[valid_pos & low], c="#7d8895", s=11, linewidths=0, zorder=2)
        last = np.flatnonzero(valid_pos)[-1]
        map_ax.scatter([lons[last]], [lats[last]], marker="^", s=90, c="#e43d30", edgecolors="white", linewidths=.8, zorder=4)
        source_times = np.array([np.nan if stamp is None else float(stamp) for stamp in data.get("t", [])])
        for stamp in alert_times:
            candidates = np.flatnonzero(valid_pos & np.isfinite(source_times))
            if candidates.size:
                prior = candidates[np.argmin(np.abs(source_times[candidates] - stamp))]
                map_ax.scatter([lons[prior]], [lats[prior]], marker="o", s=70, facecolors="none", edgecolors="#d12f2f",
                               linewidths=2.2, zorder=4)
        if good_colour.any():
            cb = fig.colorbar(dots, ax=map_ax, orientation="horizontal", fraction=.035, pad=.035)
            cb.set_label(colour_parameter, fontsize=8); cb.ax.tick_params(labelsize=7)
        map_ax.set_xlim(bounds[0], bounds[1]); map_ax.set_ylim(bounds[2], bounds[3])
        map_ax.set_aspect(1 / max(.15, math.cos(math.radians(float(np.nanmean(y))))))
    map_ax.set_facecolor("#dceaf0"); map_ax.grid(color="white", linewidth=.5, alpha=.7)
    map_ax.set_title(f"Six-hour track · colour: {colour_parameter}", fontsize=11, loc="left", weight="bold")
    map_ax.set_xlabel("longitude"); map_ax.set_ylabel("latitude")
    map_ax.tick_params(labelsize=8)

    axes = []
    for row, parameter in enumerate(LAB_PARAMETERS):
        ax = fig.add_subplot(grid[row, 1], sharex=axes[0] if axes else None); axes.append(ax)
        values = np.array([np.nan if q is None else float(q) for q in data.get("vars", {}).get(parameter, [None] * len(times))])
        good = np.isfinite(times) & np.isfinite(values)
        pumped = good & ~low
        ax.plot_date(times[pumped], values[pumped], "-", color="#177f89", linewidth=1.15, markersize=0)
        if (good & low).any():
            ax.plot_date(times[good & low], values[good & low], ".", color="#7d8895", markersize=2.5)
        unit = next((v.get("unit", "") for v in variables() if v.get("name") == parameter), "")
        label = parameter.rsplit(" (", 1)[0]
        ax.set_ylabel(f"{label}\n{unit}", fontsize=7, rotation=0, ha="right", va="center", labelpad=7)
        ax.grid(color="#d9dee5", linewidth=.5); ax.tick_params(labelsize=7, length=2)
        for stamp in alert_numbers:
            ax.axvline(stamp, color="#d12f2f", linewidth=2.2, alpha=.92, zorder=5)
        for side in ("top", "right"): ax.spines[side].set_visible(False)
        if row < len(LAB_PARAMETERS) - 1: ax.tick_params(labelbottom=False)
    if axes:
        axes[-1].xaxis.set_major_formatter(mdates.DateFormatter("%H:%M", tz=ZoneInfo(LOCAL_TZ)))
        axes[-1].set_xlabel(f"ship time ({datetime.now(ZoneInfo(LOCAL_TZ)).tzname()})", fontsize=8)
    start = next((t for t in stamps if t), None); end = next((t for t in reversed(stamps) if t), None)
    title = "CCGS Amundsen · aligned Lab observations"
    if start and end:
        title += f" · {start:%Y-%m-%d %H:%M}–{end:%H:%M} {end.tzname()}"
    if alert_times:
        title += " · red = this alert's earlier deliveries"
    fig.suptitle(title, x=.02, ha="left", fontsize=13, weight="bold")
    fig.subplots_adjust(top=.94, bottom=.07, left=.07, right=.98)
    out = io.BytesIO(); fig.savefig(out, format="png", facecolor=fig.get_facecolor()); plt.close(fig)
    return out.getvalue()


def _lab_summary(data: dict) -> str:
    rows = []
    times = data.get("t", []); low = data.get("pump_low") or [False] * len(times)
    end = max((t for t in times if t is not None), default=0); recent = end - 30 * 60_000
    for parameter in LAB_PARAMETERS:
        values = data.get("vars", {}).get(parameter) or []
        valid = [(int(times[i]), float(q)) for i, q in enumerate(values) if i < len(times) and times[i] is not None and q is not None
                 and math.isfinite(float(q)) and not (i < len(low) and low[i])]
        if not valid:
            continue
        before = min(valid, key=lambda x: abs(x[0] - recent))[1]
        rows.append(f"{parameter}: latest {valid[-1][1]:.4g}; change over 30 min {valid[-1][1] - before:+.4g}; change over panel {valid[-1][1] - valid[0][1]:+.4g}")
    return "\n".join(rows)


def ai_recommendation(image: bytes, data: dict) -> dict:
    """Ask the configured local multimodal model for a machine-readable decision."""
    import requests
    from .chatbot import ModelOffline, model_status

    status = model_status(fresh=True)
    if not status.get("online"):
        raise ModelOffline(status.get("why") or "no model loaded")
    prompt = ("The image is one aligned six-hour panel of every flow-through Lab parameter and its matching ship track. "
              "Red vertical lines and red map rings, when present, mark earlier successful deliveries of this subscriber's alert; use them to judge what changed since those notices. "
              "Decide whether the water at the latest point is scientifically interesting enough to take a discrete water sample now. "
              "Look for coherent gradients, fronts, peaks, unusual combinations and whether flow is valid. Do not call fluorescence alone a bloom. "
              "Check that every direction of change named in the reason agrees with the plotted direction, the headline, and the authoritative numerical checks below. "
              "Return only JSON with keys interesting (boolean), headline (under 70 characters), reason (one or two sentences), and objective (one sentence).\n\n"
              "Numerical checks:\n" + _lab_summary(data))
    uri = "data:image/png;base64," + base64.b64encode(image).decode()
    messages = [{"role": "system", "content": "You are a marine scientist advising the CCGS Amundsen sampling team."},
                {"role": "user", "content": [{"type": "text", "text": prompt}, {"type": "image_url", "image_url": {"url": uri}}]}]
    if status["backend"] == "openai":
        body = {"model": status["model"], "messages": messages, "stream": False, "temperature": .3, "max_tokens": 400,
                "chat_template_kwargs": {"enable_thinking": False}}
        response = requests.post(status["url"] + "/v1/chat/completions", json=body, timeout=180)
        response.raise_for_status(); text = response.json()["choices"][0]["message"]["content"]
    else:
        messages = [{"role": "system", "content": messages[0]["content"]},
                    {"role": "user", "content": prompt, "images": [uri.split(",", 1)[1]]}]
        body = {"model": status["model"], "messages": messages, "stream": False, "think": False, "keep_alive": -1,
                "options": {"temperature": .3, "num_predict": 400, "num_ctx": 16384}}
        response = requests.post(status["url"] + "/api/chat", json=body, timeout=180)
        response.raise_for_status(); text = response.json().get("message", {}).get("content", "")
    match = re.search(r"\{.*\}", text, re.S)
    if not match:
        raise ValueError("model did not return a recommendation object")
    answer = json.loads(match.group(0))
    return {"interesting": bool(answer.get("interesting")), "headline": str(answer.get("headline") or "AI sampling recommendation")[:100],
            "reason": str(answer.get("reason") or "").strip()[:800], "objective": str(answer.get("objective") or "").strip()[:500]}
