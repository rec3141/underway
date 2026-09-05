#!/usr/bin/env python3
"""Draw the map's marker sprite: static/geo/sprite.{png,json} and the @2x pair.

MapLibre draws non-circle markers from a sprite sheet, so the square used for
communities and the coloured triangles used for event-log entries live here.
Icons are 12 px at 1x: "square-15" (off-white) and "tri-0-15" … "tri-9-15" in
the event palette; Plotly requests sprite icons as "<marker.symbol>-15". Re-run after changing the palette; the dashboard's map style points
its `sprite` at these files.
"""

from pathlib import Path
import json
from PIL import Image, ImageDraw

OUT = Path(__file__).resolve().parents[1] / "dashboard" / "static" / "geo"
PALETTE = ["#7ee787", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce", "#ffb454"]
ICON = 12


def sheet(scale: int):
    icons = [("square-15", "#f2e7c9")] + [(f"tri-{i}-15", c) for i, c in enumerate(PALETTE)]   # Plotly asks for Maki-style "<symbol>-15" names
    s = ICON * scale
    img = Image.new("RGBA", (s * len(icons), s), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    index = {}
    for i, (name, colour) in enumerate(icons):
        x0 = i * s
        pad = max(1, scale)
        if name.startswith("square"):
            d.rectangle([x0 + pad, pad, x0 + s - 1 - pad, s - 1 - pad], fill=colour, outline="#0b1620", width=scale)
        else:
            d.polygon([(x0 + s / 2, pad), (x0 + s - 1 - pad, s - 1 - pad), (x0 + pad, s - 1 - pad)], fill=colour, outline="#0b1620", width=scale)
        index[name] = {"width": s, "height": s, "x": x0, "y": 0, "pixelRatio": scale}
    return img, index


for scale, suffix in ((1, ""), (2, "@2x")):
    img, index = sheet(scale)
    img.save(OUT / f"sprite{suffix}.png")
    (OUT / f"sprite{suffix}.json").write_text(json.dumps(index))
print("sprite:", sorted(index))
