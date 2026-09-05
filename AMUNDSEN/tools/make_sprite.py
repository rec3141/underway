#!/usr/bin/env python3
"""Draw the map's marker sprite: static/geo/sprite.{png,json} and the @2x pair.

MapLibre draws non-circle markers from a sprite sheet, so the square used for
communities and the coloured triangles used for event-log entries live here.
Icons at 1x: "square-15" (off-white, 12 px), "tri-0-15" … "tri-9-15" (12 px, the
event palette), "ship-15" (28×12 px, the Amundsen seen from above in Coast Guard red
and white, bow to the right, turned to the heading on the map) and "camera-15"
(14×12 px, a camera body for the daily timelapses). Plotly requests sprite icons
as "<marker.symbol>-15". Re-run after changing the palette; the dashboard's map style points
its `sprite` at these files.
"""

from pathlib import Path
import json
from PIL import Image, ImageDraw

OUT = Path(__file__).resolve().parents[1] / "dashboard" / "static" / "geo"
PALETTE = ["#7ee787", "#d2a8ff", "#f2cc60", "#79c0ff", "#ffa198", "#56d364", "#e3b341", "#a5d6ff", "#ff9bce", "#ffb454"]
ICON = 12


SHIP_W, SHIP_H = 28, 12          # the ship glyph at 1x: plan view, red hull, white house, bow to the right
HULL, HOUSE, OUTLINE = "#d52b1e", "#f4f4f4", "#0b1620"    # Canadian Coast Guard red and white


def ship(d: ImageDraw.ImageDraw, x0: int, scale: int) -> None:
    """Seen from above so it can be turned to the heading: a rounded stern at
    the left, straight sides, a pointed bow at the right; the superstructure
    sits just aft of midships, the icebreaking bow is left clear."""
    k = scale
    cy = SHIP_H * k / 2
    hull = [(x0 + 3 * k, cy - 4 * k), (x0 + 18 * k, cy - 4 * k), (x0 + 27 * k, cy), (x0 + 18 * k, cy + 4 * k), (x0 + 3 * k, cy + 4 * k),
            (x0 + 1 * k, cy + 2 * k), (x0 + 1 * k, cy - 2 * k)]
    d.polygon(hull, fill=HULL, outline=OUTLINE, width=k)
    d.rectangle([x0 + 7 * k, cy - 2 * k, x0 + 15 * k, cy + 2 * k], fill=HOUSE, outline=OUTLINE, width=k)   # house
    d.line([(x0 + 20 * k, cy - 3 * k), (x0 + 22 * k, cy + 3 * k)], fill=HOUSE, width=k)                     # the CCG stripe


CAM_W, CAM_H = 14, 12
CAM = "#dfe8f2"


def camera(d: ImageDraw.ImageDraw, x0: int, scale: int) -> None:
    """A camera body with a lens, seen from the front."""
    k = scale
    d.rectangle([x0 + 1 * k, 3 * k, x0 + 13 * k, 11 * k], fill=CAM, outline=OUTLINE, width=k)          # body
    d.rectangle([x0 + 4 * k, 1 * k, x0 + 8 * k, 3 * k], fill=CAM, outline=OUTLINE, width=k)            # viewfinder hump
    d.ellipse([x0 + 4.5 * k, 4.5 * k, x0 + 10 * k, 10 * k], fill=OUTLINE)                              # lens
    d.ellipse([x0 + 6 * k, 6 * k, x0 + 8.5 * k, 8.5 * k], fill="#5cc8ff")                             # glass


def sheet(scale: int):
    icons = [("square-15", "#f2e7c9")] + [(f"tri-{i}-15", c) for i, c in enumerate(PALETTE)]   # Plotly asks for Maki-style "<symbol>-15" names
    s = ICON * scale
    img = Image.new("RGBA", (s * len(icons) + (SHIP_W + CAM_W) * scale, max(s, SHIP_H * scale, CAM_H * scale)), (0, 0, 0, 0))
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
    x0 = s * len(icons)
    ship(d, x0, scale)
    index["ship-15"] = {"width": SHIP_W * scale, "height": SHIP_H * scale, "x": x0, "y": 0, "pixelRatio": scale}
    x0 += SHIP_W * scale
    camera(d, x0, scale)
    index["camera-15"] = {"width": CAM_W * scale, "height": CAM_H * scale, "x": x0, "y": 0, "pixelRatio": scale}
    return img, index


for scale, suffix in ((1, ""), (2, "@2x")):
    img, index = sheet(scale)
    img.save(OUT / f"sprite{suffix}.png")
    (OUT / f"sprite{suffix}.json").write_text(json.dumps(index))
print("sprite:", sorted(index))
