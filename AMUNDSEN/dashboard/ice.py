"""Exploratory visible-ice proxy from explicit sea-only image crops, not calibrated SIC."""
from __future__ import annotations
import argparse
import base64
import csv
import io
import json
from pathlib import Path

import numpy as np
from PIL import Image, ImageOps
from jinja2 import Environment


def estimate(path: Path, roi, threshold=None):
    method = "brightness-otsu-v1" if threshold is None else "brightness-manual-v1"
    if len(roi) != 4 or not (0 <= roi[0] < roi[2] <= 1 and 0 <= roi[1] < roi[3] <= 1):
        raise ValueError("ROI must be normalized left,top,right,bottom within 0..1")
    if threshold is not None and not 1 <= threshold <= 254:
        raise ValueError("Brightness threshold must be within 1..254")
    with Image.open(path) as source:
        captured = source.getexif().get(306)  # retain raw EXIF; do not assume UTC
        im = ImageOps.exif_transpose(source).convert("RGB")
        w, h = im.size
        crop = im.crop(tuple(round(v * size) for v, size in zip(roi, (w,h,w,h))))
    if min(crop.size) < 8:
        raise ValueError("ROI too small")
    crop.thumbnail((1024,1024))
    gray = np.asarray(crop.convert("L"))
    if threshold is None:
        # Otsu: split dark water and brighter ice. The user must inspect masks;
        # glare, wake, shadow and thin ice can violate this simple assumption.
        hist = np.bincount(gray.ravel(), minlength=256).astype(float)
        weights = hist.cumsum()
        sums = (hist * np.arange(256)).cumsum()
        denom = weights * (weights[-1] - weights)
        score = np.divide((sums[-1] * weights - sums * weights[-1]) ** 2,
                          denom, out=np.zeros(256), where=denom > 0)
        threshold = int(score.argmax())
    p5, p95 = np.percentile(gray, [5,95])
    quality = "inspect mask: unvalidated" if p95-p5 >= 15 and p95 >= 35 else "unusable: dark or low contrast"
    mask = gray > threshold
    fraction = float(mask.mean()) if not quality.startswith("unusable") else None
    def jpeg(image):
        buffer = io.BytesIO()
        image.save(buffer, format="JPEG", quality=85)
        return "data:image/jpeg;base64," + base64.b64encode(buffer.getvalue()).decode("ascii")
    overlay = np.asarray(crop).copy()
    overlay[mask] = (overlay[mask] * .45 + np.array([50,210,255]) * .55).astype(np.uint8)
    return {"file":path.name, "capture_time_exif_unzoned":str(captured or ""), "roi":roi,
            "method":method, "threshold":threshold, "ice_fraction_proxy":fraction,
            "fraction_threshold_plus15":float((gray > min(255,threshold+15)).mean()),
            "fraction_threshold_minus15":float((gray > max(0,threshold-15)).mean()),
            "quality":quality, "original":jpeg(crop), "overlay":jpeg(Image.fromarray(overlay))}


def report(source: Path, config: Path, output: Path):
    entries = json.loads(config.read_text(encoding="utf-8"))
    if not 1 <= len(entries) <= 500:
        raise ValueError("Use 1..500 explicitly reviewed image/ROI entries")
    rows = []
    for entry in entries:
        path = (source / entry["file"]).resolve()
        if not path.is_relative_to(source.resolve()):
            raise ValueError("Image must be inside the supplied source directory")
        rows.append(estimate(path, entry["roi"], entry.get("threshold")))
    output.mkdir(parents=True, exist_ok=True)
    compact = [{k:v for k,v in row.items() if k not in ("original","overlay")} for row in rows]
    (output / "ice.json").write_text(json.dumps(compact, indent=2), encoding="utf-8")
    with (output / "ice.csv").open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(compact[0]))
        writer.writeheader(); writer.writerows(compact)
    template = Environment(autoescape=True).from_string('''<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1"><title>Exploratory ice proxy</title>
<style>body{font:16px sans-serif;background:#14202b;color:#eee;margin:20px}img{width:48%;max-width:650px}article{border-top:1px solid #678;padding:12px 0}progress{width:260px}a{color:#8de}</style>
<h1>Visible ice proxy — NOT validated sea-ice concentration</h1>
<p>Explicit sea-only crops. Bright pixels are candidate ice (cyan). Perspective, glare, wake, shadow and thin ice can bias results. Review each mask; values are exploratory, not for navigation or scientific inference.</p>
<p>The ±15 brightness-threshold range is sensitivity, not a confidence interval. EXIF times have no assumed timezone. Different crops do not represent the same geographic footprint.</p>
<p><a href="ice.csv">CSV for plotting</a> · <a href="ice.json">JSON</a></p>
{% for r in rows %}<article><h2>{{r.file}}</h2><p>{{r.quality}} · ROI {{r.roi}} · threshold {{r.threshold}}</p>
{% if r.ice_fraction_proxy is not none %}<progress max="1" value="{{r.ice_fraction_proxy}}"></progress> {{'%.1f'|format(r.ice_fraction_proxy*100)}}% visible-ice proxy
<p>Threshold sensitivity: {{'%.1f'|format(r.fraction_threshold_plus15*100)}}–{{'%.1f'|format(r.fraction_threshold_minus15*100)}}%</p>{% else %}<p>No estimate</p>{% endif %}
<img alt="Selected sea region" src="{{r.original}}"><img alt="Candidate ice mask in cyan" src="{{r.overlay}}"></article>{% endfor %}''')
    (output / "index.html").write_text(template.render(rows=rows), encoding="utf-8")
    return compact


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--source", type=Path, required=True)
    p.add_argument("--config", type=Path, required=True)
    p.add_argument("--output", type=Path, required=True)
    print(json.dumps(report(**vars(p.parse_args())), indent=2))
