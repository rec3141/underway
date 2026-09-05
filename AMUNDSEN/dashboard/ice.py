"""Exploratory visible-ice proxy from explicit sea-only image crops, not calibrated SIC."""
from __future__ import annotations
import argparse
import base64
import csv
import io
import json
import re
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
from PIL import Image, ImageOps
from jinja2 import Environment


def read_crop(path, roi):
    if len(roi) != 4 or not (0 <= roi[0] < roi[2] <= 1 and 0 <= roi[1] < roi[3] <= 1):
        raise ValueError("ROI must be normalized left,top,right,bottom within 0..1")
    with Image.open(path) as source:
        im = ImageOps.exif_transpose(source).convert("RGB")
        w,h = im.size
        crop = im.crop(tuple(round(v*size) for v,size in zip(roi,(w,h,w,h))))
    if min(crop.size) < 8:
        raise ValueError("ROI too small")
    crop.thumbnail((1024,1024))
    return crop


def train(source, calibration):
    """Small reference-colour LDA model; training crops are not validation data."""
    groups = {"ice":[], "water":[]}
    for entry in json.loads(calibration.read_text()):
        path = (source / entry["file"]).resolve()
        if not path.is_relative_to(source.resolve()):
            raise ValueError("Calibration image outside source")
        pixels = np.asarray(read_crop(path,entry["roi"]), dtype=float).reshape(-1,3)/255
        groups[entry["label"]].append(pixels[::max(1,len(pixels)//5000)])
    if not all(groups.values()):
        raise ValueError("Calibration needs both ice and water patches")
    groups = {key:np.concatenate(values) for key,values in groups.items()}
    means = {key:values.mean(axis=0) for key,values in groups.items()}
    covariance = sum(np.cov(values.T) for values in groups.values())/2 + np.eye(3)*.0005
    return {"method":"reference-colour-lda-v1", "means":{k:v.tolist() for k,v in means.items()},
            "precision":np.linalg.inv(covariance).tolist(), "calibration":json.loads(calibration.read_text()),
            "note":"Small manually selected reference patches, not independently validated."}


def estimate(path: Path, roi, threshold=None, model=None, exclude_reason=None):
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
    decision = gray
    if model:
        rgb = np.asarray(crop,dtype=float)/255
        ice,water = np.array(model["means"]["ice"]), np.array(model["means"]["water"])
        precision = np.array(model["precision"])
        score = (rgb - (ice+water)/2) @ precision @ (ice-water)
        decision = 255/(1+np.exp(-np.clip(score,-30,30)))
        threshold = 128 if threshold is None else threshold
        method = model["method"]
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
    if exclude_reason:
        quality = "unusable: " + exclude_reason
    mask = decision > threshold
    fraction = float(mask.mean()) if not quality.startswith("unusable") else None
    def jpeg(image):
        buffer = io.BytesIO()
        image.save(buffer, format="JPEG", quality=85)
        return "data:image/jpeg;base64," + base64.b64encode(buffer.getvalue()).decode("ascii")
    overlay = np.asarray(crop).copy()
    overlay[mask] = (overlay[mask] * .45 + np.array([50,210,255]) * .55).astype(np.uint8)
    match = re.search(r"Camera360_(\d{14})",path.name)
    utc = datetime.strptime(match[1],"%Y%m%d%H%M%S").replace(tzinfo=timezone.utc).isoformat() if match else None
    position = {"lat":None,"lon":None}
    if match:
        header = path.with_name(f"Camera360_{match[1]}_header.txt")
        if header.is_file():
            text = header.read_text(encoding="latin-1")
            for key,label in [("lat","Latitude"),("lon","Longitude")]:
                found = re.search(r"Initial_"+label+r" \[deg\]:\s*([-+\d.]+)",text)
                if found:
                    position[key] = float(found[1])
    return {"file":path.name, "capture_time_utc":utc, **position, "capture_time_exif_unzoned":str(captured or ""), "roi":roi,
            "method":method, "threshold":threshold, "ice_fraction_proxy":fraction,
            "threshold_units":"decision score (not probability)" if model else "brightness",
            "fraction_threshold_plus15":float((decision > min(255,threshold+15)).mean()) if fraction is not None else None,
            "fraction_threshold_minus15":float((decision > max(0,threshold-15)).mean()) if fraction is not None else None,
            "quality":quality, "original":jpeg(crop), "overlay":jpeg(Image.fromarray(overlay))}


def report(source: Path, config: Path, output: Path, calibration: Path | None = None):
    model = train(source,calibration) if calibration else None
    entries = json.loads(config.read_text(encoding="utf-8"))
    if not 1 <= len(entries) <= 500:
        raise ValueError("Use 1..500 explicitly reviewed image/ROI entries")
    rows = []
    for entry in entries:
        path = (source / entry["file"]).resolve()
        if not path.is_relative_to(source.resolve()):
            raise ValueError("Image must be inside the supplied source directory")
        rows.append(estimate(path, entry["roi"], entry.get("threshold"), model, entry.get("exclude_reason")))
    output.mkdir(parents=True, exist_ok=True)
    if model:
        (output / "model.json").write_text(json.dumps(model,indent=2),encoding="utf-8")
    compact = [{k:v for k,v in row.items() if k not in ("original","overlay")} for row in rows]
    (output / "ice.json").write_text(json.dumps(compact, indent=2), encoding="utf-8")
    with (output / "ice.csv").open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(compact[0]))
        writer.writeheader(); writer.writerows(compact)
    template = Environment(autoescape=True).from_string('''<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1"><title>Exploratory ice proxy</title>
<style>body{font:16px sans-serif;background:#14202b;color:#eee;margin:20px}img{width:48%;max-width:650px}article{border-top:1px solid #678;padding:12px 0}progress{width:260px}a{color:#8de}</style>
<h1>Visible ice proxy — NOT validated sea-ice concentration</h1>
<p>Explicit sea-only crops. Cyan pixels are candidate ice, based on brightness or a small reference-colour model. Perspective, glare, wake, shadow and thin ice can bias results. Review each mask; values are exploratory, not for navigation or scientific inference.</p>
<p>The ±15 decision-threshold range is sensitivity, not a confidence interval. Camera360 filename times are UTC; EXIF times have no assumed timezone. Different crops do not represent the same geographic footprint. Reference patches used to fit the model are not an independent validation.</p>
<p><a href="ice.csv">CSV for plotting</a> · <a href="ice.json">JSON</a></p>
{% for r in rows %}<article><h2>{{r.file}}</h2><p>{{r.quality}} · ROI {{r.roi}} · {{r.method}} · threshold {{r.threshold}} {{r.threshold_units}}</p>
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
    p.add_argument("--calibration", type=Path)
    print(json.dumps(report(**vars(p.parse_args())), indent=2))
