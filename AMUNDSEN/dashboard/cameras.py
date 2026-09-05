"""Bounded, offline camera timelapses. Run independently of dashboard builds."""
from __future__ import annotations

import argparse
from datetime import datetime, timedelta, timezone
import json
import hashlib
import logging
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

log = logging.getLogger(__name__)
STAMP = re.compile(r"(?:Camera360_)?(\d{14})(?:_mosaic|_11)\.jpg$", re.I)


def frames(source: Path, end: datetime, hours=24, interval=120, start=None):
    """Read only date folders in the requested UTC window; one frame per bin."""
    start = start or end - timedelta(hours=hours)
    selected = {}
    day = start.date()
    while day <= (end - timedelta(microseconds=1)).date():
        folder = source / day.strftime("%Y%m%d")
        if folder.is_dir():
            # All-sky files are direct children; 360 mosaics are in HHMMSS dirs.
            for pattern in ("*_11.jpg", "*/Camera360_*_mosaic.jpg"):
                for path in sorted(folder.glob(pattern)):
                    match = STAMP.fullmatch(path.name)
                    if not match:
                        continue
                    stamp = datetime.strptime(match[1], "%Y%m%d%H%M%S").replace(tzinfo=timezone.utc)
                    if start <= stamp < end:
                        selected.setdefault(int(stamp.timestamp()) // interval, (stamp, path))
        day += timedelta(days=1)
    return sorted(selected.values())


def compose_frame(path, width, layout="mosaic"):
    from PIL import Image, ImageOps
    if layout == "portrait":
        if width % 18:
            raise ValueError("Portrait width must be a multiple of 18 (e.g. 1080 or 540)")
        height = width * 16 // 9
        top = height // 3
        bottom = height - top
        canvas = Image.new("RGB", (width,height), "black")
        views = []
        for number in (1,2,3):
            sibling = path.with_name(path.name.replace("_mosaic.jpg", f"_cam_{number}.jpg"))
            if sibling == path:
                raise ValueError("Portrait layout requires Camera360 mosaics with three sibling camera images")
            with Image.open(sibling) as image:
                views.append(image.convert("RGB"))
        # Camera 2 faces forward; opposite rotations place both skies outward.
        canvas.paste(ImageOps.fit(views[1],(width,top),centering=(.5,.5)),(0,0))
        canvas.paste(ImageOps.fit(views[0].transpose(Image.Transpose.ROTATE_90),(width//2,bottom)),(0,top))
        canvas.paste(ImageOps.fit(views[2].transpose(Image.Transpose.ROTATE_270),(width//2,bottom)),(width//2,top))
        return canvas
    if layout != "mosaic":
        raise ValueError("Unknown camera layout")
    with Image.open(path) as image:
        image.load()
        return ImageOps.pad(image.convert("RGB"), (width, width * 3 // 4 // 2 * 2), color="black")


def timelapse(source: Path, output: Path, *, hours=24, interval=120, fps=12,
              width=1280, end=None, ffmpeg="ffmpeg", selected=None, layout="mosaic"):
    from PIL import ImageDraw
    if layout not in ("mosaic", "portrait"):
        raise ValueError("Unknown camera layout")
    if layout == "portrait" and width % 18:
        raise ValueError("Portrait width must be a multiple of 18 (e.g. 1080 or 540)")
    if not source.is_dir():
        raise ValueError(f"Camera source unavailable: {source}")
    if not (0 < hours <= 168 and interval >= 60 and 1 <= fps <= 60 and 320 <= width <= 1920 and width % 2 == 0):
        raise ValueError("Use 0<hours<=168, interval>=60s, fps 1..60, and even width 320..1920")
    end = end or datetime.now(timezone.utc) - timedelta(minutes=3)
    chosen = selected if selected is not None else frames(source, end, hours, interval)
    if len(chosen) < 2:
        raise ValueError("Fewer than two camera frames; keeping previous timelapse")
    if len(chosen) > 3000:
        raise ValueError("More than 3000 frames; increase the sampling interval")
    output.mkdir(parents=True, exist_ok=True)
    cache = output / ".frames"
    cache.mkdir(exist_ok=True)
    kept, skipped = [], []
    with tempfile.TemporaryDirectory(prefix=".timelapse-", dir=output) as tmp:
        tmp = Path(tmp)
        for stamp, path in chosen:
            try:
                stat = path.stat()
                key = hashlib.sha256(f"v1|{path.resolve()}|{stat.st_size}|{stat.st_mtime_ns}|{width}".encode()).hexdigest()
                if layout == "portrait":
                    dependencies = [path.with_name(path.name.replace("_mosaic.jpg",f"_cam_{n}.jpg")) for n in (1,2,3)]
                    key = hashlib.sha256(json.dumps(["portrait-v3",key,[(p.stat().st_size,p.stat().st_mtime_ns) for p in dependencies]]).encode()).hexdigest()
                cached = cache / f"{key}.jpg"
                if not cached.is_file():
                    canvas = compose_frame(path,width,layout)
                    draw = ImageDraw.Draw(canvas)
                    if layout == "portrait":
                        from PIL import ImageFont
                        try:
                            font = ImageFont.truetype("DejaVuSans.ttf",width//36)
                        except OSError:
                            font = ImageFont.load_default()
                        bar_top = canvas.height - width//10
                        draw.rectangle((width//24,bar_top,width*23//24,canvas.height-width//24),fill="black")
                        draw.text((width//18,bar_top+width//120),stamp.strftime("%Y-%m-%d %H:%M UTC"),fill="white",font=font)
                    else:
                        draw.rectangle((0, 0, width, 28), fill="black")
                        draw.text((8, 6), stamp.strftime("%Y-%m-%d %H:%M:%S UTC") + " | sampled timelapse", fill="white")
                    canvas.save(tmp / "cache.jpg", format="JPEG", quality=85)
                    (tmp / "cache.jpg").replace(cached)
                shutil.copyfile(cached, tmp / f"{len(kept):06d}.jpg")
                kept.append({"time_utc":stamp.isoformat(), "file":str(path)})
            except (OSError, ValueError) as exc:
                skipped.append(str(path))
                log.warning("Skipping unreadable image %s: %s", path, exc)
        if len(kept) < 2:
            raise ValueError("Fewer than two readable frames; keeping previous timelapse")
        video = tmp / "latest.mp4"
        subprocess.run([ffmpeg, "-nostdin", "-hide_banner", "-loglevel", "error", "-y",
                        "-framerate", str(fps), "-i", str(tmp / "%06d.jpg"),
                        "-c:v", "libx264", "-threads", "2", "-preset", "veryfast",
                        "-crf", "25", "-pix_fmt", "yuv420p", "-movflags", "+faststart", str(video)],
                       check=True, timeout=1200)
        manifest = {"generated_utc":datetime.now(timezone.utc).isoformat(), "source":str(source),
                    "start_utc":kept[0]["time_utc"], "end_utc":kept[-1]["time_utc"],
                    "hours":hours, "interval_s":interval, "fps":fps, "width":width, "layout":layout, "frames":kept, "skipped":skipped,
                    "note":"Uniform playback of sampled images; gaps are not real-time durations."}
        metadata = tmp / "latest.json"
        metadata.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
        video.replace(output / "latest.mp4")
        metadata.replace(output / "latest.json")
        # Only our content-addressed generated thumbnails, never source images.
        cutoff = datetime.now(timezone.utc).timestamp() - 8 * 86400
        for cached in cache.glob("*.jpg"):
            if re.fullmatch(r"[0-9a-f]{64}\.jpg", cached.name) and cached.stat().st_mtime < cutoff:
                cached.unlink()
    return manifest


def build_leg(source: Path, output: Path, *, interval=120, fps=12, width=1280, now=None, ffmpeg="ffmpeg"):
    """UTC-day products plus a chronological full-leg concatenation."""
    if not source.is_dir():
        raise ValueError(f"Camera source unavailable: {source}")
    now = now or datetime.now(timezone.utc)
    ready = now - timedelta(minutes=3)
    folders = sorted(p for p in source.iterdir() if p.is_dir() and re.fullmatch(r"\d{8}", p.name))
    if not folders:
        raise ValueError("No dated source directories; keeping previous products")
    from .build import atomic_write
    for folder in folders:
        start = datetime.strptime(folder.name, "%Y%m%d").replace(tzinfo=timezone.utc)
        end = min(start + timedelta(days=1), ready)
        if start >= end:
            continue
        selected = frames(source, end, interval=interval, start=start)
        if len(selected) < 2:
            log.warning("%s: fewer than two frames; daily product not replaced", folder.name)
            continue
        signature = hashlib.sha256(json.dumps([str(source.resolve()),interval,fps,width,end == start + timedelta(days=1),
            [(str(p),p.stat().st_size,p.stat().st_mtime_ns) for _,p in selected]]).encode()).hexdigest()
        target = output / "days" / folder.name
        meta = target / "latest.json"
        previous = json.loads(meta.read_text()) if meta.is_file() else {}
        if previous.get("input_signature") == signature and (target / "latest.mp4").is_file():
            continue
        result = timelapse(source, target, interval=interval, fps=fps, width=width,
                           end=end, ffmpeg=ffmpeg, selected=selected)
        result.update(utc_day=folder.name, complete_day=end == start + timedelta(days=1), input_signature=signature)
        atomic_write(meta, json.dumps(result, indent=2))
    days = sorted((output / "days").glob("????????/latest.json"))
    if not days:
        raise ValueError("No daily products available to stitch")
    inputs = []
    for meta in days:
        data = json.loads(meta.read_text())
        if data.get("source") != str(source) or data.get("fps") != fps or data.get("width") != width:
            raise ValueError("Daily products have mixed sources/settings; use a separate output directory")
        inputs.append({"utc_day":meta.parent.name,"signature":data.get("input_signature"),
                       "start_utc":data["start_utc"],"end_utc":data["end_utc"],
                       "complete_day":data.get("complete_day",False),"file":str(meta.with_suffix(".mp4").resolve())})
    manifest = output / "full-leg.json"
    old = json.loads(manifest.read_text()) if manifest.is_file() else {}
    if old.get("days") != inputs or not (output / "full-leg.mp4").is_file():
        with tempfile.TemporaryDirectory(prefix=".stitch-", dir=output) as tmp:
            tmp = Path(tmp)
            listing = tmp / "inputs.txt"
            listing.write_text("\n".join("file '"+d["file"].replace("'", "'\\''")+"'" for d in inputs), encoding="utf-8")
            video = tmp / "full-leg.mp4"
            subprocess.run([ffmpeg,"-nostdin","-hide_banner","-loglevel","error","-y",
                            "-f","concat","-safe","0","-i",str(listing),"-c","copy",
                            "-movflags","+faststart",str(video)], check=True, timeout=1200)
            video.replace(output / "full-leg.mp4")
        atomic_write(manifest,json.dumps({"generated_utc":now.isoformat(),"days":inputs,
                    "note":"Available UTC days concatenated in order; missing days/time gaps are not filled."},indent=2))
    return inputs


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path, help="Leg directory containing YYYYMMDD folders")
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--interval", type=int, default=120)
    parser.add_argument("--fps", type=int, default=12)
    parser.add_argument("--width", type=int, default=1280)
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO)
    result = build_leg(**vars(args))
    print(f"Timelapse: {len(result)} UTC days stitched into full-leg.mp4")


if __name__ == "__main__":
    main()
