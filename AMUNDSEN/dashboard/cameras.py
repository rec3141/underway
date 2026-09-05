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


def frames(source: Path, end: datetime, hours=24, interval=120):
    """Read only date folders in the requested UTC window; one frame per bin."""
    start = end - timedelta(hours=hours)
    selected = {}
    day = start.date()
    while day <= end.date():
        folder = source / day.strftime("%Y%m%d")
        if folder.is_dir():
            # All-sky files are direct children; 360 mosaics are in HHMMSS dirs.
            for pattern in ("*_11.jpg", "*/Camera360_*_mosaic.jpg"):
                for path in sorted(folder.glob(pattern)):
                    match = STAMP.fullmatch(path.name)
                    if not match:
                        continue
                    stamp = datetime.strptime(match[1], "%Y%m%d%H%M%S").replace(tzinfo=timezone.utc)
                    if start <= stamp <= end:
                        selected.setdefault(int(stamp.timestamp()) // interval, (stamp, path))
        day += timedelta(days=1)
    return sorted(selected.values())


def timelapse(source: Path, output: Path, *, hours=24, interval=120, fps=12,
              width=1280, end=None, ffmpeg="ffmpeg"):
    from PIL import Image, ImageDraw, ImageOps
    if not source.is_dir():
        raise ValueError(f"Camera source unavailable: {source}")
    if not (0 < hours <= 168 and interval >= 60 and 1 <= fps <= 60 and 320 <= width <= 1920 and width % 2 == 0):
        raise ValueError("Use 0<hours<=168, interval>=60s, fps 1..60, and even width 320..1920")
    end = end or datetime.now(timezone.utc) - timedelta(minutes=3)
    chosen = frames(source, end, hours, interval)
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
                cached = cache / f"{key}.jpg"
                if not cached.is_file():
                    with Image.open(path) as image:
                        image.load()
                        canvas = ImageOps.pad(image.convert("RGB"), (width, width * 3 // 4 // 2 * 2), color="black")
                        draw = ImageDraw.Draw(canvas)
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
                    "hours":hours, "interval_s":interval, "fps":fps, "frames":kept, "skipped":skipped,
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path, help="Leg directory containing YYYYMMDD folders")
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--hours", type=float, default=24)
    parser.add_argument("--interval", type=int, default=120)
    parser.add_argument("--fps", type=int, default=12)
    parser.add_argument("--width", type=int, default=1280)
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO)
    result = timelapse(**vars(args))
    print(f"Timelapse: {len(result['frames'])} frames, {result['start_utc']} to {result['end_utc']}")


if __name__ == "__main__":
    main()
