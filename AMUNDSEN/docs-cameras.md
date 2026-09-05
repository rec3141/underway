# Camera products (optional, experimental)

Install `pip install -e '.[cameras]'` and ffmpeg with its libx264 encoder.
These jobs are separate from the minute-by-minute dashboard build. No imagery
is sent to external inference services.

## Timelapse

```
python -m dashboard.cameras --source /mnt/ship/Data/Camera_360/2026_LEG_03 --output /data/underway/camera360
```

Defaults: rolling 24 hours, one image per two-minute bucket, 12 fps, 1280px
wide H.264/yuv420p MP4. Supports 360 mosaic directories and all-sky daily
directories. The default end is three minutes behind UTC now to avoid images
still being written. Every frame displays its capture UTC; missing intervals
are compressed, not presented as continuous elapsed time. JSON lists original
frame paths and capture times. No matching frames or an encoder failure leave
the preceding product intact. A corrupt frame is skipped; fewer than two
readable frames fail the build. Jobs are bounded to 3000 frames and a 20-minute
encoder timeout. Output is local; source images are untouched.
Resized frames are cached locally by source path/size/mtime/width, so hourly
runs need not reread unchanged originals. Generated cache files older than
eight days are pruned after successful builds; they can be regenerated.

Review `deploy/camera.env.example`, confirm the rolling window and actual leg
photos destination, and copy it to `~/.config/underway/camera.env`. The daily
sync deliberately refuses to create a missing destination directory. It
copies only dedicated `underway-camera-YYYYMMDD.mp4/.json` products, with no
`--delete`. The date is the UTC copy date, not a claim of calendar-day coverage.
Telemetry older than 26 hours is not copied as a new daily product.

After review/merge, install the four `deploy/underway-camera*.service/.timer`
files into `/etc/systemd/system/`, run `systemctl daemon-reload`, then enable
`underway-camera.timer` and `underway-camera-sync.timer`. They generate hourly
and copy daily at 00:20 UTC. Generation and sync share a nonblocking lock;
if busy, that scheduled invocation skips. A missed daily copy can be retried
with `systemctl start underway-camera-sync.service`. The shipped units match
this workstation's user/repository paths; change them elsewhere. Timers are
**not installed or enabled by adding these files to git**.

## Ice-estimator pilot: 2025 Leg 4

```
python -m dashboard.ice \
  --source /mnt/ship/Share/2025/2025_LEG_04/PICTURES/Drone_Amundsen_Science \
  --config tools/ice-2025-leg4-pilot.json --output /tmp/ice-pilot
```

Open `index.html`: paired crop/mask previews, fraction bars, threshold
sensitivity, and CSV/JSON for plotting. Two inspected drone images from
September 19 are the initial pilot, **not a fixed-camera-series calibration**.
The config requires a separate normalized sea-only rectangle for every image;
do not reuse it on different viewpoints without inspecting them. Exclude ship,
land, sky and wake. The exploratory baseline uses Otsu brightness segmentation
(or an explicit 1–254 brightness threshold), not a trained model. Cyan means
candidate ice. Dark/low-contrast scenes get no estimate; glare, thin ice,
shadows and homogeneous bright scenes can still produce misleading results.

Outputs are unweighted fractions of crop pixels, **not calibrated geographic
sea-ice concentration**. ±15 threshold sensitivity is not uncertainty or a
confidence interval. Do not use for navigation or scientific inference. EXIF
times are retained without assuming a timezone. Input photons are not corrected
for viewing geometry; different crops cannot be compared as equal footprints.
Review masks before exploratory plotting. More varied annotated images and the
actual 2025 fixed-camera archive are needed before automated series processing.

Background: [shipborne ice-segmentation study](https://arxiv.org/abs/2409.06641)
demonstrates why image geometry and manual comparisons matter; its reported
performance does not apply to this simple baseline.

## Checks

`python -m unittest discover -s tests -p test_cameras.py` covers date selection,
sampling, corrupt-image recovery, real ffmpeg encoding (when available), a
synthetic known ice fraction, invalid crops and dark-scene rejection.
