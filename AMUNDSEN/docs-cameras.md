# Camera products (optional, experimental)

Install `pip install -e '.[cameras]'` and ffmpeg with its libx264 encoder.
These jobs are separate from the minute-by-minute dashboard build. No imagery
is sent to external inference services.

## Timelapse

```
python -m dashboard.cameras --source /mnt/ship/Data/Camera_360/2026_LEG_03 --output /data/underway/camera360
```

One product per UTC calendar day (`days/YYYYMMDD/latest.mp4` and `.json`),
then `full-leg.mp4` stitches the available days in order without re-encoding.
Defaults: one image per two-minute bucket, 12 fps, 1280px wide H.264/yuv420p.
Supports 360 mosaic directories and all-sky daily directories. Daily windows
are midnight-inclusive to next-midnight-exclusive. The current day's end is
three minutes behind UTC now to avoid images still being written; it remains
partial until the UTC day ends. Every frame displays its capture UTC; missing intervals
are compressed, not presented as continuous elapsed time. JSON lists original
frame paths and capture times. No matching frames or an encoder failure leave
the preceding product intact. A corrupt frame is skipped; fewer than two
readable frames fail the day's build. Daily encodes are bounded to 3000 frames
and a 20-minute encoder timeout. Output is local; source images are untouched.
Unchanged daily inputs/settings skip encoding; only changes trigger stitching.
A closed UTC day is not a guarantee of continuous 24-hour source coverage.
Resized frames are cached locally by source path/size/mtime/width, so hourly
runs need not reread unchanged originals. Generated cache files older than
eight days are pruned after successful builds; they can be regenerated.

Review `deploy/camera.env.example` and copy it to
`~/.config/underway/camera.env`. The confirmed destination for 2026 Leg 3 is
`/mnt/ship/Share/2026/2026_LEG_03/Pictures/Timelapse`. The daily
sync deliberately refuses to create a missing destination directory. It
copies the dated daily MP4/JSON files and full-leg MP4/JSON into the dedicated
configured `Timelapse/` directory, with no `--delete`. Cached JPEGs and hidden
working files are excluded. Old products retain their actual source dates;
they are not relabelled as new days when camera acquisition stops.
Use distinct output/destination folders for different legs or cameras.

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
  --source '/media/cryomics/T7 Shield/Amundsen/Camera_360/2025_LEG_04' \
  --config tools/ice-2025-fixed-pilot.json \
  --calibration tools/ice-2025-reference-patches.json --output /tmp/ice-pilot
```

Open `index.html`: paired crop/mask previews, fraction bars, threshold
sensitivity, and CSV/JSON for plotting. The pilot uses camera 3 from five
inspected dates spanning open water, broken ice, consolidated ice, fog and
lens droplets. A small reference-colour LDA classifier fits manually chosen
water and ice patches; these fitting images are **not independent validation**.
The two contaminated examples are explicitly excluded in the config. Automatic
fog/droplet rejection is not yet implemented; other bad scenes may slip through.
The config requires a separate normalized sea-only rectangle for every image;
do not reuse it on different viewpoints without inspecting them. Exclude ship,
land, sky and wake. Without `--calibration`, the simpler Otsu brightness
baseline remains available, including the older drone-photo pilot config.
An explicit 1–254 decision threshold can override either method. Cyan means
candidate ice. Dark/low-contrast scenes get no estimate; glare, thin ice,
shadows and homogeneous bright scenes can still produce misleading results.

Outputs are unweighted fractions of crop pixels, **not calibrated geographic
sea-ice concentration**. ±15 threshold sensitivity is not uncertainty or a
confidence interval. Do not use for navigation or scientific inference. EXIF
times are retained without assuming a timezone. The inspected 2025 JPEGs have
incorrect EXIF years (2034); use Camera360 filename UTC, not EXIF, for plotting.
Input images are not corrected
for viewing geometry; different crops cannot be compared as equal footprints.
Review masks before exploratory plotting. The first reference-colour result
still assigns some open-water pixels to ice and misses some consolidated ice;
this is an inspectable starting point, not a ready-to-publish automatic series.
More varied annotated images and held-out comparisons are needed next.

Background: [shipborne ice-segmentation study](https://arxiv.org/abs/2409.06641)
demonstrates why image geometry and manual comparisons matter; its reported
performance does not apply to this simple baseline.

## Portrait layout preview

The Python `dashboard.cameras.timelapse` helper accepts `layout="portrait"`
and `width=1080` for a 1080×1920, 9:16 video. Camera 2 is centre-cropped
across the upper third; cameras 1 and 3 rotate in opposite directions into
the lower two-thirds, with sky at the outside edges (top-to-bottom ratio 1:2).
The side views are centre-cropped to fit their taller panels. Original images are
unchanged. All three individual camera JPEGs must accompany each mosaic;
incomplete sets are skipped. Use a separate output directory for previews.
Width must be a multiple of 18 (540 is a smaller preview option).
The UTC timestamp is centred near the bottom edge, with a padded black
background fitted to the text bounds.

This is an opt-in Python preview, not a change to the scheduled daily/leg
layout. The normal CLI and hourly job still produce landscape mosaics.

## Checks

### Texture exploration

`tools/ice-texture.py` requires Pillow, NumPy, SciPy and scikit-learn. Run with
`--source /path/to/2025_LEG_04 --config tools/ice-2025-fixed-pilot.json
--sample-leg --clusters 12 --output /path/to/Downloads/ice-texture`.
It includes all five reference scenes regardless of the old exclusion flags,
plus two time-spaced frames per archive day, with up to 24 native 100×100 tiles
per scene. The fixed ROI is provisional for additional scenes. Inspect for
land, ship, darkness and optical contamination before assigning ice labels.

The self-contained HTML opens offline without a server. Click points or tiles,
filter by group, compare source-scene colours, name groups, then download labels.
Labels are not saved until exported. Standardized contrast-normalized texture
features feed both t-SNE and k-means; k-means does not cluster the distorted
2-D embedding. Groups are not physical ice types or confidence estimates.
For supervised evaluation, split by scene/day, not adjacent tiles. Eric's five
scene estimates (0,30,95,100,100%) are approximate scene references and must
not be assigned to every tile as if they were tile-level labels.

Use `--brightness-weight 0.5 --labels /path/to/ice-texture-labels.json`
to explore texture plus luminance while preserving prior labels by source
file and exact tile bounds. Brightness includes mean, spread and five
quantiles on unnormalized grayscale pixels. Each feature is standardized;
the brightness block is dimension-balanced against the texture block before
applying its explicit weight (0 = texture only). New cluster numbers do not
inherit old group names. The previous-label filter helps inspect how an old
ambiguous group splits. Labels such as “probable” or “mixed” remain uncertain,
not hard ice/water truth. Exposure and glare can confound brightness.
Dots default to grayscale mean brightness; group and scene colours remain
available. Hover a dot for its tile, or click to see the full source frame
with a red tile outline and dashed cyan crop outline. Source previews are
scaled, but boxes use original image coordinates. `--render-only` refreshes
previews/UI from existing `tiles.json` without recomputing group assignments.
Export unsaved browser labels before reloading a report.

### Local vision review

`tools/ice-vision-review.py --input /path/to/tiles.json --output /path/to/review
--model ice-vision --limit 5` calls an already-loaded vision model through
LM Studio on **127.0.0.1:1234 only**, bypassing proxy environment variables.
It downloads nothing. Load an existing local GGUF with its matching vision
projector first; do not interrupt other GPU workloads without approval.
The report embeds crops and unreviewed model responses, with prompt, usage,
timing and completion status in `results.json`. Expert labels are not sent.
These are pretrained-model suggestions, not a trained ice model. The small
reference set cannot establish general accuracy. Check `finish_reason` for
truncation and review dark/new ice particularly carefully.
`--max-tokens 6000` raises the completion budget; load enough context for both
image/prompt and output (the pilot uses 16384). Budget and completion status
are recorded in the result. No-thinking hints depend on runtime support.

`python -m unittest discover -s tests -p test_cameras.py` covers date selection,
sampling, corrupt-image recovery, real ffmpeg encoding (when available), a
synthetic known ice fraction, invalid crops, dark-scene rejection, UTC daily
boundaries, chronological stitching and unchanged-input skip behavior.
