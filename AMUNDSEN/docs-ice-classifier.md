# Experimental region classifier

Work in progress, **not enabled on the live dashboard**. No new models are
downloaded and no images are sent off-host. Runtime uses NumPy and Pillow on
CPU; scikit-learn is needed only for training and t-SNE.

Two outputs are supported: total visible ice area (%) and three constituent
area percentages: thin/new ice, broken ice/rubble, consolidated ice. The three
sum to total ice. Open water and unknown complete the area budget. These are
rough visual categories, not measured thickness, age or perspective-corrected
sea-ice concentration. Frost flowers belong to their underlying new ice;
broken floes take precedence over snow cover to prevent double-counting.

## Teacher and review

`tools/ice-overnight.py --regions-only --structured --hours 4 --output DIR`
uses the already available local Qwen3.5-9B. It explicitly assesses fog, wet
lens, blur, darkness and glare. No coupon or human cluster labels are supplied.
Scene 34 is excluded from training following Eric's visibility objection.
Other degraded, uncertain, unusable or low-confidence teacher labels are also
excluded. The script retains raw responses and finish reasons.

Run `tools/ice-region-explorer.py --output DIR` **before** the batch to produce
a fixed, reproducible t-SNE layout of all 61 reviewed 30-degree regions.
`DIR/regions.html` refreshes every minute and is regenerated after each
response. Click points for source/cutout/response; hover for previews. Colour
by total ice, individual types, unknown or visibility. Can be opened directly
from Downloads; no HTTP server or network fetch required. These experimental
tools currently reference Eric's local Downloads and 2025 SSD archive.

The monitored worker pauses between requests, unloads to cool at CPU >=88°C
or GPU >=78°C, resumes below CPU <=75°C and GPU <=65°C, and interrupts
inference at CPU >=95°C or GPU >=83°C. Three interruptions/errors stop the
run. These are conservative experiment limits, not hardware damage thresholds.
Gemma may remain loaded; only the worker's `ice-qwen35` alias is unloaded.
The Linux LM Studio Qwen backend is restricted to four logical CPUs; Gemma's
CPU affinity is untouched. Compatible structured results resume on restart.

## Train and evaluate

```
python tools/train-ice-classifier.py --structured --results DIR/results.json --output MODEL_DIR
```

Requires >=15 accepted scenes across >=5 days. Exports JSON trees, not pickle
or executable model objects. Five-fold whole-day holdout measures **agreement
with Qwen, not accuracy against human ground truth**. The initial total-only
baseline on 48 scenes/27 days had 28.4 percentage-point held-out MAE and 41.7%
three-bin agreement. This is inadequate for deployment. It took ~7–8 ms per
prepared region here, excluding source JPEG decoding. Structured training is
queued after the new teacher run, not claimed complete.

Runtime visibility screening is currently heuristic (darkness, missing
contrast/edges), not a trained fog/wet-lens classifier. Smooth ice/water can
trigger it; textured water droplets can evade it. It must not be represented
as reliable optical-quality detection. Independent human review and more
examples are needed before enabling.

## Optional dashboard hook, after review

Set `UNDERWAY_ICE_MODEL=/absolute/path/model.json` and
`UNDERWAY_ICE_SOURCE=/archive/2025_LEG_04` in the dashboard environment.
The archive must have `YYYYMMDD/HHMMSS/Camera360_*_cam_3.jpg` files.
Only the reviewed 3648x2052 camera-3 geometry is accepted. A different season,
camera placement, or image size needs a footprint review before deployment.

Every build adds up to 20 uncached predictions, newest first, with a five-second
inference admission budget. Directory enumeration and decoding can exceed
that time. Results are cached by model content, absolute source path, size and
mtime. No GPU, Qwen, or sklearn is used in dashboard inference. Exact UTC
minute bins align images to dashboard samples, without forward filling missing
minutes. Rejected images leave gaps in both total and type plots. Raw flags and
candidate estimates remain in `cache/ice-predictions.json` for audit. Optional
source/model failures do not stop normal dashboard updates.

Implementation attribution: Codex (GPT-6 Astra).

## Full 2025 leg embedding

`tools/ice-full-leg-tsne.py --source ARCHIVE --output DIR --annotations RESULTS`
processes every camera-3 photo, not a sample. Features are committed per image
to SQLite and reused on restart unless source size/mtime or feature version
changes. Corrupt or unreviewed-dimension images are listed in `excluded.json`.
All accepted feature vectors enter the Barnes–Hut t-SNE fit (seed 42).
Preview JPEGs are separate, loaded only when needed by the browser; the page
does not embed the entire archive. Scroll to zoom and click for source/region.
Annotations match by source-relative filename, never by point proximity.

The supervising process pauses its own worker at CPU >=88°C / GPU >=80°C and
resumes below CPU <=75°C / GPU <=68°C. It never stops the dashboard, Gemma or
VLC. The launched user service additionally limits CPU to one core-equivalent,
memory to 8 GB and runtime to 24 hours. A timeout/OOM can interrupt the fit,
but cached feature extraction is resumable. `progress.json` reports the stage.
This does not authorize or launch Qwen for every photo. A `linked-explorers.json`
list in the teacher output directory refreshes the full-leg annotations when
that teacher report is next regenerated.

## 600 × 300 duplicate and representative review

The feature extractor accepts an experimental resolution without changing the
240 × 120 runtime default. `ice-full-leg-tsne.py --feature-size 600 300` includes
resolution in cache fingerprints and layout metadata. Use a distinct output
directory; the new vectors are not interchangeable with existing trained models.

`ice-projections.py` adds UMAP and exact Euclidean Ward/single linkage trees to
each 2-D projection. UMAP/dbMAP dependencies live in the isolated local
`~/.local/share/underway-ice-venv`; the dashboard environment is untouched.
The installed dbMAP 1.2.0.4 required replacing its two obsolete `np.int` uses
in diffusion.py with `np.int64`. Its bundled layout API is incomplete; the
attempted compatibility variant uses its Diffusor and a modern UMAP layout.
The first full-data attempt produced an invalid diffusion basis and was not
published as a successful dbMAP embedding. Errors remain in projections.json;
other projections are still published when this optional stage fails.

`ice-cluster-representatives.py --k 64` cuts the t-SNE Ward tree at exact k,
then selects a central point and greedily separated representatives, up to
three distinct photos per cluster. Tiny clusters are not padded with duplicates.
The portal exposes the fixed cut separately from distance-height cuts.
`ice-overnight.py --regions-only --structured --queue QUEUE --hours 4` runs
the blind local teacher on that queue. It records cluster membership for audit,
not in the prompt. No labels automatically propagate from representatives.

The browser supports half-size points, mean RGB/brightness colouring, dragging,
lasso selection and additive human label sets. Human label exports are keyed by
source file and exact region geometry, and can be imported into the duplicate.
Autosave is browser-local and export remains the portable backup. The temperature
page tool samples every 30 seconds through a user timer, with seven-day retention.

## Tree proofsheet workflow

`ice-tree-review.py --portal PORTAL` adds `tree-review.html`, initially cut into
16 actual Ward subtrees of the t-SNE hierarchy. Each proofsheet includes every
remaining photo in the current node (lazy-loaded thumbnails, no sampling).
Down replaces that node with its immediate children and opens the first pending
child. Up replaces overlapping pending siblings with their parent while excluding
all previously labeled photos. Left/right move among pending subsets without
labeling. L opens a label dialog; Enter applies the label to the entire displayed
batch and advances. Undo restores the last labeled batch and prior navigation.
Labels/progress autosave in browser storage; export/import provides portable
labels compatible with the projection portal. This page deliberately never
auto-refreshes while labeling. Qwen stays stopped until explicitly resumed.

The Qwen worker now tolerates disappearing `/proc` entries and threads while
setting CPU affinity. Its CPU cooldown threshold is **95°C averaged over the
trailing 120 seconds**, time weighted; startup uses available observed duration
without zero-padding. An instantaneous 100°C emergency cutoff remains. GPU limits
are unchanged. Cooling still waits for instantaneous CPU <=75°C / GPU <=65°C.
The worker logs the CPU average as an additional telemetry column and retries
empty/truncated responses on resume. Other CPU-only projection workers retain
their existing conservative instantaneous pause thresholds.
