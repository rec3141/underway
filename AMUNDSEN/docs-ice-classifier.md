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
