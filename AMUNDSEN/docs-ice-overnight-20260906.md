# Overnight camera work — 2026-09-06

## Running services

- `ice-gemma-overnight.service` (user transient): 470 unique 2025 photos, original
  96 first then three representatives from each of 128 Ward clusters, deduplicated.
  Working tree: `/tmp/amundsen-camera-rotated/AMUNDSEN`. Teacher: local Gemma 4
  26B A4B QAT Q4_0, LM Studio, reasoning off, one request at a time.
  Results: `~/Downloads/amundsen-ice-gemma-overnight/gemma4/`.
  Saved rows are resumable by filename. Eight-hour maximum; CPU 95 C trailing
  120-second average and GPU 83 C hard stop. GPU admissions below 78 C.
- `underway-camera-features.timer` (user): every two minutes, 60 new camera-3
  photos per invocation, newest first; 90-second work budget and 180-second
  systemd timeout. Source `/mnt/ship/Data/Camera_360/2026_LEG_*`.
  Cache `~/.cache/underway/camera-features.sqlite`.
- `underway-camera-train.timer` (user): every ten minutes, evaluate teacher
  distillation as results accumulate. Never overwrites the published area model
  unless all publication gates pass. Output `~/Downloads/amundsen-ice-area-classifier`.

## Chat pause

`~/.config/underway/chat-paused` disables the dashboard's automatic LLM crew at
startup. Human chat is unchanged. Remove that flag and restart the dashboard
server when Eric wants the LLM crew back. Do not auto-load chat models overnight.
The system dashboard server was restarted to apply this; Ollama Gemma was unloaded.

## Live camera products

`dashboard/camera_live.py` publishes `/data/underway/www/data/camera-track.json`
atomically, preserving the 2025 archive. Source photos are served through
`www/photos/2026_LEG_*/` links to the mounted ship archive. Source disconnection,
partial JPEGs, and unreviewed dimensions must not replace the last good product.
The browser polls once a minute, reindexes matches and redraws without reloading
the page or duplicating cards. Missing photos remain black on camera map modes.

PCA uses the fixed 2025 means/scales/axes from the 600x300 portal. New photos get
the same 89 colour/texture features, standardized and projected onto those axes;
no PCA refitting or interpolation. The original reviewed crop is retained for
compatibility. Eric's requested shifted production crop is still a separate,
not-yet-trained input version and must not reuse these labels silently.

## Lightweight models

`tools/train-camera-presence.py`: human labels from `labels-clean.json`, 13,563
photos across 28 days, 48 extra trees, depth 10. Five whole-day folds.
Any-ice accuracy 0.8965 and F1 0.9178; macro F1 across labels 0.5247.
Thin FYI, whitecap and visibility labels perform poorly. Tree inference about
0.066 ms/photo excluding feature extraction/JPEG decoding. Scores are label
presence, NOT area percentages or calibrated probabilities. Export verified
against sklearn. Model: `~/Downloads/amundsen-ice-human-classifier/model.json`.

`tools/train-camera-area.py`: 48 shallow regression trees predicting ten surface
fractions. Discards malformed budgets, unfinished/uncertain/low-confidence
responses, strong visibility artifacts, and strong contradictions with human
ice-only/water-only labels. It does not convert human presence into coverage.
Publication gates: >=50 usable rows, >=8 days, >=3 examples at >=10% for each ice
type, held-out total-ice MAE <20 percentage points and >=10% better than the
fold-specific constant predictor. These assess teacher agreement, not scientific
truth. Candidate and evaluation are retained even if publication fails.

The live updater evaluates available portable models from cached features.
It marks out-of-training inputs, darkness, predicted visibility issues, and
ambiguous presence for review. The experimental ice card/map colours leave gaps
for flagged estimates. Coverage and human presence have separate names/units.

## Model trials and open work

Initial paired subset: six photos across original Ward clusters 0,6,12,18,24,31.
Gemma completed all six at ~26–30 s/photo with valid JSON. Muse completed at least
two at ~165–193 s/photo with reasoning uncontrolled, then was stopped to investigate
Ollama `think=false`. This subset is water-heavy and does not prove type accuracy.
Results: `~/Downloads/amundsen-ice-model-comparison/`.
Gemma's fresh, uncontended overnight load is faster (~10–14 s/photo).

Ollama 0.32.3 cannot import the local Muse GGUF: its quantizer reports unknown
architecture `muse-glimmer`. The local directory import uploaded both GGUF files
to Ollama's blob store but did not create a model. No new weights downloaded.
Eric authorized updating Ollama and specifically approved the 1.43 GB (1.34 GiB)
official 0.33.3 Linux runtime download via the SOCKS proxy localhost:1080.
Staging: `/data/scratch/ollama-update-dyJLEG`.
Expected archive SHA256:
`c13cea8f3389db4145f8a6cb88d1747242a48639d7c13e3bda7c1ebdc6eebb2f`.
Use existing `/home/cryomics/bin/sudo-askpass` (Zenity) for installation, keep a
rollback runtime, preserve models and service settings, and do not disrupt LM Studio.

## Verification

Four Python camera feature/inference tests pass. Browser integration test in the
camera worktree passes native card layout, shared axes, RGB/PC modes, photo popup,
live refresh without duplicate cards, experimental presence card, and filtering.
New teacher area trainer still needs target-validation tests and publication
results monitored. Overnight worker currently stops on transport errors; no
automatic restart watchdog has yet been installed.

Attribution: Codex (GPT-6 Astra).

## Morning update

- Gemma completed all 470 unique photos at 08:28 UTC, median 12.15 s/photo;
  ten responses have review flags. No watchdog retries or thermal cutoffs.
- Both projection portals now contain 470 Gemma annotations, with model provenance
  and quality flags; four older annotations remain. No human labels were changed.
- Underway photo popups now have an SVG orange ROI outline matching the original
  1200x600, -30 degree crop, correctly scaled with the full photograph. Browser
  integration and visual inspection passed.
- Ollama 0.33.3 installed after SHA256 verification, preserving model files and
  service configuration. Rollback binary/libraries have suffix
  `.pre-0.33.3-20260906` under `/usr/local/bin` and `/usr/local/lib` respectively.
  `muse-glimmer-local` imported successfully, with vision projector; reasoning-off
  inference has NOT yet been benchmarked on this runtime. Chat remains paused.
- Diagnosed the portable area-model discrepancy without changing deployed code:
  sklearn converts inputs to float32 before evaluating splits. A diagnostic run
  doing that conversion passes the export parity assertion on all 369 accepted
  samples. Diagnostic output:
  `~/Downloads/amundsen-ice-area-classifier/float32-diagnostic-evaluation.json`.
  All 470 teacher rows were examined: 369 accepted across 28 days, 101 rejected.
  Total-ice MAE 20.992 pp versus 37.253 pp constant baseline. All six ice types
  have sufficient support, but the <20 pp publication gate still fails.
  Area estimates remain unpublished. The production dtype fix is still pending.
- The same float32/float64 comparison changed no predictions for the human model
  across all 13,565 feature rows. Live human presence remains operational.
- Three recent current-year photos took median ~231 ms each including share read,
  JPEG decode, crop, 600x300 features, and tree inference. This small timing sample
  is cache/network dependent; tree-only inference is ~0.06 ms in larger tests.
- Eight feature/model/target validation tests pass. Teacher watchdog installed;
  it stopped its timer upon successful completion. Feature and training timers
  remain active; the latter currently encounters the documented export mismatch.
