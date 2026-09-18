# Experimental live sampling

Branch: `feature/live-sampling`. The standalone adviser reads the main dashboard's
`/api/live` and published cast/window JSON. It owns no CTD socket and does not
start a chat bot or a second acquisition listener. Open
`http://underway.local/livesample.html` after routing the experiment below.

## Run locally

Install `pip install '.[sampling]'` in a separate Python environment, then run:

```sh
python -m dashboard.livesample --root /data/underway_server/www --state-dir /path/to/private/livesample-state --port 8043
```

The page is also available directly at `http://127.0.0.1:8043/livesample.html`.
It uses the main webroot's Plotly, stylesheet, and manifest. The default live API
is `http://127.0.0.1:8042/api/live`; override it with `--live-url` for replay.
The main dashboard listener probes `10.0.0.22:49160–49168` by default and
accepts the first SeaSave converted-data XML stream. The live-sampling page
shows the active endpoint, or the ports being probed while it waits.
It listens to one port at a time. Once a converted-data field header arrives,
the connection stays open through arbitrarily long acquisition pauses; only
socket closure, a socket error, shutdown, or a source change ends that connection.
Ports without a recognized header are still subject to a five-second discovery timeout from connection establishment.
An in-water cast stays current through silent intervals until surface dwell,
a source change, or a changed field list ends it.
`underway-livesample.user.service.example` provides an isolated user service.
The deployed app directory needs `dashboard/{__init__,config,alerts,surprise,
sampling,sampling_flow,livesample}.py` and the three `static/livesample.*` files. The private
state directory retains the TSV inventory, settings, and Telegram send history.

## Ship URL routing

Place this before the main reverse proxy in the `http://underway.local` Caddy block:

```caddyfile
@sampling path /livesample.html /static/livesample.* /api/livesample /api/livesample/*
handle @sampling {
    reverse_proxy 127.0.0.1:8043
}
handle {
    reverse_proxy 127.0.0.1:8042
}
```

`livesample-route.py` can install/remove the equivalent narrow route through the
local Caddy admin API, without editing the main deployment checkout. The helper
only changes its own identified route and uses a conditional write (or one atomic route insertion/deletion on Caddy without ETag support). Runtime
configuration lasts until Caddy reloads its file; the optional user route timer
reconciles that one route while the sampling service is active. For a permanent
file-based installation, use the Caddy fragment above.

## Sample inventory

A selected-leg bottle template is downloadable from the page. It lists every
archived CTD/TM bottle firing in the checked legs, one row per bottle, with
`leg`, full `cast` ID, `bottle`, and a blank `sampled` column. Mark `1` for each
bottle sampled; blank and `0` are ignored. Invalid markers are rejected. The
checked legs determine the download even before reference settings are applied.
`GET /api/livesample/template?leg=2026_LEG_03` serves the TSV.

A custom tab-separated file without `sampled` has one sampled bottle per row:

```tsv
leg	cast	bottle
2026_LEG_03	035	1
2026_LEG_03	035	12
```

A full cast ID (`2026_LEG_03:CTD_035`) can replace the cast number. Ambiguous
numbers are rejected. `pressure` in dbar can replace `bottle`; firing pressure
is read from the archived bottle log, or calculated from its depth with the
same UNESCO relation as the main viewer. Interpolation is limited to the
profile and gaps of at most 5 dbar. Rows outside selected legs remain stored
but do not count as matched samples. Duplicate levels count once.

For collected flow-through samples, provide `source=flow`, an ISO timestamp
with timezone, `temperature` in °C, and practical `salinity`. Those observed
properties join the sampled coverage inventory. Uploading a file replaces the
saved inventory; changing other settings preserves it. An empty TSV clears it.
The page offers a template. An uploaded inventory is shared by all page users.

## Methods and interpretation

All priority distances use temperature and practical salinity, centered on
reference medians and divided by reference IQRs. Invalid sensor values are
excluded. Without matched samples, coverage uses the reference median as an
initial anchor; the page reports this condition.

- **Coverage:** farthest point from collected water, with distances updated
  after each proposed target to choose a diverse batch.
- **Rarity:** distance to the tenth nearest reference level (or all available
  levels for smaller references).
- **Gradient:** strength of the vertical T/S change per dbar.
- **Surprise:** the existing multiscale temporal scorer, using actual minute
  medians of the downcast. Short casts lack the required 11-minute warm-up.
  Flow-through uses the dashboard's published surprise score and its existing
  feature set, including air temperature.
- **Hybrid:** 60% coverage, 25% rarity, 15% gradient, normalized by candidate
  90th percentiles for CTD selection.

CTD candidates are two-dbar medians of the observed downcast. Minimum spacing
and a 1–24 target budget constrain the proposed stops. The requested number of
distinct observed levels is always shown when that many levels exist; spacing is
relaxed only when it would otherwise leave the plan short. During ascent the
complete downcast plan remains visible. A stalled or completed cast retains a
display-only plan and never generates an alert. Pressure and depth remain
explicitly labeled. These targets do not operate the rosette or account for its
already-fired bottles; set the target budget to the bottles available.

The live record may contain the deck soak and operational cast when the surface
turnaround is shorter than the acquisition server's cast-closing interval. After
an initial excursion to at least 15 m returns to 5 m or shallower, sampling waits
for a renewed descent of at least 2 m. The chart, scores, target plan, and alerts
then use only observations after that shallow turning point. Before the cycle is
complete, the chart and alerts remain paused. A continuous descent reaching 30 m
is treated as a direct cast so a feed started after the soak can still recover.

The CTD histogram compares every candidate with archived bottle firings from the
selected legs. Its value is the candidate's nearest-bottle T/S distance,
standardized against the leave-one-out nearest-neighbour distances of all valid
previous bottles. Target lines carry rank, method score, and that novelty
z-score. With fewer than two valid bottles, reference levels provide a clearly
reported fallback distribution.

The flow-through threshold is the reference 90th percentile for coverage or
rarity, with floors of 0.25 and 0.1 standardized units. Gradient uses a T/S change
of 0.5 standardized units over at least one minute; surprise uses 3 −log10 p;
hybrid uses 1.0 after normalization to those thresholds. The newest complete
temperature, salinity, and flow observation must be at most three minutes old,
flow must be at least 0.5 V, and the live leg must be selected. A newer T/S bin
whose flow field is still pending does not mask that complete observation. These
experimental thresholds need cruise-specific review.

The two plots use genuine [scikit-learn t-SNE](https://scikit-learn.org/stable/modules/generated/sklearn.manifold.TSNE.html)
and [UMAP](https://umap-learn.readthedocs.io/en/latest/api.html), seeded with 42.
They use up to 3,000 archive levels, thinned equally per selected rosette cast,
plus matched sampled levels and located archived bottle firings. The bottle toggle marks those firings independently of the uploaded sample inventory. All archive channels are available for colouring; missing readings stay grey. The five candidate embedding properties are
T/S, log fluorescence, oxygen (converted to µM), and transmission. Optional
properties require 80% coverage; remaining gaps are filled with the reference
median. Nominal/raw MVP sensors are excluded. Plots visualize structure, and
are never used to rank sampling priorities. Coordinates can change when the
reference legs, profiles, or sample inventory changes. Both plots have square axes and use pan/scroll-zoom interaction.

## Chart controls

The CTD chart uses the main Single-view parameter arrangement: selected channels
have independent axes, and the movable Chart divider chooses upper/lower axes.
The method, target count and minimum spacing sit beside the cast. Horizontal
lines mark its proposed upcast targets. Missing channels remain grey.

The flow-through chart uses the Lab variable chips and published timespan picker.
Its own method, target-count, and time-spacing controls recalculate a ranked set
for the selected window without changing the saved CTD or Telegram settings.
The requested number of distinct healthy observations is shown when available;
spacing is relaxed only to fill the requested count. Vertical chart lines and a
second novelty histogram show those hypothetical picks. They are not sent
messages or collected samples. Changing chart controls does not send
notifications. `GET /api/livesample/flow?window=6h&algorithm=coverage&count=6&spacing=15`
supplies the ranked picks and histogram distribution for a published window.

## Telegram

Enable the checkbox and apply settings to start owner notifications via the
existing `UNDERWAY_TELEGRAM_TOKEN`/`TELEGRAM_KEY`/`TELEGRAM_BOT_TOKEN` and
`TELEGRAM_ID` environment settings. No destination or credential is accepted
from the browser. CTD plans are deduplicated at the configured spacing and sent
at most every two minutes; flow recommendations at most every fifteen minutes.
Disabling the checkbox stops future notifications. Delivery errors are shown on
the page. Alerts continue with the browser closed. Tests mock delivery.

## Checks

```sh
python -m unittest discover -s tests -p 'test_livesample*.py' -v
python -m unittest discover -s tests -p 'test_sampling*.py' -v
node tests/livesample-ui.test.cjs
```
