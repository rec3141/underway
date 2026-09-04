# underway

Real-time dashboards for underway (flow-through and meteorological) data on
research vessels. Each ship's acquisition system writes a different format, so
each has its own pipeline; what they share is the goal: a page on the ship's
intranet that shows the last few hours to the last few years of the record,
updated every few minutes, with no dependence on the satellite link.

| directory | ship | acquisition system | stack | years |
|---|---|---|---|---|
| [`HEALY/`](HEALY/) | USCGC *Healy* | MetAcq (UDP multicast) | R, ggplot, ffmpeg | 2016 |
| [`AMUNDSEN/`](AMUNDSEN/) | CCGS *Amundsen* | ACSD daily CSV files | Python, SQLite, Plotly.js | 2025– |

## HEALY

R scripts that receive MetAcq's UDP stream, plot rolling windows of the
flow-through variables, compute spatial clusters, and assemble time-lapse video
from the aloft camera. See [`HEALY/README.md`](HEALY/README.md).

## AMUNDSEN

An incremental Python pipeline over the ship's `ACSD_YYYYMMDD.csv` files —
ingest into per-leg SQLite stores, derived variables and a multivariate
"surprise" score, then a single-page dashboard rendered in the browser with a
locally served Natural Earth basemap. Runs from two systemd units: a timer that
rebuilds every 10 minutes and a small static server. See
[`AMUNDSEN/README.md`](AMUNDSEN/README.md) for setup, operation, and how it
copes with instrument columns that change from leg to leg.

## Layout conventions

- One top-level directory per ship. Anything ship-specific — paths, column
  names, credentials handling — lives inside it.
- Data, caches, databases and credentials are never committed; see
  [`.gitignore`](.gitignore). Each pipeline documents where it expects them.
- Large static assets needed offline (a bundled Plotly, basemap GeoJSON) *are*
  committed, because the ships cannot fetch them on demand.
