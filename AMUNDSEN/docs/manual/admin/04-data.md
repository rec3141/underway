# Data, builds and storage

The build reads files from the ship's shares (through the local mirror) and
a few live feeds, keeps one SQLite store per leg, and writes the site. Every
store and cache it writes is derived data: deleting it costs only the time
to rebuild.

## What the dashboard reads

Paths are relative to the Data share (`UNDERWAY_DATA_ROOT`, aboard the
mirror's `/data/ship/Data`) unless they start with `Share/`
(`UNDERWAY_SHARE_ROOT`).

| Source | Path | Used for |
|---|---|---|
| ACSD underway record | `FULL_CSV/<leg>/ACSD_YYYYMMDD.csv` | the Underway tab, the map track, the surprise score |
| Archived seasons | `Share/<year>/<leg>/ACSD_YYYYMMDD.csv` | earlier legs of the ship's own record |
| TSG files | `TSG/<leg>/tsg_convdata_YYYYMMDD.cnv` | intake flow (pump-off flag), and a provisional tail ahead of the ACSD record |
| Event log | `EventLog/<leg>/Eventlog_<leg>.xls` (or `.xlsx`; also left in `Share/<year>/<leg>/…`) | Schedule tab, map event-log layer, stations |
| CTD logbook | `Rosette/<leg>/Logs/*CTD_logbook.csv` | cast station, label and time |
| Rosette sheets | `Rosette/<leg>/Logs/RosetteSheet_*.xlsx` | bottles (cruise report) |
| Cast plots | `Rosette/<leg>/plots/*_raw_data.html` | variables the `.cnv` lacks; sole source for legs without `.cnv` |
| Bottle files | `Rosette/<leg>/Btl/*.btl` | bottle firings |
| LADCP | `Rosette/<leg>/Ladcp/stn###.lad` (and diagnostic `.png`) | **LADCP** casts and the map's current arrows |
| CTD casts | `external_proprietary/CTD/*.cnv` | rosette profiles (SBE 9 and SBE 19plus) |
| MVP tows | `MVP/<leg>/<tow>/*.m1` (and `.raw`, `.log`) | MVP tows and dips |
| Camera pictures | `Camera_360/` | timelapses and the ice camera ([Integrations](integrations.html#cameras)) |
| Intranet schedule | `http://10.0.0.2/Schedule.html` (`UNDERWAY_SCHEDULE_URL`) | Schedule tab; the last copy is kept in `db/schedule.json` for when the intranet is down |
| Intranet live page | `http://10.0.0.2/live.html` (`UNDERWAY_INTRANET`) | readings a few seconds old; recorded in `db/live_scrape/` |
| Live CTD | SeaSave on `10.0.0.22`, TCP ports 49160–49168 (`UNDERWAY_CTD_TCP`) | the live cast in the Casts tab |
| Archive 2002–2024 | `UNDERWAY_HOME/archive/acsd/…` | archive legs ([Amundsen Science's archive](archive.html)) |

### Legs and the live leg

A leg is a folder named `YYYY_LEG_NN`. The build finds them in
`FULL_CSV/` (the current season), in `Share/<year>/` (archived seasons) and
in the archive's `acsd/` tree (marked `archive`). New leg folders are found
by themselves; nothing needs to be configured at the start of a leg.

The **live** leg is the leg with the latest date in its `ACSD_YYYYMMDD.csv`
file names, ties broken by year and leg number. File modification times do
not matter. "Live" means the latest available leg, not that its data are
fresh. In ashore mode no leg is live.

```sh
cd /data/underway_server/app/AMUNDSEN
/opt/miniforge3/bin/python3 -m dashboard legs     # * live, a archive
```

If neither root exists or neither holds legs with ACSD files, `legs` and
`build` exit with status 2 and the web root is left alone.

### How the live feeds fill the gap

The ACSD files are flushed every ten minutes. Two feeds fill the minutes
after the end of the ACSD record, each as a provisional tail recomputed at
every build:

- the TSG `.cnv` file, about two minutes behind: water values and GPS
  position; every provisional row is also appended to
  `db/provisional_tsg.csv`;
- the intranet's live page, polled every few seconds by the page server and
  recorded to `db/live_scrape/YYYYMMDD.jsonl`: position, heading, depth, met
  and TSG values. Nothing in it is ever deleted.

## The local mirror

Every access to a CIFS share costs a network round trip, and the build
touches thousands of files. The build therefore reads a local copy,
refreshed by `tools/mirror-share.sh` at the start of each build.

```sh
tools/mirror-share.sh [DEST] [full|quick]     # default DEST=/data/ship, full
```

| Pass | Copies | When |
|---|---|---|
| quick | `FULL_CSV` ACSD files, event logs, TSG files | every build |
| full | also Rosette (logbooks, sheets, plots, `.btl`, LADCP), CTD `.cnv`, MVP, archived seasons in `Share/<year>/` | every tenth minute, or `UNDERWAY_MIRROR_FULL=1` |

- Only those file patterns are copied (rsync filters); the first full
  mirror is about 15 GB, mostly cast plot HTML.
- Nothing is deleted from the mirror when it vanishes from the share: a
  file that disappears upstream is usually a mount hiccup.
- rsync compares size and mtime, with 60-second timeouts so a dropped mount
  fails fast.
- One mirror at a time (`/data/ship/.mirror.lock`); a log line per tree in
  `/data/ship/mirror.log`.
- Sources default to `/mnt/ship/Data` and `/mnt/ship/Share`
  (`UNDERWAY_MIRROR_DATA`, `UNDERWAY_MIRROR_SHARE`).

The build reads the mirror because `underway.service` sets
`UNDERWAY_LOCAL=1`: `update_underway_py.sh` then points
`UNDERWAY_DATA_ROOT` and `UNDERWAY_SHARE_ROOT` at `$UNDERWAY_MIRROR/Data`
and `$UNDERWAY_MIRROR/Share` and builds into `UNDERWAY_WEBROOT`
(`$UNDERWAY_HOME/www`). Other services (the page server, the report
builder) read `UNDERWAY_DATA_ROOT`/`UNDERWAY_SHARE_ROOT` from `site.env`,
which default to the shares themselves; ashore they point at the mirror
([Running ashore](ashore.html)).

Raster tiles are never written to the share: they live in
`UNDERWAY_TILES_DIR` ([Map data](map-data.html)).

## What a build does

`python -m dashboard build --root <web root>`:

1. syncs each leg's store, parsing only new or changed day files;
2. combines the legs, coalesces position, computes derived variables, the
   pump-off flag and the surprise score;
3. writes a JSON file per time window (`data/w-*.json`) and
   `data/manifest.json`;
4. publishes the map's track as content-addressed chunks under
   `data/track/`;
5. parses new casts, the schedule, the event log, ice charts and satellite
   pictures from their caches;
6. copies `static/`, renders `index.html` and, when its sources changed,
   the manual into `manual/`.

`update_underway_py.sh` passes the page title and the footer links (the two
public Google calendars) as `--title` and `--link`.

### The web root

| Path | What |
|---|---|
| `index.html` | the page |
| `data/manifest.json` | what the page loads: legs, variables, windows, sources, layers |
| `data/w-<window>.json` | chart data per window (1 h to all), averaged to the window's step |
| `data/track/` | map track chunks, at most 2,048 observations each, at 1 km, 100 m, 25 m, 5 m and native spacing |
| `data/casts/` | cast index and profiles (`ladcp.json` for the current arrows) |
| `data/sat/`, `data/ice-charts/` | satellite pictures and ice charts copied from `db/` |
| `data/history/` | the Wiki tab's layer |
| `static/` | scripts, styles, map data; `/static/tiles/` is served from `UNDERWAY_TILES_DIR` |
| `manual/` | this manual |

The page server serves `data/` with `no-store` headers, except track chunks,
which are immutable and cached; it gzips JSON (static files up to 8 MiB).
Unreferenced track chunks are removed after seven days, so an open page can
finish with an older set. Restart the page server after an update that
changes its compression or caching.

### Stores and caches

| Path | What | Safe to delete? |
|---|---|---|
| `db/<leg>.db` | one SQLite store per leg | yes: the next build reloads everything (slow for the archive) |
| `db/casts/<leg>/` | parsed casts | yes, one file to force a re-parse of that cast |
| `db/tsg/<leg>.pkl` | TSG minute means | yes |
| `cache/build/` | the build's shortcuts: leg frames, surprise scores before the live leg, long windows | yes: resets the build cache |
| `app/AMUNDSEN/cache/.run.lock` | the build lock | only if no build is running |

A cached cast is reused while its source files (size and mtime) and its
logbook row are unchanged. On the mirror every cast is revalidated on every
build. LADCP profiles always recheck size and nanosecond mtime, so corrected
exports are imported.

Not derived, and not to be deleted:

| Path | What |
|---|---|
| `db/alerts.json`, `db/alerts_state.json` | alert subscriptions and what has been sent |
| `db/waypoints.sqlite`, `db/transects.sqlite`, `db/plan/` | saved waypoints, transects, cruise plans |
| `db/feedback.sqlite` | feedback from the page |
| `db/history_flags.json` | Wiki review flags |
| `db/live_scrape/`, `db/provisional_tsg.csv`, `db/live_casts.json` | recorded live readings, available nowhere else |
| `db/photos/` | photo uploads' sessions and import state |
| `db/gcal_state.json` | which calendar events this installation created |
| `db/usage.sqlite`, `db/uptime.json` | visitor counts, uptime state |
| `chat/chat.sqlite` | the chat |

### When a build must be repeated

Some changes to how values are computed only take effect on data the build
processes afterwards. Rebuild everything by deleting `cache/build/` (and,
for a change to ingest, the leg's store) and waiting for the next run.

## When headers change between legs

Nothing is bound to a column name. `config.VARIABLES` in
`dashboard/config.py` gives each panel an ordered list of regular
expressions, tried against canonical column keys (`instrument — variable`,
NFKC-normalised, lower-cased); the first match wins. The result is in the
manifest and on the page's **Sources** tab, with a column per leg showing
where each source exists ([Sources and help](../user/sources-and-help.html)).

- A renamed column shows as "not found" and its panel says so. Add a
  pattern for the new name to `config.py`.
- A leg without a column (TSG oxygen before 2026 Leg 2) simply has gaps.

Position is taken per row from POSMV, then CNAV, then GC. An isolated fix
implausible from both neighbours (over 40 knots) is dropped; a jump the
track then continues from is drawn as a break.

Another ship or acquisition system would need its own `legs.discover()` and
header parsing in `ingest.parse_file()`; everything after that works on the
canonical keys.

## Pump-off flag

TSG intake flow below `LOW_FLOW_V` (0.5 V, `config.py`) marks a pump-off
episode. It indicates low flow, not proof that the pump is off. Each episode
is an event in the Schedule tab and is queued for the **Underway Updates**
Google calendar. Missing flow telemetry and leg boundaries split episodes;
no flow telemetry means no flag.

A window bin is flagged when any of its minutes had low flow, and so are its
two neighbours while the line flushes. On the TSG panels (`tsg=True` in
`config.py`) and on a track coloured by a TSG variable, flagged bins draw
grey, and the axis ranges and colour limits come from the unflagged bins.
Values are kept, not removed; table and CSV statistics are unchanged.

## Surprise score

The surprise score says how unusual each minute is against its own recent
past (`dashboard/surprise.py`):

- minute medians of the TSG and AVOS features (`SURPRISE_FEATURES`) are
  robustly scaled over the record (fluorescence log-transformed);
- each minute is compared with exponentially weighted means and covariances
  of the minutes before it, at five half-lives (`SURPRISE_SCALES`: 15 min,
  1 h, 3 h, 12 h, 48 h);
- the Mahalanobis distance at each half-life becomes a chi-square p-value,
  shown as −log10 p and capped; the combined score is the mean over
  half-lives.

| Setting in `config.py` | Default | What |
|---|---|---|
| `SURPRISE.min_feature_cover` | 0.05 | features with less finite data than this are dropped |
| `SURPRISE.winsor` | (0.005, 0.995) | quantiles values are clipped to |
| `SURPRISE.ridge` | 0.01 | variance floor added to each covariance |
| `SURPRISE.cap` | 6.0 | −log10 p ceiling per scale |
| `SURPRISE_ALERT_SCALE`, `SURPRISE_ALERT` | "3 h", 2.0 | a run of minutes above this level on this scale is an episode on the **Underway Updates** calendar |

## Other settings in `config.py`

`dashboard/config.py` holds what is not per installation: variables and
their patterns, windows (`WINDOWS`, `DEFAULT_WINDOW` = 6 h), the ship's time
zone for the header (`LOCAL_TZ`), the colour-scale quantiles
(`QUANTILE_LIMITS`, 5–95 %), the intranet links and the Google calendar ids.
A change there is a code change: commit it and let the deploy pull bring it
aboard.

Tables and CSV exports average compass directions as circular means (359°
and 1° give 0°); a balanced set of opposite directions has no mean and shows
a dash.
