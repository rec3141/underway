# AMUNDSEN — underway dashboard

Dashboard for the CCGS *Amundsen*'s underway data (Amundsen Science's ACSD
system: one `ACSD_YYYYMMDD.csv` per day, semicolon separated, two header
lines — variable name, then instrument — 10-second cadence, ~68 columns).

The page shows a map of the ship's track coloured by any variable, a panel per
variable, and a "surprise" panel that flags minutes that look unusual against
the previous 48 hours. A span slider reaches from the last hour back across
every leg on the share, and a leg list filters what is shown.

A Theme picker in the header offers four looks: Claude dark (the default),
Claude light, and Minimal dark and light, which set bigger type on flat
surfaces for reading on a phone or across a lab. The choice is kept in the
browser. Every colour is a token at the top of `static/style.css`, and the
graphs and the map read those tokens, so a theme is one block of values.

The “live” leg is the discovered leg with the latest date in its
`ACSD_YYYYMMDD.csv` filenames, breaking ties by year and leg number. File
copy/modification times do not affect that selection. This marks the latest
available leg, not a guarantee of fresh data or a working live CTD feed.
TSG intake flow below the 0.5 V cutoff (`LOW_FLOW_V`) marks a pump-off
episode. Each episode is an event in the Schedule tab's Event Log and Calendar and is
queued on the **Underway Updates** Google calendar; it is a low-flow
indicator, not proof that pump power is off. Missing flow telemetry and leg
boundaries split episodes rather than implying a continuous stop. An ongoing
episode extends with each build.

A window bin is flagged (`pump_low`) if any of its minutes had low flow, and
so are its two neighbours while the line flushes. On the TSG-sourced panels
(`tsg=True` in `config.py`) and on the map track coloured by a TSG variable
the flagged bins draw grey with a hover note, and the y-ranges and colour
limits are set by the unflagged bins only. Values are kept, not removed or
corrected; table/CSV statistics are unchanged. No flow telemetry means no
flag. A data rebuild is required after installing this change.

```
dashboard/           the Python package
  config.py          variables, windows, data roots — the file to edit
  ingest.py          ACSD files -> one SQLite store per leg (incremental)
  derive.py          position, distance, variable resolution, surprise
  surprise.py        the anomaly score
  legs.py            finds legs on the shares
  build.py           combined record -> data/*.json + index.html
  serve.py           static server (no-store headers for data files)
  cli.py             `python -m dashboard {legs,build,serve}`
  templates/         index.html.j2
  static/            app.js, map.js (the map), style.css, plotly.min.js (the charts),
                     maplibre-gl.js/.css (the map's library), geo/*.geojson
update_underway_py.sh    systemd-facing wrapper: build into the web root
pyproject.toml           package metadata; `pip install -e .` gives an `underway` command
deprecated/              the previous R implementation and its wrappers, kept for reference
```

## Requirements

- Python ≥ 3.11 with `pandas`, `numpy`, `jinja2`, `scipy`, `xlrd` and `openpyxl` — `pip install -e .` from this
  directory installs them and an `underway` console command. `plotly` is not
  needed at run time; its `plotly.min.js` is committed under `static/`
  (refresh it from a plotly install with the `assets` extra). The map is drawn
  by MapLibre GL JS, likewise committed (`static/maplibre-gl.js` and `.css`, the
  UMD build of the 5.x line, BSD-3-Clause, `maplibre-gl.LICENSE.txt`). An installation
  names its interpreter as `UNDERWAY_PYTHON` in its site file (see *Taking over*).
- Optional integrations: `pip install -e '.[chat]'` installs the HTTP client
  for the local AI crew; `pip install -e '.[gcal]'` installs the Google Calendar
  HTTP/signing dependencies. Human chat uses only the standard library. Use
  `UNDERWAY_LLM=0` to disable the AI crew on machines without a model server.
- Paths: the data roots and the store directory default to the ship's layout
  and can be overridden with `UNDERWAY_DATA_ROOT`, `UNDERWAY_SHARE_ROOT` and
  `UNDERWAY_DB_DIR`. Another ship or system would replace `legs.discover()`
  and the header parsing in `ingest.parse_file()`; everything downstream works
  on canonical column keys and is system-agnostic.
- The shares mounted: `//10.0.0.10/Data` at `/mnt/ship/Data` and
  `//10.0.0.10/Share` at `/mnt/ship/Share` (see *Mounting* below).
- No internet at run time. The basemap is Natural Earth 10 m GeoJSON clipped to
  the western Arctic, committed under `static/geo/`; regenerate with `ogr2ogr`
  if the ship goes somewhere else.

## Running

```sh
cd AMUNDSEN
python3 -m dashboard legs                     # what is on the shares
python3 -m dashboard build --root /path/to/webroot
python3 -m dashboard serve --root /path/to/webroot --port 8042
```

For an ordinary installation, `pip install .` installs the `underway` command;
`underway --help` works outside the source checkout. Choose writable paths for
runtime state on another machine, for example (POSIX shell):

```sh
export UNDERWAY_DB_DIR="$PWD/runtime/db"
export UNDERWAY_CHAT_DB="$PWD/runtime/chat.sqlite"
export UNDERWAY_LLM=0
underway serve --root "$PWD/www" --port 8042
```

Set `UNDERWAY_DATA_ROOT` and `UNDERWAY_SHARE_ROOT` to the mounted data before
building. These environment variables work on Windows too; the deployment
shell scripts and systemd units are for Linux with systemd (see *Taking over*).
Set `UNDERWAY_TILES_DIR` if using optional raster tiles on another machine.
The default database directory is beside the source package, and the default
chat database is `$UNDERWAY_HOME/chat/chat.sqlite` (`UNDERWAY_HOME` defaults to
`/data/underway_server`), so explicitly set both state paths when installing
into a shared or read-only Python environment.

### Reaching the dashboard on the ship

Three entry points serve the same site (Caddy on port 80 proxies to the
page server on 8042; see `deploy/Caddyfile`):

| from                          | address                                  |
|-------------------------------|------------------------------------------|
| ship Wi-Fi (192.168.3.x)      | `http://underway.local` or `http://192.168.3.216/underway/` |
| wired ship LAN (10.0.0.x)     | `http://10.0.0.58/`                        |
| this workstation              | `http://127.0.0.1:8042/`                   |

`underway.local` is an mDNS name published by `underway-mdns.service` for the
Wi-Fi address only: avahi cannot scope a record to one interface, and a client
handed the wired address from the Wi-Fi side cannot reach it. Android does not
resolve mDNS; give those users the IP. If a client cannot load the page: check
which network it is on (`192.168.3.x` vs `10.0.0.x`) and use that network's
address; try the IP before the name; make sure the URL starts with `http://`
(nothing listens on 443); and from the workstation confirm the page answers on
`127.0.0.1:8042` and that `ip route get 10.0.0.10` says `dev enp6s0`, not
`tun0`. The UM VPN re-installs `10.0.0.0/25` routes (and the odd `/32`) on
every reconnect; `ship-routes.timer` re-asserts on-link `/26` routes each
minute, and any colliding host can be listed in `SHIP_EXTRA_HOSTS` for its own
`/32`. A DHCP reservation or a ship DNS name would be the stable long-term fix.

### Map layers

Besides the track, stations and tow tracks, the map offers an **Event log**
layer (geolocated entries of the ship's event log, filtered by the shown legs
and span, grouped by position so several events at one spot share a marker and
one hover) and a **Places** layer: settlements of Nunavut, the NWT,
Labrador, the northern shores of Québec/Ontario/Manitoba and all of Greenland,
from GeoNames (CC BY 4.0), with a curated list of Inuit, Greenlandic and older
colonial names. Labels thin out with zoom (population 2000+ far out, all when
close in). Refresh the layer with `tools/make_communities.py` after downloading
new `CA.zip`/`GL.zip` dumps from geonames.org into `/data/gis/geonames/`.
A **Names** layer (bays, sounds, straits, islands, capes, lakes, rivers,
glaciers, mountains) appears as a pill when its tiles exist; see *Geographic
names* below.

### Cast cache

Parsed casts are cached under `db/casts/<leg>/`. A cached cast is reused only
while its source files (size and mtime) and the logbook row behind its
metadata are unchanged; on the local mirror every cast is re-validated on
every build, straight off the CIFS share only casts younger than three days
are (a stat there costs a round trip). LADCP profiles always revalidate size
and nanosecond mtime, including older casts, so corrected exports are imported.
Delete a cache file to force a re-parse.

`build` syncs every leg's store (only new or changed day files are parsed),
combines the legs, computes derived variables, writes `data/w-*.json` for each
window plus `data/manifest.json`, publishes track chunks under `data/track/`,
copies `static/`, and renders `index.html`.
Files are written under a temporary name and renamed, so a page polling the
directory never reads a partial file. A full build of eight legs (2.8 M rows)
takes about 15 s and 2 GB of memory; a build with nothing new takes a few
seconds (these timings predate the initial track-chunk publication).

The map loads observations independently of the charts. `manifest.track`
indexes immutable, content-addressed chunks of at most 2,048 observations at
1 km, 100 m, 25 m, 5 m, and native resolution. Auto detail follows the map's ground
scale automatically, never exceeding 1 km spacing. Selection preserves actual
observations, bends, leg boundaries, and gaps. Native observations farther
apart than the selected spacing remain gaps in sampling; no points are invented.
Charts continue to use time-averaged windows. New builds no longer publish
`w-*-fine.json`, and the browser never requests those legacy files.

Panning and zooming request only chunks overlapping the visible bounds and
selected time/legs. The loader cancels superseded requests, fetches at most
four chunks concurrently, and caps each request/render at 50,000 rows. Its
cache is bounded by 100,000 rows and an estimated 64 MiB. A density message
asks the viewer to zoom in or shorten the span when that budget is reached;
the track may be incomplete until then. Colours use the selected span's
limits, which stay stable when panning. Camera ice observations remain a
separate time-filtered API and are matched to the loaded track by time/leg.

The Python server negotiates gzip for JSON (static files up to 8 MiB), and
track chunks use immutable caching. Unreferenced chunks are reclaimed after
seven days so open pages can finish using an older generation. Other data remains uncached. Track detail
also works on a static server; configure gzip there separately. Rebuild the
site to publish the track index and new scripts together, and restart the
Python page server to enable its compression changes.

Stores live in `AMUNDSEN/db/<leg>.db` by default (`UNDERWAY_DB_DIR` overrides).
They are derived data: delete them and the next build reloads everything.

## Local mirror (recommended on the ship)

Every access to the CIFS shares costs a network round trip — a `stat()`, an
8 KB read, a tile write — and the build touches thousands of files. Running
against a local mirror removes all of that:

```sh
tools/mirror-share.sh /data/ship          # rsync only what the dashboard reads
UNDERWAY_LOCAL=1 update_underway_py.sh    # mirror, then build into $UNDERWAY_HOME/www
```

With `UNDERWAY_LOCAL=1` (set as `Environment=` in `underway.service`) the
wrapper runs the mirror first and builds from it with `UNDERWAY_DATA_ROOT` and
`UNDERWAY_SHARE_ROOT` pointing into `/data/ship`; the web root moves to local
disk (`UNDERWAY_WEBROOT`, default `$UNDERWAY_HOME/www`) and the server serves
that. The share is then read by one rsync pass per build and by nothing else,
and `RequiresMountsFor` is no longer needed. The initial mirror is ~15 GB
(mostly the per-cast plot HTML); afterwards a pass copies only new files.

Raster tiles are never written to the share: `make_gebco_tiles.sh` writes to
`UNDERWAY_TILES_DIR` (default `/data/gis/tiles`) and the server maps
`/static/tiles/` onto it.

## Operation on the ship

An installation is one directory, `UNDERWAY_HOME` (`/data/underway_server` on
the Amundsen's workstation):

```
app/         a clone of this repository on master: what the services run
db/ cache/   the ingest stores and build cache (UNDERWAY_DB_DIR, UNDERWAY_CACHE_DIR)
www/         the built site the page server serves (UNDERWAY_WEBROOT)
chat/        the chat database (UNDERWAY_CHAT_DB)
camera360/   the daily camera timelapses (UNDERWAY_CAMERA_OUTPUT)
```

Everything particular to the machine — that directory, the account the
services run as, the Python interpreter, the mirror, the arctic-history clone,
the port and the Wi-Fi interface — is in one file, `/etc/underway/site.env`
(from `deploy/site.env.example`). The unit files in `deploy/` are templates
filled in from it by `deploy/install.sh`; the units also read it into their
environment, and the shell tools source it.

`underway-deploy.timer` runs `tools/deploy-pull.sh` every five minutes, which
fetches, fast-forwards `app/` to `origin/master`, and restarts the serving
processes (`underway-dashboard`, `underway-telegram`) when a Python file under
`dashboard/` changed; anything else is picked up by the next build. A merge to
master is live on the ship within minutes, and no development checkout is ever
what the ship serves. Unit files are the one thing the pull cannot install:
when `deploy/` changes, run `sudo $UNDERWAY_HOME/app/AMUNDSEN/deploy/install.sh`
(the pull's journal line says so; `deploy/install.sh --check` shows the
difference first).

The units:

| unit | what it does |
|---|---|
| `underway.timer` → `underway.service` | mirror the shares and build the site, every minute (`update_underway_py.sh`; a lock keeps runs from overlapping) |
| `underway-dashboard.service` | the page server, `python -m dashboard serve` on `UNDERWAY_PORT` (8042) |
| `underway-deploy.timer` | the pull above |
| `underway-alerts.timer`, `underway-telegram.service` | schedule alerts by Telegram and email; the Telegram bot |
| `underway-gcal.timer` | queued Google Calendar items |
| `underway-satellite.timer` | Sentinel imagery around the ship |
| `underway-history.timer` | the Wiki tab's layer from grid, every 20 minutes (`tools/history-pull.sh`) |
| `underway-camera.timer`, `underway-camera-sync.timer` | the camera timelapses and their daily copy to the leg's photo folder |
| `underway-mdns.service` | publishes `underway.local` on the Wi-Fi interface |
| `ship-routes.timer` | keeps the ship's 10.0.0.x network off the UM VPN (only needed with that VPN) |

`underway-history.timer` runs at minutes 3, 23 and 43, clear of the round
minutes when the build and the deploy pull run. It brings four things: the
arctic-history code by git, the English research database, the reviewed
translation release behind the wiki's language variants, and the fetched
files. The two databases swap inside the build's own lock, together, because
the publisher rechecks each translation against the English source it was made
from. `tools/history-pull.sh status` shows the row counts on both sides and
which locales the installed release offers; `journalctl -u underway-history`
is the log. The timer needs the service account's ssh key on grid, so it is
enabled once that works:

```sh
sudo systemctl enable --now underway-history.timer
```

Without a clone at `ARCTIC_HISTORY_ROOT` the unit's condition skips it and the
Wiki tab is simply absent. No release on grid leaves whatever release the ship
already has; an empty release is how the crew withdraws every locale.

### Taking over

To run the dashboard on another account or another machine:

1. Mount the ship's shares: `sudo tools/ship-smb-setup.sh` (asks for the
   share password once; see *Mounting*).
2. Clone the repository as the service account into `UNDERWAY_HOME/app`, and
   install the packages into the interpreter the services will use:
   `pip install -e 'app/AMUNDSEN[chat,gcal,ice-charts]'`. The ice-charts extra
   is what `underway-ice-charts.timer` needs to read a chart, along with the
   GDAL commands for the daily raster (`gdal_translate`, `gdalwarp`,
   `gdalinfo`); without them the timer logs that it cannot convert and the map
   keeps the bundled chart.
3. `sudo mkdir /etc/underway && sudo cp app/AMUNDSEN/deploy/site.env.example
   /etc/underway/site.env`, and edit it: the directory, the account, the
   interpreter, the Wi-Fi interface (`ip -br addr`), and the camera settings
   for the current leg.
4. Put the secrets in the account's `~/.config/underway/` (or
   `UNDERWAY_CONFIG`), neither of them in git:
   - `underway.env`, from `deploy/underway.env.example`, mode 600: Telegram,
     the SMTP account for alerts, the operations address, Copernicus;
   - `gcal-sa.json`, the Google service account's key.
   `admins.json` there (a JSON list of chat names that may clear review flags)
   is optional. Each integration is off, and says so on the page, while its
   values are missing.
5. `sudo app/AMUNDSEN/deploy/install.sh --enable` installs the units and starts
   the build, the page server and the deploy pull; enable the others as their
   credentials go in (`sudo systemctl enable --now underway-alerts.timer
   underway-telegram.service ...`).
6. Caddy on port 80 (`deploy/Caddyfile`) and the mDNS name are what people on
   the ship type; copy the Caddyfile to `/etc/caddy/Caddyfile` and reload Caddy.

If neither data root is a directory, or they hold no `YYYY_LEG_NN` folders
with ACSD files, `build` exits with status 2 without touching the web root:
the SMB mounts drop from time to time, and an empty mount looks exactly like
"no data", so the run fails loudly rather than publishing an empty page over a
good one. `journalctl -u underway.service` shows the reason.

Useful commands:

```sh
systemctl list-timers underway.timer underway-deploy.timer
journalctl -u underway.service -n 50
journalctl -u underway-dashboard.service -n 20
journalctl -u underway-deploy.service -n 20      # what the last pulls brought in
```

Firewall: nothing on the workstation blocks 8042 (`ufw` is installed but
inactive). The ship's Fortinet blocks *outbound* 22 and plain-HTTP requests
with a package manager's User-Agent; HTTPS is fine.

### Mounting

`sudo tools/ship-smb-setup.sh` writes the two CIFS entries to `/etc/fstab` and
starts their automount units; `sudo tools/ship-routes.sh on` steers `10.0.0.0/24` to the
local gateway when the UM VPN is up (the VPN pushes `10.0.0.0/25`, which
swallows the NAS). Do **not** add `x-systemd.idle-timeout` to the mounts: an
idle unmount stops every unit with `RequiresMountsFor` on that path, and a
build that reads its inputs in the first seconds and plots for minutes gets
killed part-way.

## When headers change between legs

Nothing is bound to a column name. `config.VARIABLES` gives each panel an
ordered list of regular expressions tried against canonical column keys
(`instrument — variable`, NFKC-normalised, lower-cased). The first match wins.
The resolution is written to the manifest and shown in the page's *Data
sources & provenance* table, with a per-leg column showing where each source
exists. A renamed column shows as "not found" and its panel says so; add a
candidate pattern to `config.py` to pick the new name up. Legs without a
column (e.g. TSG oxygen before 2026 Leg 2) simply have gaps.

Position is coalesced per row from POSMV, then CNAV, then GC. Isolated fixes
implausible from both neighbours (> 40 kn) are dropped; a *step* — a jump the
track then continues from — is a discontinuity in the log and is drawn as a
break rather than a chord.

## Surprise score

Minute medians of the TSG and AVOS features are robust-scaled against the last
48 h; PCA gives Hotelling's T² and the residual Q, and a Schäfer–Strimmer
shrinkage covariance gives a Mahalanobis distance. Each becomes an upper-tail
empirical p-value against the learning set; the score is Σ −log10 p, so 3 is
roughly "one in a thousand across the three tests". Larger is more surprising.
Configure in `config.SURPRISE`.

## Tabs

The map stays on the left; the tabs swap the right-hand pane.

- **Underway** — the rolling-window panels described above.
- **Casts** — CTD profiles and sections. A Rosette cast's profile is the
  SeaBird `.cnv` in `Data/external_proprietary/CTD/` when one exists
  (downcast, binned to 1 dbar; position and seafloor depth from the file);
  the Plotly HTML plots under `Data/Rosette/<leg>/plots/` add the variables
  the `.cnv` lacks (CDOM, PAR, buoyancy frequency, nitrates), and are the sole
  source for legs without `.cnv` files. Station, label and time come from the
  leg's CTD logbook; the plot filename says which rosette (Classic or TM), and
  the list filters Rosette / TM / MVP. An MVP tow (`Data/MVP/<leg>/<tow>/*.m1`)
  is one entry holding every dip, drawn as a track on the map: its row selects
  the whole tow (for a section), and expanding it lets individual dips be
  picked (for profiles). Select from the list, or click station markers and
  tow starts on the map; *Profiles* overlays the selection per variable (a
  tow's dips shade light to dark along the tow), *Section* grids one variable
  against the header's Time or Distance axis in time order.
  Lowered ADCP profiles (`Data/Rosette/<leg>/Ladcp/stn###.lad`) appear as
  separate **LADCP** casts linked to their parent CTD identifier. They retain
  their native depth bins in metres; these are not pressure values in dbar.
  Eastward and northward current, derived speed and source error velocity
  are in m/s. Components already reference true north/east, so no magnetic
  rotation is applied. Direction means where the water flows toward.
  These processed exports have no verified scientific QC flag; error velocity
  is preserved without imposing a threshold. The source header, file size and
  timestamp travel with each profile. Invalid identity, coordinates or depth
  grids reject that file without blocking the other casts; nonfinite velocity
  values become JSON null. No depth extrapolation is performed by the importer.
  In Casts, choose **LADCP**, select profiles, then use **Single**, **Multi**
  or **Section**. Sections show samples at stations without interpolating
  between stations. On the map, enable **LADCP currents** and enter a depth
  in metres. Arrows use the nearest measured bin within half the typical bin
  spacing, follow the selected legs/time span, and omit unsampled depths.
  Arrowheads point toward flow; length and colour encode speed. Hover shows
  actual bin depth and error velocity; click opens the current profile.
  `data/casts/ladcp.json` contains the small collection of full current profiles
  for depth-selectable map arrows, and the main cast index links it through
  `ladcp_file`. The full mirror pass includes only `.lad` and diagnostic `.png`
  files from each leg's `Ladcp` directory. Continuous shipboard ADCP time series
  are a separate source and are not imported here.
  Both SeaBird CTDs are handled: the SBE 9 rosette (`prDM`, `t090C`, with
  CDOM, PAR and SUNA columns) and the SBE 19plus (`prdM`, `tv290C`). Files on
  the share are read in one call each — CIFS charges a network round trip per
  read, so line-by-line access turns a 21 MB `.cnv` into minutes. Parsing is cached under `db/casts/` (versioned, trusted by name
  except for casts under three days old, because every `stat()` on the share
  costs seconds) and batched per build so the underway page keeps its cadence.
- **Calendar** — the ship's event log (`Data/EventLog/<leg>/Eventlog_<leg>.xls`)
  as an agenda or timeline, with the operations schedule scraped from the
  intranet (`UNDERWAY_SCHEDULE_URL`, default `http://10.0.0.2/Schedule.html`;
  the last copy is cached in `db/schedule.json` for when the intranet is down).
- **Table** — hourly or daily mean/min/max/count of every panel variable with
  position and leg, sortable by any column, downloadable as CSV.

## Higher-resolution bathymetry (optional)

`tools/make_gebco_tiles.sh` turns the GEBCO 2024 GeoTIFF release (4.4 GB from
BODC/CEDA) into a shaded-bathymetry Web Mercator tile pyramid. The pyramid on
the ship is the globe at zooms 0–8 (610 m/px, about what GEBCO's 15" grid
supports) with the western Arctic and Labrador Sea at zoom 9 on top, built as
two runs into one directory (`tools/rerender-gebco.sh` does both into a
staging directory and swaps it in; the ship's `/data/gis/gebco/rerender-world.sh`
is its forerunner):

```sh
tools/make_gebco_tiles.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/gebco" -180 -90 180 90 0-8      # the globe, ~6 min on 32 cores
tools/make_gebco_tiles.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/gebco" -150 45 -15 86 9-9       # the Arctic box at z9
```

With `LAND` set to the OSM land polygons (see the coastline section below) and
`LAND_BBOX` to their box, the shore inside that box comes from the polygons
rather than GEBCO's zero contour, so a strait the polygons keep open stays
open in the picture; `rerender-gebco.sh` passes both through.

When `gebco/` exists under `UNDERWAY_TILES_DIR` the map draws it beneath the
vector layers instead of the Natural Earth depth bands. The build reads the
directory and puts each run of zooms into the map as its own source — the
boxed run with bounds — so the map never asks for a tile that is not there
(`raster_pyramid` in `build.py`). The pyramid is served with a week-long cache
and is **not** committed (a few GB); regenerate it on a new machine. Needs
GDAL with Python bindings (`gdal-bin python3-gdal` on Ubuntu).

## Exporting the map

The map's ⇩ button opens the export panel, which writes a PNG or an SVG of the
view, and a **KMZ** of everything the map has loaded, off screen included:
the track, the stations, the waypoints, the cruise plan, the event log, the
camera positions, the wiki's history and nature layers, the ice chart's
polygons and the ship, each a folder of placemarks in Google Earth. It carries
what the page holds in memory, so it is as complete as the layers switched on
and the span chosen when it is taken, and it names positions and labels rather
than a picture of them. `static/map-kmz.js` builds it: the KML from the map's
traces, then a zip written in the browser (`tests/kmz.test.cjs` reads one back
the way Google Earth does). Nothing is fetched to make it.

## Waypoints and sea distances

A double click on open map, or a press held on it, drops a waypoint; a click
on a station marks that. The mark's box gives the position, the distance from
the ship by air (a great circle) and by sea, and the depth or ground height
there. It stays until the mark is clicked again or another point is, and its
text can be selected and copied.

A waypoint's name can be typed over. Saving it keeps it for everyone
(`api/waypoints`, `db/waypoints.sqlite`): it joins the Stations tab beside the
logged stations and draws on the map with them, and the row there removes it.

The sea route and the depth come from `dashboard/searoute.py`
(`GET /api/searoute?to=lat,lon` for the ground alone, with `&from=lat,lon` as
well for the distances and the route drawn on the map). Both read a grid built
once from the GEBCO release the tiles are rendered from:

```sh
tools/make_sea_grid.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$UNDERWAY_TILES_DIR/sea-grid"          # the western Arctic and Labrador Sea, ~4 min
```

With `LAND` set to the shore polygons (reprojected to the grid's plane: GDAL
does not reproject while rasterising) the land comes from the coastline the
map draws rather than from GEBCO's own zero contour. With `TID` set to
GEBCO's Type Identifier grid, each cell also records where its depth came
from, which the route uses and the box reports.

That writes `elevation.npy`, `water.npy`, `source.npy` and `grid.json` on a polar
stereographic plane (EPSG:3413) at 250 m: about 31,000 by 26,000 cells, 1.7 GB,
memory-mapped so a route touches only the window it walks. A cell is water when
the lowest GEBCO sample in it is below sea level, so a channel narrower than a
cell — Bellot Strait is about one — stays open. The server reads
`UNDERWAY_SEA_GRID` (default `sea-grid` under `UNDERWAY_TILES_DIR`) and picks a
new build up without a restart; without it the box shows the air distance only.

The route is the fast-marching solution of the eikonal equation, which needs
`scikit-fmm` (`pip install -e '.[routing]'`; it builds from source). Arrival
time spreads from the ship through the water and the route is the way back down
that field, so it is not confined to a lattice of headings: open water comes out
as a straight line rather than a staircase of 22.5 degree legs, and a distance
in open water lands within a few tens of metres of the great circle. Two things
set the speed. The plane is conformal, so its scale varies with latitude and the
speed carries that factor, which makes arrival time a true ground distance.
Water that has actually been surveyed is counted as faster, by a third, with
the credit tapering out over 2 km. Only about a quarter of this water has ever
been sounded and what has is very largely ship tracks, which join up, so this
makes a route follow them where they go its way: on the runs measured, routes
spend about three times as much of their length on surveyed water for one to
three percent more distance.

Water shallower than 100 m is then slowed, and so is water within 5 km of land,
which together hold a route off the coast: on a clear run it stands about 5 km
off a headland for a few tens of metres of extra distance. Both are survey
margins rather than keel margins. This ship clears far less than 100 m, but
only a small share of this coast is surveyed to modern standards, and GEBCO
interpolates where no one has sounded, so the shallows and the shore are where
the chart is least trustworthy. A route is an estimate for planning, not a
track to steer, and the depth is GEBCO's at that cell, not a sounding.

The line drawn is the walk back down the arrival time, following its gradient,
so it curves where the route curves. It is simplified only to within a fraction
of a cell and never across land.

## A finer coastline (optional)

The coastline, land and islands in `static/geo/` are Natural Earth 10 m,
generalized to about a kilometre, which wanders on and off the relief's own
shore at zoom 9. A vector tile set cut from the OpenStreetMap land polygons
(osmdata.openstreetmap.de, ODbL) replaces them when it exists as
`coast/` under `UNDERWAY_TILES_DIR`. It is built on grid, where the 1.3 GB
source and GDAL live, and copied to the ship:

```sh
ogr2ogr -t_srs EPSG:3857 -clipdst <box in metres> -nlt MULTILINESTRING coast.gpkg coastlines-split-4326/lines.shp -nln coast
ogr2ogr -update -t_srs EPSG:3857 -clipdst <box in metres> -nlt MULTIPOLYGON coast.gpkg land-polygons-split-4326/land_polygons.shp -nln land
ogr2ogr -f MVT coast coast.gpkg -dsco MINZOOM=0 -dsco MAXZOOM=10 -dsco COMPRESS=NO
```

Grid keeps the result as `/data/gis/tiles/coast/`; the ship pulls it (grid
cannot reach the ship) with `tools/tiles-pull.sh`, which copies every tile
set grid has, each swapped in whole (`tools/tiles-pull.sh status` lists what
is on either side).

The shoreline must come from OSM's coastline *lines*, not from the boundary
of the land polygons: the polygons are shipped split into a grid, and their
boundaries include every cut, which draws as a lattice over the land.

The build reads the writer's `metadata.json` for the zoom range, bounds and
layer names (`vector_tiles` in `build.py`); the map then draws the `coast`
layer as the shoreline and, without the relief raster, the `land` layer as
land, and fetches neither Natural Earth file. Glaciers and depth bands stay
Natural Earth.

## Geographic names (optional)

`tools/make_names_tiles.py` cuts the geographic names of the map's box as
vector tiles: every official name in the Canadian Geographical Names Database
(Open Government Licence – Canada) and GeoNames' Greenland dump (CC BY 4.0),
less the settlements (the Places layer has them) and the roads, parks and
reserves. Each name carries the zoom it first shows at — from CGNDB's
"relevance at scale" field, with a floor by generic term so a cove never
shows far out, and by hand for the Greenland names the world knows — and the
tiles hold one layer per band, so a far-out tile carries a few dozen names
and a close-in one every name there is. The map draws a symbol layer per
band, water names in italic (`Open Sans Italic`, served beside the Regular
glyphs) and land names upright, and MapLibre's collision engine thins the
rest; the page's own labels sit above them. CGNDB names in an Indigenous
language show over the English on a second line. The sources are downloaded
on grid, where GDAL lives, and the tiles copied to the ship like the
coastline:

```sh
mkdir -p /data/gis/names/src && cd /data/gis/names/src
curl -O https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_csv_eng/cgn_canada_csv_eng.zip
curl -O https://download.geonames.org/export/dump/GL.zip
tools/make_names_tiles.py /data/gis/names/src /data/gis/tiles/names     # ~30 s, 34,000 tiles, 140 MB
```

Then, on the ship, `tools/tiles-pull.sh names` (or plain `tools/tiles-pull.sh`
for every set grid has).

The build reads the writer's `metadata.json` (`vector_tiles` in `build.py`,
as for the coast) and the Names pill appears. Known gaps: no names for the
Alaskan sliver of the box (GeoNames' US dump is large; add it to the script
if the ship works the Beaufort), and the Greenland labels are GeoNames'
primary form, often the Danish one (Scoresby Sund) rather than the
Greenlandic (Kangertittivaq).

The glyphs under `static/geo/glyphs/` are built with `fontnik`
(`npm install fontnik`, then `fontnik.range({font, start, end})` for the
four Latin ranges 0–1023) from the Open Sans TTFs in the googlefonts/opensans
repository.

## Publishing to the public web

`tools/publish-web.sh` puts the dashboard at https://cryomics.org/underway/,
in two hops, because the ship's firewall lets it reach grid and nothing else:

1. **On the ship** `publish-web.sh push` first runs `publish-sources.sh`.
   This incrementally mirrors the full current/archived ACSD CSV records,
   TSG inputs, event logs, Rosette logbooks/bottle files and MVP profiles to
   `grid:/data/underway_server/source`. CTD CNVs and rendered cast HTML are
   excluded; TSG CNVs are included because they supply the underway readings.
   Only the dedicated `source/` mirror is pruned, including excluded files.
   Recorded live snapshots and the provisional tail go into `source/runtime/`;
   accounts, subscriptions and credentials are never transferred.
2. The ship pushes its built site to grid's `www/`, excluding `data/w-*.json`, `data/track/`
   and the history layer. Grid retains its generated data. Use
   `publish-web.sh push --dry-run` to preview source and site transfers without
   rebuilding or deploying; the source destination directories may be created.
3. **On grid**, each push first fast-forwards the checkout from `origin/master`
   under the deployment lock. `publish-web.sh rebuild-deploy` stages a `build --tracks-only`
   from `source/Data`, `source/Share` and the recorded live snapshots. Per-leg
   stores and analysis caches stay in grid's `db/` and `cache/` for reuse.
   The build verifies source leg order against the incoming manifest, then
   generates chart windows and viewport track chunks locally. Immutable chunks
   and their fingerprint indexes are reused from the previous build; unreferenced
   chunks expire after seven days. A failed rebuild blocks deployment.
   Other ship-generated products (casts, schedule and aggregate tables) stay
   intact. Grid renders its history layer, updates the public manifest and
   front page, then publishes to DreamHost. The campus-to-web transfer includes
   all history files unless `UNDERWAY_PUBLISH_MAX_MB` specifies a cap.

Grid needs its own Python environment with the dashboard dependencies. For example:

```sh
python3 -m venv /data/underway_server/.venv
/data/underway_server/.venv/bin/pip install -e '/data/dev/underway/AMUNDSEN[cameras]'
```

`UNDERWAY_PUBLISH_TRACK_PYTHON` overrides that interpreter;
`UNDERWAY_PUBLISH_SOURCE_REMOTE` overrides the ship's destination and
`UNDERWAY_PUBLISH_SOURCE_DIR` the corresponding grid directory. Keep the
source destination a dedicated absolute directory ending in `/source`.
`UNDERWAY_PUBLISH_STATE_DIR` defaults to the parent of the mirror's `www/`.

Enable five-minute pushes on the ship with `sudo deploy/install.sh` and
`sudo systemctl enable --now underway-publish.timer`. The first run can ingest
all source records; subsequent runs reuse stores and rsync only changes.
Each transfer logs `Total bytes sent` and `Total bytes received` in
`journalctl -u underway-publish.service`; these are the compressed rsync transfer
counts (before SSH/network overhead), unlike `Total transferred file size`,
which counts whole changed files even when only their deltas cross the link.

What stays aboard: the shipboard cameras (`/camera/`), the nature journal's
photographs and the `/Share` gallery (`/journal/`), and the GEBCO raster
pyramid. The page's services do not run on the web server: every `api/`
request there answers 503 with a JSON body, and the published manifest is
marked `public` and lists no cameras. The page reads that mark
(`UW.public`) and hides what only runs aboard rather than letting it fail:
the Photos and Chat tabs and the chat side bar, the Feedback button, the
Live cast, the schedule's alert bells and form, the wiki's review flags and
Ask button, the ice-camera panel, the intranet feed, the photos map layer
and the KMZ drop. The Sources tab says so in place of the intranet links.

The web copy's map draws grid's own tiles, not the ship's: the GEBCO relief,
the coastline and the geographic names under `UNDERWAY_TILES_DIR` on grid
(`gebco/`, `coast/`, `names/`; without a `gebco/` there the map falls back
to Natural Earth's depth bands). `publish-web.sh tiles` copies them to the
web server by hand the first time (a few hundred thousand small files), and
after that each deploy re-syncs a set whose directory has changed.
`tools/rerender-gebco.sh` builds the pyramid on grid as on the ship.
`publish-web.sh status` says what is where and when; the settings are the
`UNDERWAY_PUBLISH_*` lines of `site.env`.

## Front end notes

- Plotly's toolbar is off. Drag pans, the wheel zooms, double-click resets, ⟲
  resets; `log` toggles a log axis on spiky variables.
- The map (`static/map.js`) is MapLibre, drawn from the trace-shaped layers the
  page's modules describe (`UW.extraMapTraces` and the layer builders in
  `app.js`): every trace becomes features of four layers (lines, circles,
  sprite icons, labels), so a redraw is a `setData` on each and the ship's
  position one on a one-point source. The basemap style is reloaded only when
  its id changes (theme, satellite picture, geography, the names layer).
- The map is drawn as a globe (`projection: globe` in the style), not in Web
  Mercator: at the ship's latitudes Mercator stretches the map four to eight
  times. The tiles are still Web Mercator tiles, drawn on the sphere, so
  nothing exists above 85.05° N and the pole is blank; a true polar
  projection would mean another library (issue #90). Consequences in the
  code: a view can be a box to fit (`{bounds}`) rather than a centre and zoom,
  and the scale bar and the track-detail spacing read the ground distance off
  the map with `unproject` instead of a Mercator formula.
- The chosen *Colour by* variable colours the map track and every panel's
  points on one shared scale (5–95 % of what is shown).
- Panels can be dragged to reorder, expanded (⤢) or minimised (—) to the
  bottom bar; layout persists in the browser's localStorage.
- Axes are UTC; the header shows ship time (`config.LOCAL_TZ`).
- `SST (°C)` is the TSG hull temperature, as in the R version; it reads warm
  when the ship is stationary with the TSG not flowing.

## Troubleshooting

The header reports failed data updates while keeping the last successfully
loaded observations. Requests time out after 30 seconds and retry on the next
30-second poll, on network reconnection, or when the page becomes visible
again. Casts, Stations, Schedule, and Table refresh while open; failed tab downloads are
retried even if the build timestamp has not changed.

- *Page loads but map is blank*: check `static/geo/*.geojson` served (200) and
  that the browser has WebGL. The map is MapLibre (`static/map.js`,
  `static/maplibre-gl.js`); `window.UW.mapView.map` is the MapLibre map in the
  browser console.
- *"nothing to show"*: all legs unticked, or the span holds no data.
- *Timer runs but nothing changes*: `journalctl -u underway.service`; a lock
  held by a stuck run is `AMUNDSEN/cache/.run.lock`.
- *Mount points empty after boot*: `systemctl start mnt-ship-Data.automount
  mnt-ship-Share.automount`.

## Refresh regression checks

The request/cache checks need Node.js 22 or later:

```sh
node --test tests/data.test.cjs
```

The browser check uses an isolated synthetic data server and the real page
template/scripts; it does not access the deployed dashboard or shared drives.
It needs Chromium and Python with Jinja2. Set `PYTHON` to the desired interpreter:

```sh
PYTHON=python3 node tests/refresh-browser.cjs /path/to/chromium
PYTHON=python3 node tests/track-browser.cjs /path/to/chromium
```

This test starts Chromium with its sandbox disabled for compatibility with
restricted development environments, using a fresh temporary profile and only
the local test page. It checks initial-load recovery, failed updates, active
tab refresh, revised cast data, and out-of-order window responses.
The track check verifies viewport loading, zoom-dependent detail, stale-request
rejection, and event-log axis bounds without discarding out-of-span events.

### Wind-direction statistics

Hourly/daily tables and CSV exports use the same unweighted circular mean
as the plotted windows for compass directions: 359° and 1° average to 0°,
not 180°. Missing samples are excluded. A balanced set of opposing directions
has no mean direction (resultant magnitude below 1e-12); it appears as a dash
in the table and an empty CSV field, with the sample count retained.
Direction minimum/maximum remain the observed numeric extremes, not a circular
spread or the bounds of the shortest arc. Other variables retain arithmetic
means. Rebuild the dashboard data after updating to regenerate these aggregates.
