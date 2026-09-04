# AMUNDSEN — underway dashboard

Dashboard for the CCGS *Amundsen*'s underway data (Amundsen Science's ACSD
system: one `ACSD_YYYYMMDD.csv` per day, semicolon separated, two header
lines — variable name, then instrument — 10-second cadence, ~68 columns).

The page shows a map of the ship's track coloured by any variable, a panel per
variable, and a "surprise" panel that flags minutes that look unusual against
the previous 48 hours. A span slider reaches from the last hour back across
every leg on the share, and a leg list filters what is shown.

```
underway/            the Python package
  config.py          variables, windows, data roots — the file to edit
  ingest.py          ACSD files -> one SQLite store per leg (incremental)
  derive.py          position, distance, variable resolution, surprise
  surprise.py        the anomaly score
  legs.py            finds legs on the shares
  build.py           combined record -> data/*.json + index.html
  serve.py           static server (no-store headers for data files)
  cli.py             `python -m underway {legs,build,serve}`
  templates/         index.html.j2
  static/            app.js, style.css, plotly.min.js, geo/*.geojson
update_underway_py.sh    systemd-facing wrapper: build into the web root
pyproject.toml           package metadata; `pip install -e .` gives an `underway` command
deprecated/              the previous R implementation and its wrappers, kept for reference
scheduler/               separate tool: event log -> Google Calendar sync (R)
```

## Requirements

- Python ≥ 3.11 with `pandas`, `numpy`, `jinja2` — `pip install -e .` from this
  directory installs them and an `underway` console command. `plotly` is not
  needed at run time; its `plotly.min.js` is committed under `static/`
  (refresh it from a plotly install with the `assets` extra). On the ship
  workstation the interpreter with the stack is `/opt/miniforge3/bin/python3`.
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
python3 -m underway legs                     # what is on the shares
python3 -m underway build --root /path/to/webroot
python3 -m underway serve --root /path/to/webroot --port 8042
```

`build` syncs every leg's store (only new or changed day files are parsed),
combines the legs, computes derived variables, writes `data/w-*.json` for each
window plus `data/manifest.json`, copies `static/`, and renders `index.html`.
Files are written under a temporary name and renamed, so a page polling the
directory never reads a partial file. A full build of eight legs (2.8 M rows)
takes about 15 s and 2 GB of memory; a build with nothing new takes a few
seconds.

Stores live in `AMUNDSEN/db/<leg>.db` by default (`UNDERWAY_DB_DIR` overrides).
They are derived data: delete them and the next build reloads everything.

## Operation on the ship

Two systemd units (the files are in `/etc/systemd/system/`):

- `underway.timer` → `underway.service` runs `update_underway_py.sh` every
  10 minutes (`OnCalendar=*:0/10`). The wrapper holds a lock so runs never
  overlap, and writes straight into the web root.
- `underway-dashboard.service` runs `python3 -m underway serve` on port 8042.

Useful commands:

```sh
systemctl list-timers underway.timer
journalctl -u underway.service -n 50
journalctl -u underway-dashboard.service -n 20
```

Firewall: nothing on the workstation blocks 8042 (`ufw` is installed but
inactive). The ship's Fortinet blocks *outbound* 22 and plain-HTTP requests
with a package manager's User-Agent; HTTPS is fine.

### Mounting

`~/bin/ship-smb-setup` writes the two CIFS entries to `/etc/fstab` and starts
their automount units; `~/bin/ship-routes on` steers `10.0.0.0/24` to the
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

## Higher-resolution bathymetry (optional)

`tools/make_gebco_tiles.sh` turns the GEBCO 2024 GeoTIFF release (4.4 GB from
BODC/CEDA) into a shaded-bathymetry Web Mercator tile pyramid:

```sh
tools/make_gebco_tiles.sh gebco_2024_sub_ice_topo_geotiff.zip \
    "$WEBROOT/static/tiles/gebco" -150 45 -15 86 2-9
```

When `static/tiles/gebco/` exists under the web root the map draws it beneath
the vector layers instead of the Natural Earth depth bands. The pyramid is
served with a week-long cache and is **not** committed (hundreds of MB);
regenerate it on a new machine. Needs GDAL with Python bindings
(`gdal-bin python3-gdal` on Ubuntu).

## Front end notes

- Plotly's toolbar is off. Drag pans, the wheel zooms, double-click resets, ⟲
  resets; `log` toggles a log axis on spiky variables.
- The chosen *Colour by* variable colours the map track and every panel's
  points on one shared scale (5–95 % of what is shown).
- Panels can be dragged to reorder, expanded (⤢) or minimised (—) to the
  bottom bar; layout persists in the browser's localStorage.
- Axes are UTC; the header shows ship time (`config.LOCAL_TZ`).
- `SST (°C)` is the TSG hull temperature, as in the R version; it reads warm
  when the ship is stationary with the TSG not flowing.

## Troubleshooting

- *Page loads but map is blank*: check `static/geo/*.geojson` served (200) and
  that the browser has WebGL. The map is Plotly `scattermap` (MapLibre).
- *"nothing to show"*: all legs unticked, or the span holds no data.
- *Timer runs but nothing changes*: `journalctl -u underway.service`; a lock
  held by a stuck run is `AMUNDSEN/cache/.run.lock`.
- *Mount points empty after boot*: `systemctl start mnt-ship-Data.automount
  mnt-ship-Share.automount`.
