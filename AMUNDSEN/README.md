# AMUNDSEN — underway dashboard

A web dashboard of the CCGS *Amundsen*'s data for everyone aboard and ashore:
the ship's track on a map coloured by any underway variable, live panels and
charts of the underway record (thermosalinograph, weather, navigation, winches),
CTD, MVP and LADCP casts, the operations schedule and event log, photographs, a
wiki of the region's history and nature, a ship chat, and a builder for each
science team's cruise report. It runs on a workstation aboard (or as one Docker
image), works without internet, and publishes a public copy at
https://cryomics.org/underway/.

## Documentation

The manual is served by every installation at **`/manual/`**
(`http://underway.local/manual/` aboard), with a print version of the whole
thing at `/manual/print.html`:

- the **User manual**: every tab, step by step, with screenshots;
- the **Administrator manual**: how it works, installing, running, settings,
  the container and releases, publishing, troubleshooting, and a reference of
  every setting.

Its sources are `docs/manual/` (Markdown, one file per chapter; how to write
them is in `docs/manual/STYLE.md`). Every build renders them into the web root
when they change; `python -m dashboard manual --root <web root>` does it on
demand.

## Repository

```
dashboard/            the Python package: ingest, build, page server, services
  config.py           variables, windows, settings read from the environment
  ingest.py           ACSD day files -> one SQLite store per leg (incremental)
  derive.py           position, distance, variable resolution
  surprise.py         the anomaly score
  legs.py             finds legs on the shares (and in the archive)
  build.py            combined record -> data/*.json, track chunks, index.html
  serve.py            the page server and its api/ routes
  manual.py           docs/manual -> manual/
  templates/          index.html.j2
  static/             the page: app.js, tabs.js, map.js (MapLibre), plotly.min.js,
                      style.css, geo/, the i18n catalog
cruisereport/         the cruise report builder, served at /report/
deploy/               systemd unit templates, install.sh, site.env.example, Caddyfile,
                      container/ (the Docker kit and make-release.sh)
tools/                operational scripts: mirror, deploy pull, publishing, tile
                      builders, the archive fetch
locales/              UI strings: en.json and the translators' variants
docs/                 the manual's sources and topic notes
tests/                Python (pytest) and Node tests
update_underway_py.sh the build wrapper the build timer runs
pyproject.toml        package metadata: `pip install -e .` gives `underway`
deprecated/           the previous R implementation, kept for reference
```

## Developing

Python 3.11 or later:

```sh
cd AMUNDSEN
pip install -e '.[chat,gcal,cameras,ice-charts,report]'
export UNDERWAY_DATA_ROOT=/path/to/Data UNDERWAY_SHARE_ROOT=/path/to/Share
export UNDERWAY_DB_DIR=$PWD/runtime/db UNDERWAY_CHAT_DB=$PWD/runtime/chat.sqlite UNDERWAY_LLM=0
python -m dashboard legs                            # what the data roots hold
python -m dashboard build --root $PWD/runtime/www   # build the site
python -m dashboard serve --root $PWD/runtime/www --port 8042
```

A local copy of the shares (`/data/ship` on the workstation) is the easiest
data root. Everything the services read is an environment variable with a
default; the Administrator manual's *Reference* chapter lists them.

How the pieces fit:

- **Ingest** parses each new or changed `ACSD_YYYYMMDD.csv` into its leg's
  SQLite store, keyed by canonical column names (`instrument — variable`), so
  header changes between legs need no code.
- **Build** reads the stores, resolves each dashboard variable from the
  patterns in `config.VARIABLES`, derives position, distance, pump state and the
  surprise score, and writes chart windows (`data/w-*.json`), content-addressed
  track chunks (`data/track/`), casts, the calendar, the manifest and
  `index.html`. Every file is written atomically; caches in `cache/build/` make
  a build with nothing new take seconds.
- **The page** is static: it polls `data/manifest.json` and fetches what the
  chosen legs and span need. Charts are Plotly; the map is MapLibre on a globe,
  drawn from Plotly-shaped traces (`map.js`). Colours are tokens in
  `static/style.css`, read by the charts and the map, so a theme is one block of
  values.
- **The server** serves the web root and the `api/` routes (chat, feedback,
  waypoints, photo uploads, the sea route, the live feeds).

UI text lives in `locales/en.json`; after editing it, run
`python tools/build-ui-catalog.py` to regenerate `static/i18n-catalog.js`
(`--check` in tests).

A change merged to `master` is live on the ship within five minutes (the
deploy pull); branch as `AMUNDSEN--<topic>`.

## Tests

```sh
python -m pytest tests -q                 # Python
for f in tests/*.test.cjs; do node --test "$f"; done          # Node 22 or later
PYTHON=python3 node tests/refresh-browser.cjs /usr/bin/google-chrome   # browser checks (Chromium)
PYTHON=python3 node tests/track-browser.cjs /usr/bin/google-chrome
```

The browser checks run the real page template and scripts against an isolated
synthetic data server; they never touch a deployed dashboard or the shares.

## Credits and licences

Data: Amundsen Science / ArcticNet (the ship's ACSD record and the 2002–2024
archive from https://erddap.amundsenscience.com, CC BY 4.0). Map: Natural Earth,
GEBCO 2024, OpenStreetMap contributors (ODbL), GeoNames (CC BY 4.0), the
Canadian Geographical Names Database (Open Government Licence – Canada).
Libraries bundled under `static/`: MapLibre GL JS (BSD-3-Clause, see
`maplibre-gl.LICENSE.txt`) and Plotly.js (MIT).
