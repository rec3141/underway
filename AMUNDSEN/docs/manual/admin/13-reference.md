# Reference

## Settings

Every setting is an environment variable. On a workstation, plain settings
go in `/etc/underway/site.env` and secrets in
`~/.config/underway/underway.env`; in the container both are written by the
`/settings` page into `data/config/`. "Page" marks the settings that page
offers.

### Installation and paths

| Setting | Default | What |
|---|---|---|
| `UNDERWAY_HOME` | `/data/underway_server` | the installation directory; the paths below default into it |
| `UNDERWAY_USER` | (required by `install.sh`) | the account the services run as |
| `UNDERWAY_PYTHON` | `python3` | the interpreter with the dashboard's packages |
| `UNDERWAY_CONFIG` | `~/.config/underway` | the secrets directory |
| `UNDERWAY_SITE` | `/etc/underway/site.env` | the site file the tools and `install.sh` read |
| `UNDERWAY_DB_DIR` | `db/` beside the package (the units set `$UNDERWAY_HOME/db`) | stores and service state |
| `UNDERWAY_CACHE_DIR` | `cache/` beside the package (units: `$UNDERWAY_HOME/cache`) | build cache |
| `UNDERWAY_WEBROOT` | `$UNDERWAY_HOME/www` | the built site |
| `UNDERWAY_CHAT_DB` | `$UNDERWAY_HOME/chat/chat.sqlite` | the chat database |
| `UNDERWAY_CAMERA_OUTPUT` | `$UNDERWAY_HOME/camera360` | camera timelapses |
| `UNDERWAY_ARCHIVE_DIR` | `$UNDERWAY_DB_DIR/../archive` | Amundsen Science's archive |
| `UNDERWAY_ICE_ROOT` | `$UNDERWAY_HOME/ice` | the ice camera's results |
| `UNDERWAY_TILES_DIR` | `/data/gis/tiles` | map tiles |
| `UNDERWAY_SEA_GRID` | `$UNDERWAY_TILES_DIR/sea-grid` | the sea-route grid |
| `UNDERWAY_TILES_REMOTE` | `grid:/data/gis/tiles` | where `tiles-pull.sh` pulls from |
| `ARCTIC_HISTORY_ROOT` | `/data/dev/arctic-history` | the Wiki's clone |
| `ARCTIC_HISTORY_REMOTE` | `grid:/data/dev/arctic-history` | where `history-pull.sh` pulls from |
| `UNDERWAY_PORT` | `8042` | the page server's port |
| `UNDERWAY_IFACE` | `wlo1` | the Wi-Fi interface (mDNS, the routes' fallback) |
| `UNDERWAY_MDNS_NAME` | `underway.local` | the mDNS name |
| `TZ` | `America/Toronto` (Page: UTC if blank) | the ship's time zone |
| `UNDERWAY_APP` | the checkout the script is in | `deploy-pull.sh`: the deploy checkout |

### Data

| Setting | Default | What |
|---|---|---|
| `UNDERWAY_DATA_ROOT` | `/mnt/ship/Data` | the Data share (`FULL_CSV/`, `Rosette/`, …) |
| `UNDERWAY_SHARE_ROOT` | `/mnt/ship/Share` | the Science share (`<year>/<leg>/`) |
| `UNDERWAY_LOCAL` | unset; `1` in `underway.service` | mirror first and build from the mirror |
| `UNDERWAY_MIRROR` | `/data/ship` | the local mirror |
| `UNDERWAY_MIRROR_DATA`, `UNDERWAY_MIRROR_SHARE` | `/mnt/ship/Data`, `/mnt/ship/Share` | what the mirror copies from |
| `UNDERWAY_MIRROR_FULL` | `0` | `1` forces a full mirror pass |
| `UNDERWAY_ASHORE` | unset | the day the cruise ended (`YYYY-MM-DD`): ashore mode |
| `UNDERWAY_DATA_URL` | `smb://10.0.0.10/Data` | the Data share as a link people can open |
| `UNDERWAY_INTRANET` | `http://10.0.0.2` | the ship's intranet (live page, links) |
| `UNDERWAY_SCHEDULE_URL` | `http://10.0.0.2/Schedule.html` | the operations schedule |
| `UNDERWAY_CTD_TCP` (Page) | `10.0.0.22:49160,…,49168` | SeaSave's address and ports for the live cast |

### Ship shares (container)

| Setting | Default | What |
|---|---|---|
| `SHIP_SMB_HOST` (Page) | `10.0.0.10` | the file server |
| `SHIP_SMB_DATA` (Page) | `Data` | the Data share's name |
| `SHIP_SMB_SHARE` (Page) | `Share` | the Science share's name |
| `SHIP_SMB_USER` (Page) | | share user name |
| `SHIP_SMB_PASSWORD` (Page, secret) | | share password |

On a workstation the shares are mounted from `/etc/fstab` with
`/etc/ship-smb.creds` instead ([Installing](installing.html#mounting-the-shares)).

### Alerts and keys (secrets in `underway.env`)

| Setting | Default | What |
|---|---|---|
| `TELEGRAM_KEY` (Page) | | the schedule bot's token (also read as `UNDERWAY_TELEGRAM_TOKEN`, `TELEGRAM_BOT_TOKEN`) |
| `TELEGRAM_ID` (Page) | | the keeper's chat id |
| `SMTP_HOST` (Page) | | mail server |
| `SMTP_PORT` (Page) | `465` | mail port |
| `SMTP_USER`, `SMTP_PASSWORD` (Page) | | the sending account |
| `SMTP_FROM` (Page) | `SMTP_USER` | From address |
| `SMTP_SSL` (Page) | `1` | `1` SSL, `0` STARTTLS |
| `SMTP_REPLY_TO` (Page) | | Reply-To |
| `UNDERWAY_OPS_EMAIL` (Page) | `SMTP_USER` | operations alerts |
| `UNDERWAY_ADMINS` | | chat names that may clear review flags (comma-separated); also `admins.json` |
| `UNDERWAY_UPTIME_URLS` | the backend, Caddy and mDNS health URLs | space-separated URLs for the uptime check |
| `COPERNICUS_ID`, `COPERNICUS_SECRET` (Page) | | Copernicus Data Space OAuth client |
| `UNDERWAY_GCAL` | `1` | `0` stops Google Calendar push |
| `UNDERWAY_GCAL_CREDS` | `$UNDERWAY_CONFIG/gcal-sa.json` | the Google service account key (Page: upload) |

### AI models

| Setting | Default | What |
|---|---|---|
| `OPENROUTER_API_KEY` (Page, secret) | | Dashboard AI key |
| `OPENROUTER_MODEL` (Page) | `google/gemini-3.8-flash` | Dashboard AI model |
| `OPENROUTER_ICE_KEY` (Page, secret) | | ice camera key |
| `OPENROUTER_ICE_MODEL` (Page) | `google/gemma-4-26b-a4b-it` | ice camera model |
| `OPENROUTER_REPORT_KEY` (Page, secret) | Dashboard AI key | cruise report key |
| `CRUISE_DIGITIZE_MODEL` (Page) | `google/gemini-3.8-flash` | cruise report model |
| `CRUISE_DIGITIZE_FALLBACK` | `openai/gpt-6-luna-pro` | used when the first refuses |
| `CRUISE_DIGITIZE_EFFORT` | unset | reasoning effort |
| `UNDERWAY_LLM` | `1` (container `0`) | use a local model when a part has no key |
| `UNDERWAY_LLM_URL` | `http://127.0.0.1:11434` | local model server |
| `UNDERWAY_LLM_API` | `ollama` | `ollama` or `openai` |
| `UNDERWAY_LLM_MODEL` | `gemma4-local` | local model name |

### Cameras

| Setting | Default | What |
|---|---|---|
| `UNDERWAY_CAMERA_SOURCE` (Page) | `/mnt/ship/Data/Camera_360` | camera pictures |
| `UNDERWAY_CAMERA_INTERVAL` (Page) | `120` | seconds between frames |
| `UNDERWAY_CAMERA_LAYOUT` (Page) | `mosaic` (Page and example: `portrait`) | `portrait` or `mosaic` |
| `UNDERWAY_CAMERA_WIDTH` (Page) | 720 portrait, 1280 mosaic | video width |
| `UNDERWAY_CAMERA_SYNC_LEG` (Page) | | the leg the daily copy takes |
| `UNDERWAY_CAMERA_SHARE` (Page) | | the daily copy's destination (must exist) |
| `UNDERWAY_CAMERA_PYTHON` | `UNDERWAY_PYTHON` | interpreter for the camera job |

### Cruise report

`CRUISE_DATA_ROOT`, `CRUISE_SHARE_ROOT`, `CRUISE_UNDERWAY_DB`,
`CRUISE_STATE_DIR`, `CRUISE_TEMPLATE`, `CRUISE_GEO_DIR`, `CRUISE_SHIP_TZ`:
see [The cruise report builder](cruise-report.html#settings).

### Publishing

`UNDERWAY_PUBLISH_*`: see [Publishing to the web](public-web.html#settings).

### Network and tools

| Setting | Default | What |
|---|---|---|
| `SHIP_EXTRA_HOSTS` | | ship hosts that get their own `/32` route (`ship-routes.sh`) |
| `AMUNDSEN_GAME` | `/data/dev/amundsen-game` | the game checkout for `make-release.sh` |
| `STAGE` | `/data/underway-release-stage` | `make-release.sh` staging |
| `UNDERWAY_VERSION`, `UNDERWAY_HTTP_PORT`, `UNDERWAY_DATA` | `latest`, `80`, `./data` | `compose.yaml` |
| `GDAL_PYTHON`, `RAMP`, `LAND`, `LAND_LAYER`, `LAND_BBOX`, `TID`, `ARCTIC_BBOX` | | tile builders ([Map data](map-data.html)) |

### Codex bot

`TELEGRAM_KEY_CODEX`, `UNDERWAY_CODEX_TELEGRAM_ID`, `UNDERWAY_CODEX_BIN`,
`UNDERWAY_CODEX_TIMEOUT` (in `underway.env`), `UNDERWAY_CODEX_CWD`,
`UNDERWAY_CODEX_DOWNLOADS`, `UNDERWAY_CODEX_HOME` (in `site.env`): see
[Integrations](integrations.html#the-codex-bot-optional).

## Files in the config directory

`~/.config/underway/` (container: `data/config/`):

| File | What |
|---|---|
| `underway.env` | secrets (mode 600) |
| `site.env` | plain settings (container only; a workstation uses `/etc/underway/site.env`) |
| `gcal-sa.json` | Google service account key (mode 600) |
| `admins.json` | chat names that may clear review flags |
| `chat-model.json` | local chat model override (`api`, `url`, `model`) |
| `chat-paused` | if present, the AI crew is off |
| `ice.json` | the ice camera worker's settings (workstation) |
| `admin-password`, `.session-key`, `.reload` | the settings page: password hash, cookie key, reload signal |

## Ports

| Port | Bound to | What |
|---|---|---|
| 80 | all | Caddy |
| 8042 | all (`--bind 0.0.0.0`) | page server |
| 8044 | 127.0.0.1 | cruise report builder |
| 8043 | 127.0.0.1 | live sampling adviser (experimental) |
| 8050 | 127.0.0.1 | the game (container) |
| 11434 | 127.0.0.1 | local Ollama model (workstation) |
| 18043 | 127.0.0.1 | ice camera model server (workstation, optional) |

Outbound aboard: 10.0.0.10 (SMB), 10.0.0.2 (intranet, HTTP), 10.0.0.22
(SeaSave TCP 49160–49168), grid (ssh), GitHub (deploy pull), and the
internet over HTTPS for OpenRouter, Telegram, SMTP, Google, Copernicus and
the Ice Service.

## Commands

From `$UNDERWAY_HOME/app/AMUNDSEN`, with `$UNDERWAY_PYTHON`:

| Command | What |
|---|---|
| `python -m dashboard legs` | list the legs found (`*` live, `a` archive) |
| `python -m dashboard build --root DIR [--title T] [--link 'LABEL|URL']` | build the site into DIR |
| `python -m dashboard build --tracks-only [--public] --root DIR` | rebuild only windows and track (grid) |
| `python -m dashboard serve --root DIR [--port 8042] [--bind 0.0.0.0]` | serve a built site |
| `python -m dashboard manual --root DIR` | render this manual into DIR/manual |
| `python -m dashboard archive-import` | write the archive as ACSD files |
| `python -m dashboard alerts` | send due alerts once |
| `python -m dashboard telegram-bot` | run the Telegram bot |
| `python -m dashboard gcal-push` | push Google Calendar items once |
| `python -m dashboard satellite [--force] [--backfill YYYY-MM-DD] [--within-km 500]` | satellite pictures |
| `python -m dashboard ice-charts --refresh | --list | --daily WIS36C | --url/--file …` | ice charts |
| `python -m dashboard supervise` | the container's init |
| `python -m dashboard set-admin-password [--random]` | set the settings page password |
| `python -m dashboard codex-bot` | the Codex bot |
| `python -m dashboard.usage` | page-view counts |
| `python -m dashboard.uptime` | one uptime check |
| `python -m dashboard.feedback` | read feedback |
| `python -m dashboard.cameras --source S --output O` | build timelapses |
| `python -m cruisereport serve --bind 127.0.0.1 --port 8044` | the report builder |

`pip install -e .` also installs `underway` (the same as
`python -m dashboard`) and `cruise-report` commands.

| Tool | What |
|---|---|
| `update_underway_py.sh` | the build job: mirror, then build |
| `deploy/install.sh [--check|--enable]` | install the units |
| `tools/mirror-share.sh [DEST] [full|quick]` | mirror the shares |
| `tools/deploy-pull.sh` | the deploy pull |
| `tools/ship-smb-setup.sh` | mount the shares (once) |
| `tools/ship-routes.sh on|off|status` | ship routes under the UM VPN |
| `tools/publish-mdns.sh NAME IFACE` | publish the mDNS name |
| `tools/camera-job.sh build|sync` | camera jobs |
| `tools/history-pull.sh [pull|status]` | the Wiki pull |
| `tools/tiles-pull.sh [all|SET|status]` | pull tile sets from grid |
| `tools/publish-web.sh push|deploy|rebuild|rebuild-deploy|history|static|tiles|status` | web publishing |
| `tools/publish-sources.sh [--dry-run]` | source files to grid |
| `tools/fetch-amundsen-archive.py fetch|split|status` | fetch the archive (grid) |
| `tools/rerender-gebco.sh`, `make_gebco_tiles.sh`, `make_survey_tiles.sh`, `make_sea_grid.sh`, `make_names_tiles.py`, `make_communities.py` | map data |
| `tools/build-ui-catalog.py [--check]` | rebuild the page's translation catalog |
| `deploy/container/make-release.sh VERSION OUTDIR [--with-data]` | make a container release |

## Languages

The page's own text is translated from catalogs in `locales/`
(`en.json` the source, `locales/variants/<locale>/<profile>.json` the
translations, `locales/selection.json` which one is used). After changing
them, rebuild the compiled catalog and commit it:

```sh
python3 tools/build-ui-catalog.py
python3 tools/build-ui-catalog.py --check
```

Missing or out-of-date translations fall back to English. The Wiki's
articles are translated separately, in the arctic-history project.
`docs/localization.md` describes the catalog format.

## Tests

Python tests (from `AMUNDSEN/`):

```sh
python -m unittest discover -s tests -p 'test_*.py'
python -m unittest discover -s tests -p test_ice_charts.py -v      # one file
```

Node tests (Node 22 or later):

```sh
node --test tests/*.test.cjs
```

Browser tests run the real page template and scripts against an isolated
synthetic data server, in headless Chromium with a fresh profile; they do
not touch the deployed dashboard or the shares. They need Chromium and a
Python with Jinja2:

```sh
PYTHON=python3 node tests/refresh-browser.cjs /usr/bin/google-chrome   # loading, failed updates, tab refresh, revised casts
PYTHON=python3 node tests/track-browser.cjs /usr/bin/google-chrome     # map track loading and detail
node tests/ice-charts-browser.cjs /usr/bin/google-chrome
```
