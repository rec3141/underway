# Integrations and keys

Each integration is off, and the page says so, until its keys are in place.
On a workstation the keys go in `~/.config/underway/underway.env` (mode 600)
and plain settings in `/etc/underway/site.env`; in the container both are
made on the `/settings` page ([The container and releases](container.html#the-settings-page)).
After editing `underway.env` on a workstation, restart the servers:

```sh
sudo systemctl restart underway-dashboard underway-telegram underway-report
```

| Integration | Keys | Unit | Aboard only |
|---|---|---|---|
| AI models | `OPENROUTER_*`, or a local model | (in the servers) | no |
| Telegram and email alerts | `TELEGRAM_KEY`, `TELEGRAM_ID`, `SMTP_*`, `UNDERWAY_OPS_EMAIL` | `underway-alerts.timer`, `underway-telegram.service` | yes |
| Google Calendar | `gcal-sa.json` | `underway-gcal.timer` | yes |
| Satellite pictures | `COPERNICUS_ID`, `COPERNICUS_SECRET` | `underway-satellite.timer` | yes |
| Cameras | `UNDERWAY_CAMERA_*` | `underway-camera*.timer` | yes |
| Ice charts | none | `underway-ice-charts.timer` | no (needs internet) |
| Wiki | an ssh key grid accepts | `underway-history.timer` | no |

## AI models

Several parts use a language model. They share three settings
(`dashboard/llm.py`), each an OpenRouter key and a model:

| Setting | Covers | Key | Model setting | Default model |
|---|---|---|---|---|
| Dashboard AI | the chat crew, the Wiki's answers, photo tags, underway-water alert advice, the Hearts crew in the game | `OPENROUTER_API_KEY` | `OPENROUTER_MODEL` | `google/gemini-3.8-flash` |
| Ice camera | the ice camera's classifications | `OPENROUTER_ICE_KEY` | `OPENROUTER_ICE_MODEL` | `google/gemma-4-26b-a4b-it` |
| Cruise report sheets | the report builder's logbook digitizing | `OPENROUTER_REPORT_KEY` | `CRUISE_DIGITIZE_MODEL` | `google/gemini-3.8-flash` |

- The keys come from openrouter.ai (pay as you go). The models must read
  images.
- The cruise report uses the Dashboard AI key when it has none of its own.
- The ice camera sends pictures all day and runs only on its own key.
- A model left at its default is not written to the settings, so a later
  default still applies.
- "Check the AI keys" on the settings page asks OpenRouter about each saved
  key, at no cost, and checks that each model exists and reads pictures.

### Without a key: the local model

A part with no OpenRouter key uses the local arrangement aboard: a model
already loaded on the workstation's shared GPU. The chat never loads a model
itself, since the GPU is shared; if none is loaded it tells the operator by
Telegram.

| Setting | Default | What |
|---|---|---|
| `UNDERWAY_LLM` | `1` | `0` turns the local model off (the container's setting) |
| `UNDERWAY_LLM_URL` | `http://127.0.0.1:11434` | the local server |
| `UNDERWAY_LLM_API` | `ollama` | `ollama` or `openai` (an OpenAI-style server) |
| `UNDERWAY_LLM_MODEL` | `gemma4-local` | the model name |
| `~/.config/underway/chat-model.json` | | overrides `api`, `url`, `model` without a restart |
| `~/.config/underway/chat-paused` | | if this file exists, the AI crew is off |

To keep the Ollama model resident: load `gemma4-local` with `keep_alive` -1;
unload it with `ollama stop gemma4-local`.

## Telegram and email alerts

People subscribe to schedule alerts from the Schedule tab (email) or by
messaging the Telegram bot, and set over/under alerts on underway values
from the bells on the Underway tab. `underway-alerts.timer` runs
`python -m dashboard alerts` every two minutes and sends what is due, once
per row and event. `underway-telegram.service` answers the bot's commands
within a second; while it is down, the alerts run answers them instead.

| Key (`underway.env`) | What |
|---|---|
| `TELEGRAM_KEY` | the bot's token, from @BotFather (`UNDERWAY_TELEGRAM_TOKEN` and `TELEGRAM_BOT_TOKEN` are also read) |
| `TELEGRAM_ID` | the keeper's own chat id: operations alerts and chat model notices go there |
| `SMTP_HOST`, `SMTP_PORT` | the mail server; Gmail is `smtp.gmail.com`, 465 |
| `SMTP_USER`, `SMTP_PASSWORD` | the sending account; for Gmail an app password |
| `SMTP_SSL` | `1` (SSL, port 465, the default) or `0` (STARTTLS, port 587) |
| `SMTP_FROM` | the From address; default `SMTP_USER` |
| `SMTP_REPLY_TO` | where replies go |
| `UNDERWAY_OPS_EMAIL` | where operations alerts go; default `SMTP_USER` |

Operations alerts go to the keeper (`UNDERWAY_OPS_EMAIL` and `TELEGRAM_ID`):

- the ACSD `FULL_CSV` record has not grown for 30 minutes (once per
  episode, with a note when it recovers);
- Wiki pages flagged for review, gathered into one message per run;
- outages and recoveries from the uptime check
  ([Running it day to day](running.html#health-url-and-uptime-alerts)).

Review flags raised by several people can be cleared only by an admin: a
chat name listed in `UNDERWAY_ADMINS` (comma-separated) or in
`~/.config/underway/admins.json` (a JSON list), on the device that owns that
name in the chat.

State files: `db/alerts.json` (subscriptions; writers take `db/alerts.lock`),
`db/alerts_state.json` (what has been sent), `db/telegram_bot.alive` (the
bot's heartbeat).

## Google Calendar

Two public calendars are shown in the Schedule tab and kept up to date from
the ship: **Amundsen Schedule** (event-log operations and intranet schedule
rows) and **Underway Updates** (surprise episodes and pump-off episodes).
Their ids are in `config.GCAL`.

The build touches no Google endpoint: it writes wanted items to
`db/gcal_queue.json` and reads the feeds from `db/gcal_cache.json`.
`underway-gcal.timer` runs `python -m dashboard gcal-push` every five
minutes, which refreshes the feeds and works through the queue, at most 200
requests a run.

| Setting | Default | What |
|---|---|---|
| `~/.config/underway/gcal-sa.json` | | the service account's key; the account owns both calendars |
| `UNDERWAY_GCAL_CREDS` | that path | another path for the key |
| `UNDERWAY_GCAL` | `1` | `0` stops pushing |

Every pushed item is remembered in `db/gcal_state.json` with its event id,
so nothing is inserted twice; with an empty state, the push adopts what is
already on the calendars.

## Satellite pictures

`underway-satellite.timer` runs `python -m dashboard satellite` every half
hour. It renders recent Sentinel imagery from the Copernicus Data Space
(Sentinel Hub processing API) when a picture is due:

| Picture | What |
|---|---|
| `s1` | newest Sentinel-1 radar (sees ice through cloud and dark), last two days |
| `s2` | least cloudy Sentinel-2 true colour, last week |
| `s1near`, `s2near` | a 240 × 160 km box round the ship at 50 m, renewed when the ship moves on |
| `s1north` | a fixed box over northern Eureka Sound and Nansen Sound |

The regional pictures cover 74–83.5 °N, 130–60 °W. Results are in
`db/sat/` (pictures and `sat.json` with corners, scene times and cost);
earlier pictures in `db/sat/archive/`. The build copies them to `data/sat/`.

Keys: `COPERNICUS_ID` and `COPERNICUS_SECRET`, an OAuth client of a
Copernicus Data Space account (User settings → OAuth clients). Each request
spends processing units from that account.

```sh
python -m dashboard satellite --force                   # render now
python -m dashboard satellite --backfill 2026-08-01     # a picture a day per sensor from that day
```

## Cameras

The ship's three 360° cameras write pictures to `Data/Camera_360/`. Two
products come from them: daily timelapse videos, and the ice camera's
classifications.

### Timelapses

`underway-camera.timer` runs `tools/camera-job.sh build` hourly;
`underway-camera-sync.timer` runs `tools/camera-job.sh sync` daily at
00:20 UTC. They share a lock (`camera360/.job.lock`); a run that finds it
busy skips. Needs `ffmpeg` with libx264 and the `cameras` extra.

| Setting (`site.env`) | Amundsen | What |
|---|---|---|
| `UNDERWAY_CAMERA_SOURCE` | `/mnt/ship/Data/Camera_360` | one leg's directory (`YYYYMMDD` folders), or the directory holding the legs; with the latter every leg builds into `camera360/<leg>/` |
| `UNDERWAY_CAMERA_INTERVAL` | `120` | seconds between frames |
| `UNDERWAY_CAMERA_LAYOUT` | `portrait` | `portrait` (9:16, the three cameras stacked, for phones) or `mosaic` (side by side, the default when unset) |
| `UNDERWAY_CAMERA_WIDTH` | `720` | pixels; default 720 portrait, 1280 mosaic |
| `UNDERWAY_CAMERA_SYNC_LEG` | `2026_LEG_03` | the leg the daily copy takes |
| `UNDERWAY_CAMERA_SHARE` | `/mnt/ship/Share/2026/2026_LEG_03/Pictures/Timelapse` | where the daily copy goes; must already exist |
| `UNDERWAY_CAMERA_OUTPUT` | `$UNDERWAY_HOME/camera360` | the products, served at `/camera/` |

Change `UNDERWAY_CAMERA_SYNC_LEG` and `UNDERWAY_CAMERA_SHARE` at the start of
each leg.

What the build job makes:

- one video per UTC day, `days/YYYYMMDD/latest.mp4` with a `.json` listing
  each frame's source and capture time, and `full-leg.mp4`, the days
  stitched in order without re-encoding;
- 12 frames per second, one picture per interval, H.264; each frame shows
  its capture time in UTC, and gaps are compressed, not shown as elapsed
  time;
- the current day ends three minutes behind now, and stays partial until the
  UTC day ends;
- unchanged days are not re-encoded; resized frames are cached and pruned
  after eight days;
- a day with no frames, or an encoder failure, leaves the previous video in
  place; a corrupt picture is skipped.

The sync copies the daily and full-leg MP4 and JSON files into the
destination, with no `--delete`; it never creates a missing destination
(a missing directory usually means an unmounted share). A missed copy can be
retried:

```sh
sudo systemctl start underway-camera-sync.service
```

### The ice camera

The ice camera classifies the ice in new camera pictures. Its results are in
`$UNDERWAY_HOME/ice/` (`ice.sqlite` and derived images; `UNDERWAY_ICE_ROOT`
overrides) and appear in the Underway tab's ice camera panel and in the
cruise report's conditions.

| Where | How it runs |
|---|---|
| Container | the supervisor runs `python -m dashboard.ice_worker` on OpenRouter while **Ice camera: OpenRouter key** is set |
| Workstation | two optional user units, `underway-ice-model.service` (a local llama.cpp server with the Gemma model, on the GPU) and `underway-ice.service` (the worker) |

Workstation setup (not installed by `install.sh`):

1. Copy `deploy/ice.json.example` to `~/.config/underway/ice.json`; set the
   picture source, the product root, and the paths of `llama-server`, the
   Gemma weights and projector. Nothing downloads models.
2. Copy `deploy/ice-model.user.service.example` and
   `deploy/ice-worker.user.service.example` to
   `~/.config/systemd/user/underway-ice-model.service` and
   `underway-ice.service`; adjust the Python and installation paths.
3. `systemctl --user daemon-reload && systemctl --user enable --now underway-ice.service`.

The first scan records a start time and classifies only pictures taken after
it; after downtime it catches up from there. Do not delete the database to
restart it. Restart the worker by hand after code updates: the deploy pull
does not touch user units. Thermal limits stop the model service when the
CPU or GPU runs hot and retry later. `deploy/ice-runtime.html` has the
details.

The classifications are exploratory, not calibrated ice concentration.

## Ice charts

The map shows Canadian Ice Service charts: weekly SIGRID-3 vector charts for
Eastern Arctic, Western Arctic and Hudson Bay, and the daily Eureka raster
(`WIS36C`) for the ship's area. `underway-ice-charts.timer` runs
`python -m dashboard ice-charts --refresh` at 04:23, 10:23, 16:23 and 22:23;
it fetches what is newer than the cache and leaves the rest. The build
publishes from the cache; the timer writes no web root.

Needs the `ice-charts` extra and, for the daily raster, `gdal_translate`,
`gdalwarp` and `gdalinfo`. Without them the timer logs that it cannot
convert and the map keeps the bundled chart. A region not posted, or a
failed download, is logged and skipped; the unit fails only when a run got
nothing at all.

Cache: `$UNDERWAY_DB_DIR/ice-charts/<region>-<date>.geojson` per vector
chart, `.png` plus `.raster.json` per daily raster. A failed import never
replaces a good chart. Fresh installations include seed charts so the map
has something before anything is fetched.

By hand:

```sh
python -m dashboard ice-charts --list                      # current download URLs
python -m dashboard ice-charts --daily WIS36C              # the daily raster
python -m dashboard ice-charts --url <URL from --list> --date 2026-09-07 --region 'Eastern Arctic'
python -m dashboard ice-charts --file chart.zip --date 2025-08-18 --region 'Eastern Arctic' \
    --source-url https://iceweb1.cis.ec.gc.ca/Archive/page1.xhtml
```

For past dates, download **Shapefile / SIGRID-3** from the CIS archive and
import one chart per run; `--date` is the chart's valid date. ZIP, TAR or a
local SHP with SHX, DBF and PRJ are accepted. Charts are Canadian Ice
Service / ECCC data under the Open Government Licence – Canada.
`deploy/ice-charts.md` has the format details.

## The Wiki's data

The Wiki tab (history and nature) comes from the arctic-history project,
kept on grid by its research crew. The ship only reads it.
`underway-history.timer` runs `tools/history-pull.sh` at minutes 3, 23 and
43. It brings:

1. the code, by `git pull --ff-only` (if that fails, nothing else is
   pulled, so the data stay matched to the code);
2. the English research database, as a consistent snapshot;
3. the reviewed translation release behind the Wiki's language variants;
4. the fetched files, by rsync.

The two databases swap in together, inside the build's lock. No release on
grid leaves the ship's installed release alone; an empty release withdraws
every language.

| Setting | Amundsen | What |
|---|---|---|
| `ARCTIC_HISTORY_ROOT` | `/data/dev/arctic-history` | the clone; without it the Wiki tab is absent and the unit is skipped |
| `ARCTIC_HISTORY_REMOTE` | `grid:/data/dev/arctic-history` | where it comes from (ssh host:path) |

The service account needs an ssh key grid accepts; enable the timer once
that works:

```sh
sudo systemctl enable --now underway-history.timer
tools/history-pull.sh status          # row counts on both sides, languages installed
journalctl -u underway-history -n 30
```

A first pull over a slow link copies the whole database and every file; the
unit allows 45 minutes.

## Photos and uploads

The Photos tab imports pictures from folders on the Science share and takes
uploads from phones on the ship's network.

- The service account needs write access to the destination folder on the
  share.
- Upload sessions are in `db/photos/uploads.sqlite` and expire after 24
  hours (that never deletes photos). Files are staged on local disk,
  checked, then copied to the share.
- Accepted: JPEG, PNG, WebP; up to 300 photos, 64 MiB and 80 megapixels per
  photo, 2 GiB per batch.
- Pictures that fail to decode are recorded in
  `db/photos/failed-images.json` and skipped until they change.
- Each upload batch folder carries a hidden `.phone-upload` marker; leave it
  in place.

Uploads are meant for the trusted ship network, not the public internet.

## Feedback

The page's **Feedback** button stores messages in `db/feedback.sqlite`. Read
them on the server:

```sh
UNDERWAY_DB_DIR=/data/underway_server/db /opt/miniforge3/bin/python3 -m dashboard.feedback
```

## The live CTD cast

The page server listens for SeaSave's converted-data XML stream on the CTD
acquisition PC and shows the cast in progress in the Casts tab. By default
it probes `10.0.0.xxx`, ports 49160–49168, and keeps the first stream it
recognises. Set `UNDERWAY_CTD_TCP` (for example
`10.0.0.xxx:49161`) only if the live cast does not appear.

## The Codex bot (optional)

`underway-codex.service` runs a Telegram bot that passes messages to the
Codex coding agent, working in a development checkout
(`UNDERWAY_CODEX_CWD`, default `/data/dev/underway`), never the deployed one.

| Setting | Where | What |
|---|---|---|
| `TELEGRAM_KEY_CODEX` | `underway.env` | its own bot token, separate from the schedule bot |
| `UNDERWAY_CODEX_TELEGRAM_ID` | `underway.env` | the only user allowed; default `TELEGRAM_ID` |
| `UNDERWAY_CODEX_BIN`, `UNDERWAY_CODEX_TIMEOUT` | `underway.env` | the CLI's path; seconds per job (1800) |
| `UNDERWAY_CODEX_CWD`, `UNDERWAY_CODEX_DOWNLOADS` | `site.env` | its working directory; a writable Downloads folder |

Install and log in to the Codex CLI as the service account, add the token,
then `sudo deploy/install.sh` (which also installs its AppArmor profile) and
`sudo systemctl enable --now underway-codex.service`. Jobs run one at a time;
state is in `db/codex_bot.sqlite`. `deploy/telegram-codex.md` has the
details.

## The live sampling adviser (experimental)

A separate page, `livesample.html`, suggests CTD bottle depths and
flow-through samples. It runs as its own optional user service on port 8043
with the `sampling` extra, reading the main dashboard's live API, and needs
a narrow Caddy route. It is not part of the standard install;
`deploy/livesample.md` describes it.
