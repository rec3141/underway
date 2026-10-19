# Running it day to day

Everything runs as systemd units under the service account. This chapter
lists them, says what each does and when, and gives the commands to check
them. In the container the same jobs run under one supervisor
([The container and releases](container.html)).

## The units

| Unit | Runs | What it does |
|---|---|---|
| `underway.timer` → `underway.service` | every minute, on the minute | mirror the shares, then build the site (`update_underway_py.sh`) |
| `underway-dashboard.service` | always | the page server, `python -m dashboard serve` on `UNDERWAY_PORT` (8042) |
| `underway-report.service` | always | the cruise report builder on 127.0.0.1:8044 |
| `underway-deploy.timer` | every 5 minutes | pull `master` into `app/` and restart what changed (`tools/deploy-pull.sh`) |
| `underway-uptime.timer` | every minute | check the site's health URLs; email on outage and recovery |
| `underway-ice-charts.timer` | 04:23, 10:23, 16:23, 22:23 | fetch newer Canadian Ice Service charts into the cache |
| `underway-alerts.timer` | every 2 minutes (at :20 s) | schedule and underway-water alerts by Telegram and email |
| `underway-telegram.service` | always | the Telegram bot, answering commands |
| `underway-gcal.timer` | every 5 minutes from :02 | push queued items to Google Calendar |
| `underway-satellite.timer` | :07 and :37 | Sentinel imagery around the ship |
| `underway-camera.timer` | hourly | rebuild the camera timelapses |
| `underway-camera-sync.timer` | daily, 00:20 UTC | copy the timelapses to the leg's photo folder |
| `underway-history.timer` | :03, :23, :43 | pull the Wiki tab's data from grid (`tools/history-pull.sh`) |
| `underway-publish.timer` | every 5 minutes | push the site to grid for the public web copy (`tools/publish-web.sh push`) |
| `underway-mdns.service` | always | publish `underway.local` on the Wi-Fi interface |
| `underway-codex.service` | always | optional: a Telegram bot for the Codex coding agent (`deploy/telegram-codex.md`) |

`deploy/install.sh --enable` enables the core set: `underway.timer`,
`underway-deploy.timer`, `underway-dashboard.service`,
`underway-report.service`, `underway-uptime.timer` and
`underway-ice-charts.timer`. Enable the others one by one once their
credentials or prerequisites are in place:

| Unit | Needs |
|---|---|
| `underway-alerts.timer`, `underway-telegram.service` | Telegram and/or SMTP keys in `underway.env` |
| `underway-gcal.timer` | `gcal-sa.json` |
| `underway-satellite.timer` | Copernicus keys |
| `underway-camera*.timer` | the camera lines in `site.env`, `ffmpeg` |
| `underway-history.timer` | a clone at `ARCTIC_HISTORY_ROOT` and an ssh key grid accepts |
| `underway-publish.timer` | an ssh key grid accepts; grid set up ([Publishing to the web](public-web.html)) |
| `underway-mdns.service` | avahi |

The minute-by-minute jobs are spread across the minute so they do not
start together: the build on the minute, alerts at :20 s
every other minute; the Wiki pull keeps clear of the round minutes.

Unit hardening: the jobs run with `ProtectSystem=full`,
`ProtectHome=read-only` and write access only to `UNDERWAY_HOME` (plus the
mirror and the history clone's `db/` for the build). A path a job has to
write outside those needs a `ReadWritePaths=` line in its template.

## The build

`underway.service` runs `update_underway_py.sh`, which:

1. takes the lock `app/AMUNDSEN/cache/.run.lock` (a run still going makes the
   next one exit at once with "Another run is in progress");
2. unless ashore, mirrors the shares with `tools/mirror-share.sh`: a quick
   pass every minute (ACSD day files, event logs, TSG files) and a full pass
   every tenth minute (also the CTD, MVP, LADCP and archived-season trees).
   `UNDERWAY_MIRROR_FULL=1` forces a full pass. A failed mirror pass is not
   fatal: the build proceeds on what is mirrored;
3. runs `python -m dashboard build --root $UNDERWAY_WEBROOT` at low CPU and
   I/O priority.

A build with nothing new takes a few seconds. If neither data root holds
`YYYY_LEG_NN` folders with ACSD files, the build exits with status 2 and
leaves the web root alone: an empty mount looks exactly like "no data", so
the run fails loudly rather than publishing an empty page.

More on what the build reads and writes is in
[Data, builds and storage](data.html).

## The deploy pull

What the ship runs is `UNDERWAY_HOME/app`, a clone on `master`. No
development checkout is ever served. `underway-deploy.timer` runs
`tools/deploy-pull.sh` every five minutes, as root, with git run as the
checkout's owner:

1. fetch `origin master`; with no route to GitHub, do nothing;
2. fast-forward `app/` to `origin/master`;
3. restart what the change needs:

| Changed | Effect |
|---|---|
| `dashboard/*.py` (other than `casts.py`, `ladcp.py`) | restart `underway-dashboard`, `underway-telegram` (and `underway-codex` if enabled) |
| `cruisereport/*.py` | restart `underway-report` |
| `deploy/` | nothing installed; the journal says `unit files changed; install them: sudo …/deploy/install.sh` |
| anything else | picked up by the next build |

A merge to `master` is therefore live within minutes. When the journal
reports unit changes, look first and then install:

```sh
/data/underway_server/app/AMUNDSEN/deploy/install.sh --check
sudo /data/underway_server/app/AMUNDSEN/deploy/install.sh
```

## Checking health

```sh
systemctl list-timers 'underway*'                 # when each job last ran and runs next
systemctl --failed                                # anything that failed
journalctl -u underway.service -n 50              # the last builds
journalctl -u underway-dashboard.service -n 20    # the page server
journalctl -u underway-deploy.service -n 20       # what the last pulls brought in
curl -s http://127.0.0.1:8042/api/health          # {"ok":true}
```

A good build ends with a line `[OK] <time> all legs rebuilt in /data/underway_server/www`.
The mirror writes one line per tree to `/data/ship/mirror.log`.

### Health URL and uptime alerts

`/api/health` answers 200 with `{"ok":true}` when the page server can see
the built `index.html` and manifest, and 503 otherwise.

`underway-uptime.timer` runs `python -m dashboard.uptime` every minute. It
checks three URLs: the page server (`http://127.0.0.1:8042/api/health`),
the Caddy route (`http://127.0.0.1/underway/api/health`) and the mDNS name
(`http://underway.local/api/health`), so a failure can be told apart as
backend, front door or name. Two consecutive failures send an outage
notice; the first success after that sends a recovery. Notices go to the
journal and to the operations email (`UNDERWAY_OPS_EMAIL`, else the SMTP
account); failed emails are kept and retried.

| Setting | Default | What |
|---|---|---|
| `UNDERWAY_UPTIME_URLS` | the three above | space-separated health URLs, in `site.env`; replaces the defaults. Include the address ship clients actually use |

State is in `db/uptime.json`; probe history is in the journal
(`journalctl -u underway-uptime.service --since today`).

These checks run on the server itself: they cannot see a powered-off host
or prove that another ship computer can reach it. For that, run the same
probe on another always-on LAN host with its own `UNDERWAY_DB_DIR`, alert
settings and `UNDERWAY_UPTIME_URLS` pointing at the ship-facing address.

### The status page

`/status.html` (also `/underway/status.html`) is not linked from the
dashboard. It shows the current checks, pending email notices, the latest
100 outage and recovery events, daily page views and distinct client
addresses. It refreshes every 30 seconds and marks monitoring older than
three minutes as stale (as here, with the uptime timer stopped). Its data is
`/status.html?format=json`.

![The status page: the three health checks, then visitor counts](img/admin-status.webp)

### Page-view counts

The page reports a view when it is opened and when the main tab changes;
automatic refreshes do not count. Counts are kept per UTC day and page for
90 days in `db/usage.sqlite`, with client addresses stored separately to
count distinct visitors. Only aggregates are shown. No cookies, Wiki slugs,
search text or referrers are stored. Forwarded addresses are trusted only
from the loopback proxy (Caddy).

```sh
UNDERWAY_DB_DIR=/data/underway_server/db /opt/miniforge3/bin/python3 -m dashboard.usage
```

## Routine tasks

| When | Do |
|---|---|
| Start of a leg | set `UNDERWAY_CAMERA_SYNC_LEG` and `UNDERWAY_CAMERA_SHARE` in `site.env` to the new leg and its existing `Pictures/Timelapse` folder. New leg folders on the shares are found by themselves |
| A pull reports unit changes | `deploy/install.sh --check`, then `sudo deploy/install.sh` |
| `site.env` edited | `sudo deploy/install.sh` (restarts the servers whose unit changed); timers pick it up on their next run |
| `underway.env` edited | `sudo systemctl restart underway-dashboard underway-telegram underway-report` |
| End of the cruise | switch to ashore mode ([Running ashore](ashore.html)) |
| Before moving the installation | back up `UNDERWAY_HOME` (minus `www/`, which rebuilds) and `~/.config/underway/` |

!!! note "Aboard only"
    Alerts, the uptime check of `underway.local`, the camera jobs, the
    satellite pictures and Google Calendar push are useful only aboard;
    ashore they are stopped.
