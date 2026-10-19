# The container and releases

For a machine that runs only Docker (Linux, Windows or macOS), the whole
dashboard is one image. It holds the page server, the build, alerts,
calendar, satellite, cameras, ice charts, the cruise report builder
(`/report/`) and the game (`/game/`, from github.com/rec3141/amundsen-game),
behind Caddy on port 80. Settings are made on the site's `/settings` page,
not in files.

## What is in the image and what is not

| In the image | Not in the image |
|---|---|
| page server, build, cruise report builder, game, Caddy | publishing to the web |
| alerts, Telegram bot, Google Calendar, satellite, ice charts, uptime check | the Wiki pull from grid: the Wiki is whatever snapshot the data folder holds |
| camera timelapses, the ice camera (on its own OpenRouter key) | the Codex bot |
| SMB mounting of the ship's shares | a local GPU: `UNDERWAY_LLM=0`; AI parts run on OpenRouter once they have a key ([Integrations](integrations.html#ai-models)) |

## The supervisor

`python -m dashboard supervise` is the container's init. It does what the
systemd units do on a workstation:

- keeps the servers up, restarting any that exits (backing off up to five
  minutes for one that keeps dying): the page server (8042), the report
  builder (8044), the game (8050), Caddy (80), the Telegram bot while it has
  a token, and the ice camera while it has its own OpenRouter key;
- runs the jobs on the timers' schedules, never two runs of one job at once:

| Job | Every | Runs when |
|---|---|---|
| build (mirror + build) | minute | always |
| alerts | 2 minutes | always |
| uptime | minute | always |
| gcal | 5 minutes | `config/gcal-sa.json` exists |
| satellite | 30 minutes | Copernicus keys set |
| camera | hour | the camera pictures folder exists |
| camera-sync | day, 00:20 UTC | **Copy timelapses to** set |
| ice-charts | 6 hours | always |

- mounts the ship's SMB shares at `/mnt/ship/Data` and `/mnt/ship/Share`
  from the settings, unless something is already mounted or bind-mounted
  there.

It records each server's state and each job's last run, duration and exit
code in `data/supervisor.json`. Its output, with each line prefixed by the
part's name, is the container's log.

## The data folder

Everything the container keeps is one host folder, `data/` beside the kit
(`UNDERWAY_DATA` in `compose.yaml`), mounted at `/underway`:

| Folder | What |
|---|---|
| `config/` | `site.env` (plain settings), `underway.env` (secrets, mode 600), `gcal-sa.json`, `admin-password` |
| `db/`, `cache/`, `www/` | stores, build cache, the built site |
| `mirror/` | the local mirror of the shares |
| `tiles/` | map tiles |
| `arctic-history/` | the Wiki snapshot |
| `archive/` | Amundsen Science's archive |
| `camera360/`, `ice/` | timelapses, ice camera results |
| `report/`, `game/` | report drafts, the game's state |
| `chat/` | the chat database |
| `supervisor.json` | the supervisor's state |

The paths inside the container are fixed by the image (`UNDERWAY_HOME=/underway`
and the rest); the supervisor ignores those keys if they appear in the
settings files. Back up `data/` at the end of a leg with the container
stopped; on Linux its files belong to root, so copy them with `sudo`.

## The kit

`deploy/container/` is what goes to the people who run it:

| File | What |
|---|---|
| `HOW-TO.txt` | one page for a non-technical keeper: install Docker, start, settings, everyday use, trouble |
| `start.sh`, `start.bat` | refuse to run from a USB drive or network share; load the image if needed; unpack the seed on the first start; `docker compose up -d`; make the settings password on the first run and print the addresses |
| `stop.sh`, `stop.bat` | `docker compose down` (nothing is lost) |
| `status.sh` | `docker compose ps` and the last 40 log lines |
| `compose.yaml` | the container: port 80, the data folder, the capabilities SMB mounting needs |
| `Caddyfile` | the container's front door |
| `make-release.sh` | pack a frozen release onto a drive |

`compose.yaml` reads three values:

| Setting (environment or `.env`) | Default | What |
|---|---|---|
| `UNDERWAY_VERSION` | `latest` | the image tag; the release writes it into `.env` |
| `UNDERWAY_HTTP_PORT` | `80` | the host port |
| `UNDERWAY_DATA` | `./data` | the data folder |

The container has `restart: unless-stopped` (it starts again after a
reboot), the `SYS_ADMIN` and `DAC_READ_SEARCH` capabilities and
`apparmor:unconfined` (mounting CIFS from inside needs them), and a health
check on `/api/health`.

A host that mounts the shares itself, or a Windows machine with mapped
drives, can bind them at `/mnt/ship/Data` and `/mnt/ship/Share` instead
(commented lines in `compose.yaml`); the supervisor leaves a share the host
provides alone.

### Addresses

| Path | Goes to |
|---|---|
| `/` and `/underway/` | the dashboard |
| `/report/` | the cruise report builder |
| `/game/` | the game |
| `/settings` | the settings page |
| `/status.html` | the status page |

## Everyday commands

From the kit folder:

```sh
bash start.sh                  # start (Windows: start.bat)
bash stop.sh                   # stop  (Windows: stop.bat)
bash status.sh                 # state and recent log
docker compose logs -f         # follow the log
docker exec -it underway bash  # a shell inside
```

## The settings page

`/settings` is a table of keys and account settings, in groups:

| Group | Settings |
|---|---|
| Ship shares | server address (10.0.0.xxx), Data share name, science share name, user name, password |
| Telegram | bot token, your chat id |
| Email alerts | mail server, port, account, password, send as, secure connection, replies go to, problem alerts to |
| Satellite | Copernicus client id and secret |
| Google Calendar | the service account key (a file upload) |
| AI models | an OpenRouter key and a model for each of Dashboard AI, Ice camera, Cruise report sheets |
| Cameras | pictures folder, seconds between frames, layout, width, current leg, copy timelapses to |
| General | time zone, SeaSave address |

The settings keys are listed in [Reference](reference.html#settings).

How it works:

- Secrets go to `config/underway.env` (mode 600), everything else to
  `config/site.env`. Comments, order and keys the page does not know are
  kept.
- Secrets are never sent back to the browser: the page shows only whether
  one is set and its last four characters. An empty secret box leaves the
  secret as it is; a tick box beside a secret removes it. An emptied plain
  setting is removed, so its default applies.
- Plain settings must be shell-safe (no spaces, quotes or `$`), since the
  shell tools source the file.
- A save touches `config/.reload`; the supervisor then restarts the servers
  and remounts the shares with the new values. Jobs read both files at each
  start. New data shows in about a minute.
- Buttons send a test Telegram message, a test email, and check the AI
  keys ("Check the AI keys": asks OpenRouter about each saved key at no
  cost, and checks each model exists and, where the part sends pictures,
  reads them).

### The admin password

There is one admin password, stored as a scrypt hash in
`config/admin-password`. The first password is set only from the machine,
never from the web:

```sh
docker exec -it underway python3 -m dashboard set-admin-password           # type it
docker exec underway python3 -m dashboard set-admin-password --random      # make one up and print it
```

The start scripts run the second on the first start and save the result in
`ADMIN-PASSWORD.txt`. Once logged in, the password can be changed on the
page with the current one. A forgotten password: delete
`data/config/admin-password` and run the start script again.

Other protections:

- Sessions are signed cookies valid 12 hours (key in `config/.session-key`);
  forms carry a CSRF token; after five wrong passwords from one address,
  further tries are slowed (up to 15 minutes).
- The site is plain HTTP on the ship's network, so the password crosses it
  unencrypted.

!!! note "On a workstation install"
    The page server answers `/settings` there too. Its secrets go to the
    account's `underway.env` and its plain settings to
    `~/.config/underway/site.env`; the units read both, the page's
    `site.env` after `/etc/underway/site.env`, so a value saved on the page
    wins. There is no supervisor: the timers' jobs take a saved setting at
    their next run, and the servers once restarted
    (`sudo systemctl restart underway-dashboard underway-telegram underway-report`),
    as the page says after a save.

## Making a release

A release is a git tag and a drive (or a folder on the ship's file server,
`\\10.0.0.xxx\Share\Software\underway-<version>`).

```sh
git tag 2026.10 && git push origin 2026.10
deploy/container/make-release.sh 2026.10 /media/usb/underway --with-data
```

`make-release.sh VERSION OUTDIR [--with-data]`:

1. checks the checkout is clean and at the tag;
2. builds the image from the checkout and the game checkout
   (`AMUNDSEN_GAME`, default `/data/dev/amundsen-game`) and saves it as
   `OUTDIR/underway-VERSION.tar.gz`;
3. copies the kit beside it and writes `.env` with `UNDERWAY_VERSION`;
4. with `--with-data`, packs a seed of this installation into `OUTDIR/seed/`,
   one tar per part: `tiles`, `arctic-history` (without `.git`), `db`,
   `cache`, `archive`, `report`, `game`, `ice` (results, not pictures). The
   start scripts unpack it into `data/` on the first start, so the next
   machine starts where this one is instead of rebuilding for hours.

Seed details: parts are staged under `STAGE` (default
`/data/underway-release-stage`, on the same filesystem as the sources, so
staging is hard links); SQLite databases are replaced by consistent
snapshots; symbolic links are packed as the files they point to. Secrets are
never packed: they go in again on the settings page. A full release with the
archive is tens of GB.

To build an image by hand:

```sh
docker build --build-context game=/data/dev/amundsen-game -t underway:dev .
```

The image does not update itself: a new release is a new drive.

## Installing a release

The keeper's steps are in `HOW-TO.txt`. In short:

1. Install Docker (Docker Desktop on Windows or macOS;
   `sudo apt install docker.io docker-compose-v2` on Ubuntu). The computer
   must be on the ship's wired network and stay on.
2. Copy the whole release folder onto the computer's own disk (it writes to
   `data/` all the time; the start scripts refuse to run from a drive or
   share). Keep as much space free again for the first unpacking.
3. Run `bash start.sh` or `start.bat`. The first start unpacks the seed (up
   to half an hour) and loads the image; afterwards `seed/` can be deleted.
4. Open `http://<address>/settings`, log in, fill in at least **Ship
   shares** and **Cameras**, and save.
5. Tell people the address, `http://<this computer's address>/`.

At the start of each leg, change the camera leg and timelapse folder on the
settings page; nothing else is needed.
