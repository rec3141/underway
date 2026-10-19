# Publishing to the web

A public copy of the dashboard is at <https://cryomics.org/underway/>. It is
published in two hops: the workstation sends its source files and built site
to a publishing host, which rebuilds the public copy (thinned to what Amundsen
Science has released, with the Wiki layer) and sends it to the web server. In
this installation the publishing host is grid (a University of Manitoba
server) and the web server is DreamHost. Both are settings
([Settings](#settings)): any host the workstation can reach by ssh, and that
can reach the web server, can be the publishing host. This chapter calls it
grid.

```
ship  --(rsync over ssh, every 5 min)-->  grid  --(rebuild, then rsync)-->  web server (DreamHost)
```

`tools/publish-web.sh` does both hops; `underway-publish.timer` runs
`tools/publish-web.sh push` on the ship every five minutes.

## What a push does

1. **Source files, ship → grid.** `tools/publish-sources.sh` mirrors the
   ACSD records (current and archived), TSG files, event logs, Rosette
   logbooks and bottle files and MVP profiles from the local mirror to
   `grid:/data/underway_server/source`. CTD `.cnv` files and rendered cast
   HTML are excluded; TSG `.cnv` files are included. The recorded live
   readings (`db/live_scrape/`, `db/provisional_tsg.csv`) go to
   `source/runtime/`. Accounts, subscriptions and credentials are never sent.
   Only this dedicated `source/` directory is pruned.
2. **The built site, ship → grid.** The ship's web root goes to grid's
   `www/`, without the chart windows (`data/w-*.json`), the track
   (`data/track/`) and the Wiki layer, which grid makes itself.
3. **On grid.** Under grid's publishing lock, grid fast-forwards its
   checkout to `origin/master`, then runs `publish-web.sh rebuild-deploy`:
   - `build --tracks-only --public` from `source/` and the recorded live
     readings, in a staging directory: the chart windows and track, thinned
     to what Amundsen Science has released. Stores and caches stay in grid's
     `db/` and `cache/` for reuse; unchanged track chunks are reused. A
     failed rebuild stops the deploy and keeps the stage for inspection;
   - renders the Wiki layer from grid's own database;
   - copies the page's assets, writes the web server's `.htaccess` and
     `api-off.json`, marks the manifest `public` and re-renders the page;
   - rsyncs everything except cameras, journal photographs and tiles to the
     web server, then the Wiki's files (all of them unless
     `UNDERWAY_PUBLISH_MAX_MB` caps their size).

Each transfer logs `Total bytes sent` and `Total bytes received`: the
compressed rsync counts, before SSH overhead. `Total transferred file size`
counts whole changed files even when only their differences crossed.

## Commands

On the ship:

```sh
tools/publish-web.sh push              # what the timer runs
tools/publish-web.sh push --dry-run    # show the source and site transfers; no rebuild or deploy
tools/publish-web.sh status            # what is where, and when
journalctl -u underway-publish.service -n 50
```

On grid (each takes grid's publishing lock):

| Command | What |
|---|---|
| `publish-web.sh rebuild-deploy` | rebuild the public windows and track, then deploy (what a push asks for) |
| `publish-web.sh rebuild` | only the rebuild |
| `publish-web.sh deploy` | deploy without rebuilding |
| `publish-web.sh history` | only the Wiki layer |
| `publish-web.sh static` | only the page's assets |
| `publish-web.sh tiles` | copy the map tiles to the web server (by hand, the first time) |

## What the web copy releases

The web copy releases only what Amundsen Science has released itself
(`dashboard/public.py`):

| | Released by Amundsen Science | Web copy |
|---|---|---|
| Underway, from 2026 | a position every 5 min and a reading every 15 min (data.amundsen.ulaval.ca) | positions every 5 min, every variable every 15 min; no window shorter than 12 h |
| Underway, 2025 | TSG per minute and navigation per second (ERDDAP `amundsen12715`, `amundsen12447`) | TSG and navigation per minute, the rest every 15 min |
| CTD, LADCP, trace-metal and MVP profiles | ERDDAP `amundsen12713`, through 2024 | casts of 2024 and before; later station markers stay |
| Schedule whiteboard | not published | withheld |

The deploy lists what it withheld in `www/.public-filter` as rsync hide
rules, which also delete any copy already on the web server. When Amundsen
Science releases more, change `PER_MINUTE_UNTIL` or
`CASTS_PUBLISHED_THROUGH` in `dashboard/public.py`.

What stays aboard: the shipboard cameras (`/camera/`), the nature journal's
photographs and the `/Share` gallery (`/journal/`), and the ship's own map
tiles.

No service runs on the web server: every `api/` request answers 503 with a
JSON body, and the manifest is marked `public` and lists no cameras. The
page sees that mark and hides what only runs aboard: the Photos and Chat
tabs and the chat side bar, **Feedback**, the live cast, the schedule's alert
bells and form, the Wiki's review flags and **Ask**, the ice-camera panel,
the intranet feed, the photos map layer and the KMZ drop. The **Sources** tab
says so in place of the intranet links.

## Map tiles

The web copy's map draws grid's own tiles from grid's `UNDERWAY_TILES_DIR`:
`gebco/` and any `gebco-<ramp>/`, `survey/`, `coast/`, `names/`. Without a
`gebco/` there, the web copy falls back to Natural Earth's depth bands.

`publish-web.sh tiles` copies them to the web server by hand the first time
(a few hundred thousand small files). After that, each deploy re-syncs a set
whose directory has changed. Build the relief on grid with
`tools/rerender-gebco.sh`, as on the ship ([Map data](map-data.html)).

## Settings

In `/etc/underway/site.env` on the ship (and the same file on grid):

| Setting | Default | Side | What |
|---|---|---|---|
| `UNDERWAY_PUBLISH_REMOTE` | `grid:/data/underway_server/www` | ship | where the web root goes |
| `UNDERWAY_PUBLISH_SOURCE_REMOTE` | the remote's parent + `/source` | ship | where the source files go; must be a dedicated absolute directory ending in `/source` |
| `UNDERWAY_PUBLISH_APP` | `/data/dev/underway/AMUNDSEN` | ship | grid's checkout |
| `UNDERWAY_PUBLISH_MIRROR` | `/data/underway_server/www` | grid | grid's copy of the web root |
| `UNDERWAY_PUBLISH_STATE_DIR` | the mirror's parent | grid | grid's `db/`, `cache/`, locks |
| `UNDERWAY_PUBLISH_SOURCE_DIR` | state dir + `/source` | grid | where the source files are |
| `UNDERWAY_PUBLISH_TRACK_PYTHON` | state dir + `/.venv/bin/python` | grid | the interpreter for the rebuild |
| `UNDERWAY_PUBLISH_TARGET` | `dreamhost:cryomics.org/underway` | grid | the web server, ssh host:path |
| `UNDERWAY_PUBLISH_URL` | `https://cryomics.org/underway/` | both | where it is served |
| `UNDERWAY_PUBLISH_MAX_MB` | `0` (no cap) | grid | size cap on each Wiki file |
| `ARCTIC_HISTORY_ROOT` | `/data/dev/arctic-history` | grid | the Wiki's database |
| `UNDERWAY_PYTHON` | the history clone's `.route-venv` | grid | the interpreter for the Wiki layer (Python 3.11+, with Pillow) |

## Setting it up

On grid, once:

```sh
python3 -m venv /data/underway_server/.venv
/data/underway_server/.venv/bin/pip install -e '/data/dev/underway/AMUNDSEN[cameras]'
```

Grid also needs its checkout at `UNDERWAY_PUBLISH_APP`, an ssh key the web
server accepts, the archive ([Amundsen Science's archive](archive.html)) and
its tiles.

On the ship: the service account's ssh key must open grid. Then:

```sh
sudo /data/underway_server/app/AMUNDSEN/deploy/install.sh
sudo systemctl enable --now underway-publish.timer
```

The first push copies and ingests every source record and can take hours
(the unit allows two); later pushes reuse the stores and send only changes.
