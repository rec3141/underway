# How it works

The underway dashboard is a web page that shows the CCGS *Amundsen*'s
underway record, CTD casts, map, schedule, cameras and wiki. A Python build
turns the ship's data files into a static site every minute; a small Python
server serves that site and answers the page's few live requests (chat,
alerts, uploads, the live CTD cast). Nothing needs the internet at run time.

It runs on one Linux workstation aboard, either as a set of systemd units
(the workstation install, [Installing and taking over](installing.html)) or
as one Docker container ([The container and releases](container.html)).

## The parts

| Part | What it is | Where |
|---|---|---|
| Build | `python -m dashboard build`: reads the data files, updates one SQLite store per leg, writes the site's JSON and `index.html` | `underway.service`, every minute |
| Page server | `python -m dashboard serve`: serves the built site and the `api/` requests | `underway-dashboard.service`, port 8042 |
| Front door | Caddy on port 80: `/underway/` and `/report/`, and the `underway.local` name | `/etc/caddy/Caddyfile` |
| Local mirror | `tools/mirror-share.sh`: an rsync copy of the parts of the ship's shares the build reads | `/data/ship` |
| Cruise report builder | `python -m cruisereport serve`: a separate page at `/report/` | `underway-report.service`, port 8044 |
| Jobs | alerts, Google Calendar, satellite pictures, ice charts, camera timelapses, uptime checks, the Wiki pull, the deploy pull, web publishing | one systemd timer each ([Running it day to day](running.html)) |

The code is the `AMUNDSEN/` directory of the repository: `dashboard/` (the
Python package and the page's `static/` files and `templates/`),
`cruisereport/`, `deploy/` (unit templates, Caddyfile, container kit) and
`tools/` (shell and Python tools).

## How data flows

```
 Ship's NAS 10.0.0.xxx                    workstation (/data/underway_server)
 //Data  //Share  (CIFS)
   |  /mnt/ship/Data, /mnt/ship/Share
   v
 mirror-share.sh (rsync) --> /data/ship/{Data,Share}       intranet 10.0.0.xxx
                                   |                        (schedule, live page)
                                   v                              |
                    python -m dashboard build  <------------------+
                     |  db/<leg>.db (SQLite stores)
                     |  cache/      (build cache)
                     v
                    www/  (index.html, data/*.json, static/, manual/)
                     |
         python -m dashboard serve :8042  <--  Caddy :80  <--  browsers
                     |                                      (ship Wi-Fi, LAN)
                     |
   publish-web.sh push (every 5 min)  -->  grid  -->  web server (cryomics.org)
```

1. The ship's file server (`//10.0.0.xxx/Data` and `//10.0.0.xxx/Share`) is
   mounted at `/mnt/ship/Data` and `/mnt/ship/Share`.
2. Each build first runs `tools/mirror-share.sh`, which copies only what the
   dashboard reads onto local disk (`UNDERWAY_MIRROR`, `/data/ship`). Every
   other read is local.
3. The build ingests new or changed day files into `db/<leg>.db`, combines
   the legs, computes derived variables and the surprise score, and writes
   the site into `www/`. Files are written under a temporary name and
   renamed, so a page never reads a half-written file.
4. The page server serves `www/` on port 8042; Caddy on port 80 is what
   people type.
5. Optionally, `underway-publish.timer` pushes the site and its source
   files to grid, a university server, which rebuilds a thinned public copy
   and puts it on the public web server ([Publishing to the web](public-web.html)).

The main input is Amundsen Science's ACSD system: one
`ACSD_YYYYMMDD.csv` per day in `Data/FULL_CSV/<leg>/`, semicolon separated,
two header lines (variable name, then instrument), 10-second cadence, about
68 columns. The other inputs (TSG files, event log, CTD logbooks and casts,
MVP tows, LADCP profiles, the intranet schedule) are listed in
[Data, builds and storage](data.html).

## Where everything lives

| Path | What |
|---|---|
| `/etc/underway/site.env` | the installation's settings (no secrets) |
| `~/.config/underway/` | the service account's secrets: `underway.env`, `gcal-sa.json` |
| `/data/underway_server/` | `UNDERWAY_HOME`, the installation |
| `…/app/` | a clone of the repository on `master`: what the services run |
| `…/db/` | per-leg stores and the services' own state files |
| `…/cache/` | build cache |
| `…/www/` | the built site |
| `…/chat/` | the chat database |
| `…/camera360/` | the camera timelapses |
| `…/report/` | the cruise report builder's drafts and uploads |
| `…/archive/` | Amundsen Science's 2002–2024 archive |
| `…/ice/` | the ice camera's results |
| `/data/ship/` | the local mirror of the shares |
| `/data/gis/tiles/` | optional map tiles (relief, coastline, names, sea grid) |
| `/data/dev/arctic-history/` | the Wiki tab's data, pulled from grid |

The full list is in [Reference](reference.html).

## Aboard, ashore and public

The same code runs in three situations.

| | Aboard | Ashore | Public web copy |
|---|---|---|---|
| Data | the shares, through the mirror | the mirror only, frozen | grid's copy of the ship's source files |
| Live leg, live CTD, intranet schedule | yes | no | no |
| Alerts, chat, uploads, cameras | yes | partly (see [Running ashore](ashore.html)) | no: every `api/` request answers 503 |
| Record released | everything | everything | only what Amundsen Science has released |

Ashore mode is one setting, `UNDERWAY_ASHORE`, in `site.env`
([Running ashore](ashore.html)). This workstation is in ashore mode now.

## Reaching the dashboard aboard

| From | Address |
|---|---|
| ship Wi-Fi `AMD-WiFi` (192.168.3.x) | `http://underway.local` or `http://192.168.3.xxx/underway/` |
| wired ship LAN (10.0.0.x) | `http://10.0.0.xxx/` |
| the workstation itself | `http://127.0.0.1:8042/` |

Plain `http://` only: nothing listens on port 443. `underway.local` is
published by mDNS on the Wi-Fi interface only, and Android does not resolve
mDNS names; give Android users the IP address. The manual itself is in the
site at `manual/`, and an unlinked status page is at `/status.html`.
