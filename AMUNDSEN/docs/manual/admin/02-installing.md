# Installing and taking over

This chapter installs the dashboard on a Linux workstation with systemd, or
moves it to another account or machine. For a machine that runs only Docker,
see [The container and releases](container.html) instead.

## Requirements

| What | Detail |
|---|---|
| OS | Linux with systemd (the shell tools and units are Linux-only) |
| Python | 3.11 or later, with `pandas`, `numpy`, `jinja2`, `scipy`, `xlrd`, `openpyxl` (installed by `pip install -e .`) |
| Network | the ship's wired LAN (10.0.0.x) for the shares and the intranet; the Wi-Fi for `underway.local` |
| Disk | about 15 GB for the first mirror of the shares (mostly per-cast plot HTML), plus the stores, cache and site |
| Packages | `rsync`, `cifs-utils`, `flock` (util-linux), Caddy, avahi (for `underway.local`) |
| Optional | `ffmpeg` with libx264 (camera timelapses); GDAL commands `gdal_translate`, `gdalwarp`, `gdalinfo` (ice charts); GDAL with Python bindings (building map tiles) |

The page's JavaScript libraries (Plotly for the charts, MapLibre GL JS for
the map) are committed under `dashboard/static/`; nothing is fetched from
the internet at run time.

### Optional Python extras

Install with `pip install -e 'AMUNDSEN[extra,extra,...]'`.

| Extra | For |
|---|---|
| `chat` | the AI crew in the chat (HTTP client) |
| `gcal` | Google Calendar push |
| `cameras` | camera timelapses (Pillow) |
| `ice-charts` | Canadian Ice Service charts (`pyshp`, `pyproj`) |
| `report` | the cruise report builder (`python-docx`, `matplotlib`, `pyproj`, Pillow) |
| `routing` | sea-route distances (`scikit-fmm`; builds from source, needs a compiler) |
| `sampling` | the experimental live sampling adviser |
| `assets` | only to refresh the bundled `plotly.min.js` |

The usual set aboard is `chat,gcal,cameras,ice-charts,report`.

## Install, step by step

The steps below use the Amundsen workstation's values: installation in
`/data/underway_server`, service account `cryomics`. Substitute your own.

1. **Mount the ship's shares.**

        sudo tools/ship-smb-setup.sh

    It asks once for the share user (default `science`) and password, writes
    them to `/etc/ship-smb.creds` (root only, mode 600), adds two CIFS lines
    to `/etc/fstab` and starts their automount units. See
    [Mounting the shares](#mounting-the-shares).

2. **Clone the repository as the service account** into
   `UNDERWAY_HOME/app`, and install the packages into the interpreter the
   services will use:

        git clone <repository url> /data/underway_server/app
        /opt/miniforge3/bin/python3 -m pip install -e \
            '/data/underway_server/app/AMUNDSEN[chat,gcal,cameras,ice-charts,report]'

3. **Write the site file.**

        sudo mkdir /etc/underway
        sudo cp /data/underway_server/app/AMUNDSEN/deploy/site.env.example /etc/underway/site.env
        sudoedit /etc/underway/site.env

    Set at least `UNDERWAY_HOME`, `UNDERWAY_USER`, `UNDERWAY_PYTHON`, the
    Wi-Fi interface `UNDERWAY_IFACE` (see `ip -br addr`) and the camera
    lines for the current leg. See [The site file](#the-site-file).

4. **Put the secrets in place** as the service account. See
   [Secrets](#secrets).

5. **Install and start the units.**

        sudo /data/underway_server/app/AMUNDSEN/deploy/install.sh --enable

    This installs every unit and enables the core ones: the build, the page
    server, the deploy pull, the cruise report builder, the uptime check and
    the ice charts. Enable the others as their credentials go in:

        sudo systemctl enable --now underway-alerts.timer underway-telegram.service \
            underway-gcal.timer underway-satellite.timer \
            underway-camera.timer underway-camera-sync.timer underway-mdns.service

6. **Set up Caddy** on port 80. See [Caddy and the mDNS name](#caddy-and-the-mdns-name).

7. **Check the first build.**

        journalctl -u underway.service -f          # watch it
        curl -s http://127.0.0.1:8042/api/health   # {"ok":true} once the site exists

    The first build mirrors about 15 GB and ingests every leg; it takes a
    while. Later builds take seconds.

## The site file

`/etc/underway/site.env` holds everything particular to the machine. It is
world-readable: nothing secret goes in it.

- `deploy/install.sh` fills the unit templates in `deploy/` from it.
- Each unit reads it into its environment when it starts.
- The shell tools source it.

So a changed value reaches a timer's job on its next run, a server on its
restart, and a unit file on the next `install.sh`.

Format: `KEY=value`, no spaces around `=`, comments on their own lines only
(systemd reads a trailing `# ...` as part of the value). The format is shared
by systemd and bash.

| Key | Example (Amundsen) | What |
|---|---|---|
| `UNDERWAY_HOME` | `/data/underway_server` | the installation directory |
| `UNDERWAY_USER` | `cryomics` | the account the services run as; owns `UNDERWAY_HOME` |
| `UNDERWAY_PYTHON` | `/opt/miniforge3/bin/python3` | the interpreter with the dashboard's packages |
| `UNDERWAY_CONFIG` | `~UNDERWAY_USER/.config/underway` | where the secrets are |
| `UNDERWAY_MIRROR` | `/data/ship` | the local mirror of the shares |
| `UNDERWAY_TILES_DIR` | `/data/gis/tiles` | optional map tiles |
| `ARCTIC_HISTORY_ROOT` | `/data/dev/arctic-history` | the Wiki tab's clone; without it the tab is absent |
| `ARCTIC_HISTORY_REMOTE` | `grid:/data/dev/arctic-history` | where the Wiki pull comes from (ssh host:path) |
| `UNDERWAY_PORT` | `8042` | the page server's port |
| `UNDERWAY_IFACE` | `wlo1` | the Wi-Fi interface: `underway.local` is published on it |
| `UNDERWAY_MDNS_NAME` | `underway.local` | the mDNS name |
| `TZ` | `America/Toronto` | the ship's time zone |
| `UNDERWAY_CAMERA_*` | | the camera timelapses ([Integrations](integrations.html#cameras)) |
| `UNDERWAY_PUBLISH_*` | | publishing to the web ([Publishing to the web](public-web.html)) |

Every setting, with its default, is in [Reference](reference.html#settings).

## Secrets

The secrets live in the service account's `~/.config/underway/` (or
`UNDERWAY_CONFIG`), never in git and never in `site.env`.

| File | What | Mode |
|---|---|---|
| `underway.env` | every secret, `KEY=value`: Telegram, the SMTP account for alerts, the operations address, Copernicus, OpenRouter keys. Start from `deploy/underway.env.example` | 600 |
| `gcal-sa.json` | the Google service account's key | 600 |
| `admins.json` | optional: a JSON list of chat names that may clear Wiki review flags | |

```sh
install -m 600 /data/underway_server/app/AMUNDSEN/deploy/underway.env.example \
    ~/.config/underway/underway.env
```

A new secret is a line in `underway.env`, never a new file. Each
integration is off, and the page says so, while its values are missing.
The units load `underway.env` when they start: restart
`underway-dashboard` and `underway-telegram` after an edit. What each key
does is in [Integrations and keys](integrations.html).

## The install script

`deploy/install.sh` renders each unit template in `deploy/` (every
machine-specific value is a `@NAME@` placeholder filled from `site.env`),
installs those whose content changed into `/etc/systemd/system/`, reloads
systemd and restarts the long-running servers whose unit changed. A oneshot
job takes its new unit on its next run.

| Command | What |
|---|---|
| `deploy/install.sh --check` | show what would change; installs nothing; no sudo needed |
| `sudo deploy/install.sh` | render, install, reload, restart what changed |
| `sudo deploy/install.sh --enable` | the same, then enable and start the core units |

It also creates the state directories (`db`, `cache`, `www`, `chat`,
`camera360`, `report`) owned by the service account, and installs the Codex
bot's AppArmor profile where AppArmor is present.

Run it again whenever `site.env` or anything under `deploy/` changes. The
deploy pull says so in its journal when a pull brought unit changes
([Running it day to day](running.html#the-deploy-pull)).

## Caddy and the mDNS name

Caddy on port 80 is the address people use. Copy the Caddyfile and reload:

```sh
sudo cp /data/underway_server/app/AMUNDSEN/deploy/Caddyfile /etc/caddy/Caddyfile
sudo systemctl reload caddy
```

| Request | Goes to |
|---|---|
| `http://<ip>/` | redirected to `/underway/` |
| `http://<ip>/underway/…` | the page server, `127.0.0.1:8042` |
| `http://<ip>/report/…` | the cruise report builder, `127.0.0.1:8044` |
| `http://<ip>/status.html` | the page server's status page |
| `http://underway.local/…` | the page server (and `/report/` to the builder) |
| anything else on `:80` | `/var/www/html` and the `:5000` API on this machine, unchanged |

`underway-mdns.service` runs `tools/publish-mdns.sh`, which publishes
`UNDERWAY_MDNS_NAME` for the address of `UNDERWAY_IFACE` only. Avahi cannot
scope a record to one interface, and a client on the Wi-Fi handed the wired
address cannot reach it, so the name is published for the Wi-Fi address
alone.

Firewall: nothing on the workstation blocks port 80 or 8042 (`ufw` is
installed but inactive). The ship's Fortinet blocks outbound port 22 and
plain-HTTP requests with a package manager's User-Agent; HTTPS works.

## Mounting the shares

`sudo tools/ship-smb-setup.sh` writes these to `/etc/fstab`:

```
//10.0.0.xxx/Data  /mnt/ship/Data  cifs credentials=/etc/ship-smb.creds,uid=…,gid=…,file_mode=0664,dir_mode=0775,iocharset=utf8,vers=3.0,_netdev,nofail,noauto,x-systemd.automount 0 0
//10.0.0.xxx/Share /mnt/ship/Share cifs …same options…
```

The mounts belong to the service account and mount on first access
(`ls /mnt/ship/Data`).

!!! warning
    Do not add `x-systemd.idle-timeout` to these lines. An idle unmount
    stops every unit that requires the mount, and a long job reading the
    share is killed part-way.

The build itself does not need the mount to stay up: it reads the local
mirror, and only the mirror pass touches the share.

## Taking over on another machine or account

The installation is portable: everything machine-specific is in
`/etc/underway/site.env`, and everything else is in `UNDERWAY_HOME` and the
account's `~/.config/underway/`.

1. On the new machine, follow [Install, step by step](#install-step-by-step)
   with the new account and paths in `site.env`.
2. To keep the history instead of rebuilding it, stop the old units and copy
   `UNDERWAY_HOME/{db,cache,chat,camera360,report,archive,ice}` and the
   mirror (`/data/ship`) across, keeping ownership with the new account.
   The stores are derived data and rebuild by themselves if left out; the
   chat, the subscriptions in `db/alerts*.json`, the waypoints and the
   report drafts are not.
3. Copy the secrets (`underway.env`, `gcal-sa.json`, `admins.json`) into the
   new account's `~/.config/underway/`, mode 600.
4. Give the service account an ssh key that grid accepts, for the Wiki pull
   (`underway-history.timer`) and web publishing (`underway-publish.timer`),
   then enable those timers.
5. Map tiles (`/data/gis/tiles`) are not in the installation directory:
   copy them, or rebuild them ([Map data](map-data.html)).

Nothing the dashboard needs lives only in `~/bin` or the crontab.
