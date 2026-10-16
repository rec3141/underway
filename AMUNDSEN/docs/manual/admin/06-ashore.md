# Running ashore

Off the ship the shares and the intranet are gone. The dashboard then runs
from its own copy of the shares, the local mirror, frozen at the end of the
cruise. Ashore mode is three lines in `/etc/underway/site.env`.

## Switching to ashore mode

1. Make sure the mirror is complete while still aboard: run one full pass.

        sudo -u cryomics UNDERWAY_MIRROR_FULL=1 /data/underway_server/app/AMUNDSEN/tools/mirror-share.sh /data/ship full

2. Copy the cruise report template into the mirror, if it is not there: the
   report builder reads
   `$UNDERWAY_SHARE_ROOT/Guidelines, Templates & Forms/Cruise Report Template_2026.docx`.

3. Add to `/etc/underway/site.env`:

        UNDERWAY_ASHORE=2026-10-01
        UNDERWAY_DATA_ROOT=/data/ship/Data
        UNDERWAY_SHARE_ROOT=/data/ship/Share

    `UNDERWAY_ASHORE` is the day the cruise ended (`YYYY-MM-DD`). The two
    roots point every service at the mirror, not only the build.

4. Stop what works only aboard, and restart the servers so they read the new
   values:

        sudo systemctl disable --now underway-camera.timer underway-camera-sync.timer \
            underway-alerts.timer underway-uptime.timer underway-satellite.timer \
            underway-gcal.timer ship-routes.timer
        sudo systemctl restart underway-dashboard underway-report

## What changes

| | Aboard | Ashore |
|---|---|---|
| Mirror pass before each build | yes | no: the mirror is the copy |
| Live leg | the latest leg | none: the page opens on the last season |
| Status line | how old the data are | when the cruise ended |
| Schedule's "now" banner | shown | left out |
| Provisional tail from the intranet live page and the TSG file | added | left out |
| Public rebuild on grid | follows the ship | takes the same from the ship's manifest |

The build, page server, report builder, deploy pull, Wiki pull and web
publishing keep running.

## Back aboard

1. Remove `UNDERWAY_ASHORE`, `UNDERWAY_DATA_ROOT` and `UNDERWAY_SHARE_ROOT`
   from `site.env`.
2. Check the shares mount (`ls /mnt/ship/Data`).
3. Enable the units again and restart the servers:

        sudo systemctl enable --now underway-camera.timer underway-camera-sync.timer \
            underway-alerts.timer underway-uptime.timer underway-satellite.timer \
            underway-gcal.timer
        sudo systemctl restart underway-dashboard underway-report

    Enable `ship-routes.timer` too if the UM VPN is in use.

4. Set the camera leg for the new leg ([Running it day to day](running.html#routine-tasks)).
