# Troubleshooting

Start with these three commands; most problems show in one of them.

```sh
systemctl --failed
journalctl -u underway.service -n 50
curl -s http://127.0.0.1:8042/api/health
```

## The page

| Symptom | Cause | Fix |
|---|---|---|
| A client cannot load the page | wrong network's address, `https://`, or a name the client cannot resolve | check which network it is on (192.168.3.x Wi-Fi or 10.0.0.x wired) and use that network's address; try the IP before the name; use `http://` (nothing listens on 443); Android does not resolve `underway.local` |
| Nobody can load the page | page server or Caddy down | on the workstation, `curl http://127.0.0.1:8042/api/health`; `systemctl status underway-dashboard caddy underway-mdns` |
| `/api/health` answers 503 | the web root has no `index.html` or manifest yet | wait for the first build, or see "The build" below |
| `underway.local` fails but the IP works | mDNS service down, or the client is on the wired side | `systemctl status underway-mdns`; check `UNDERWAY_IFACE` is the Wi-Fi interface (`ip -br addr`); wired clients use `http://10.0.0.xxx/` |
| The header says a data update failed | a request timed out or the server was briefly away | nothing: the page keeps the last data and retries on the next 30-second poll, on reconnection, or when the page becomes visible |
| The map is blank | the browser has no WebGL, or `static/geo/*.geojson` is not served | check the browser's network panel for 200s on `static/geo/`; try another browser. In the console, `UW.mapView.map` is the MapLibre map |
| "nothing to show" | every leg unticked, or the span holds no data | tick a leg; widen **Show last** |
| A panel says "not found" | a column was renamed in the ACSD files | add a pattern to `config.VARIABLES` ([Data](data.html#when-headers-change-between-legs)) |
| The Wiki tab is missing | no clone at `ARCTIC_HISTORY_ROOT` | clone it and enable `underway-history.timer` ([Integrations](integrations.html#the-wikis-data)) |
| No relief or names on the map | the tile set is not in `UNDERWAY_TILES_DIR` | build or pull it ([Map data](map-data.html)) |
| A waypoint shows only the distance by air | no sea grid, or `scikit-fmm` missing | build `sea-grid/`; `pip install -e '.[routing]'` |

## The build

| Symptom | Cause | Fix |
|---|---|---|
| Build exits with status 2 | neither data root holds legs: the shares or mirror are missing | `ls /data/ship/Data/FULL_CSV /mnt/ship/Data`; the page keeps the last good build meanwhile |
| Timer runs but nothing changes; journal says "Another run is in progress" | a stuck build holds the lock | `systemctl status underway.service`; if no build process is running, remove `app/AMUNDSEN/cache/.run.lock` |
| Data are old, build succeeds | the mirror is not getting new files | `tail /data/ship/mirror.log`: "source missing" or `rsync exit` means the share is not reachable |
| Data are old, mirror is fine | the ship's acquisition system has stopped writing `FULL_CSV` | the keeper gets an operations alert after 30 minutes; the TSG and live-page tail keep the track going meanwhile |
| A build is suddenly slow | the build cache was reset, or a full mirror pass is running | normal after deleting `cache/build/`; full passes run every tenth minute |
| A cast shows old values after the file was corrected | the cast cache still matches | delete its file under `db/casts/<leg>/` |
| Something looks wrong after an update | the derived data predate a change in how they are computed | delete `cache/build/` (and the leg's store if needed); the next build redoes it |

## The shares and the network

| Symptom | Cause | Fix |
|---|---|---|
| `/mnt/ship/Data` is empty after boot | the automount units are not active | `sudo systemctl start mnt-ship-Data.automount mnt-ship-Share.automount` |
| Jobs killed part-way while reading the share | `x-systemd.idle-timeout` on the mount | remove it from `/etc/fstab` |
| Package installs fail aboard | the ship's Fortinet blocks plain-HTTP package requests and outbound port 22 | use HTTPS mirrors; for git and ssh, use a host that is reachable |

## Services and integrations

| Symptom | Cause | Fix |
|---|---|---|
| A merged change is not live | the deploy pull failed, or the change is in `deploy/` | `journalctl -u underway-deploy -n 20`; for unit changes run `sudo deploy/install.sh` |
| A changed `site.env` value has no effect | the unit was not reinstalled or the server not restarted | `sudo deploy/install.sh`; restart the server |
| A new key in `underway.env` has no effect | the servers read it only when they start | `sudo systemctl restart underway-dashboard underway-telegram underway-report` |
| An integration says it is off | its key is missing | add it ([Integrations and keys](integrations.html)) |
| The chat crew is silent | no OpenRouter key and no local model loaded, or `chat-paused` exists | add a Dashboard AI key, or load the local model; remove `~/.config/underway/chat-paused` |
| Alerts are not sent | `underway-alerts.timer` disabled, or SMTP/Telegram keys wrong | `systemctl status underway-alerts.timer`; `journalctl -u underway-alerts -n 30`; the settings page's test buttons |
| Uptime emails about `underway.local` only | mDNS problem, not the dashboard | see the mDNS row above |
| Camera sync fails | the destination folder does not exist (or the share is not mounted) | create `UNDERWAY_CAMERA_SHARE` on the share; the sync never creates it |
| Ice charts never update | GDAL commands or the `ice-charts` extra missing, or no internet | `journalctl -u underway-ice-charts -n 30` |
| Wiki pull fails | the service account's ssh key does not open grid, or grid's code did not fast-forward | `journalctl -u underway-history -n 30`; `tools/history-pull.sh status` |
| Publishing fails | ssh to grid, or grid's rebuild failed | `journalctl -u underway-publish -n 50`; a failed rebuild names its retained stage directory on grid |

## The container

| Symptom | Fix |
|---|---|
| The page does not open | is Docker running? run `start.sh` again; use the numeric address and `http://` |
| No data | fill in **Ship shares** on `/settings`; `bash status.sh` shows "could not mount …" with the reason |
| Data are old | the file server may have restarted; the supervisor retries every minute |
| Forgotten settings password | delete `data/config/admin-password` and run `start.sh` again |
| A job keeps failing | `data/supervisor.json` has each job's last exit code; `docker compose logs` the details |
| The start script refuses to run | the kit is on a USB drive or share: copy it to the computer's disk first |
