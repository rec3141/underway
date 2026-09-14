# Usage and availability

The browser reports a page view on opening the site and switching main tabs.
Reloads count as new views; automatic data refreshes do not. Counts are grouped
by UTC day and main page (including Wiki as one page), retained for 90 days in
`UNDERWAY_DB_DIR/usage.sqlite`. Client IP addresses are stored separately by UTC day to count distinct addresses
for today, the last seven days and the retained 90 days. Only aggregate counts
are returned on the status page. Counting starts when this feature is deployed;
older page-view records cannot supply historical IP counts. A shared connection
may represent several people, and one person may use multiple addresses. No
cookies, Wiki slugs, search text or referrers are stored. Browsers that block
usage beacons may be missed. Forwarded addresses are accepted only from the
loopback reverse proxy; direct clients use their socket address.

Read counts using the service account and its database directory:

```sh
UNDERWAY_DB_DIR=/data/underway_server/db python -m dashboard.usage
```

`/api/health` returns 200 with `{"ok":true}` when the Python server can see the
built index and manifest; otherwise it returns 503. The independent
`underway-uptime.timer` checks this once a minute through the backend, the
Caddy `/underway/` route, and the mDNS hostname. Two consecutive failures trigger
an outage notice; the first successful check triggers recovery. Notifications
are written to the server journal and sent to the existing operations email.
Failed emails are retained and retried. An unavailable Internet connection may
delay email delivery until connectivity returns.

After deploying the code, install and enable the timer:

```sh
sudo AMUNDSEN/deploy/install.sh
sudo systemctl enable --now underway-uptime.timer
```

The installer also enables it with the other core units when run with `--enable`.
The installed site's environment and alert credentials are loaded by the unit.
Optional `UNDERWAY_UPTIME_URLS` is a space-separated list of health URLs in
`site.env`; it replaces the defaults. Include the actual ship-facing address
if clients use an IP or a different hostname. All checks bypass HTTP proxy
environment variables. Probe results, response times, incident state and pending
notices are in `UNDERWAY_DB_DIR/uptime.json`; probe history is in the journal:

```sh
journalctl -u underway-uptime.service --since today
systemctl status underway-dashboard caddy underway-mdns
```

These checks run on the server. They can distinguish a backend failure from a
front-door or hostname failure, but cannot detect a powered-off host or prove
reachability from another ship computer. For that coverage, run the same probe
on a separate always-on LAN host with its own database directory, alert
configuration and `UNDERWAY_UPTIME_URLS` pointing at the ship-facing site.

The unlinked `/status.html` page reports current checks, pending email notice
count, the latest 100 outage/recovery events recorded after its installation,
daily page-view counts, and unique client IP counts. It refreshes every 30 seconds and marks monitoring
older than three minutes as stale. It is also available at
`/underway/status.html`; no dashboard navigation link is added. The data response
is `/status.html?format=json` (or the corresponding prefixed path).
