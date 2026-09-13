#!/usr/bin/env bash
# Publish the dashboard to the public web, in two hops. The ship's firewall
# lets it reach grid only, so the ship pushes its web root to grid; grid puts
# the history layer in from its own database (fresher than the ship's copy,
# and the 22 GB of plates are already there) and copies the light parts to
# the web server: the page, its data, the wiki with its thumbnails and the
# pictures under a size cap. What stays behind: the shipboard cameras
# (/camera/), the nature journal's photographs (/journal/), the raster tiles,
# and every picture over the cap. The page's own services (chat, uploads, the
# live feed, alerts) do not run on the web server: every api/ call answers
# 503 with a JSON body, which is what the page already handles when the
# ship's share is down.
#
#   tools/publish-web.sh push      on the ship: www -> grid, then grid deploys
#   tools/publish-web.sh deploy    on grid: the history layer, the assets, then mirror -> web server
#   tools/publish-web.sh history   on grid: only the history layer, from grid's database
#   tools/publish-web.sh static    on grid: only the page's assets, from this checkout
#   tools/publish-web.sh status    what is where, and when
#
# Settings, from /etc/underway/site.env or the environment:
#   UNDERWAY_PUBLISH_REMOTE   ship side: where the web root goes  (grid:/data/underway/www)
#   UNDERWAY_PUBLISH_APP      ship side: this checkout on grid     (/data/dev/underway/AMUNDSEN)
#   UNDERWAY_PUBLISH_MIRROR   grid side: the mirror of the web root (/data/underway/www)
#   UNDERWAY_PUBLISH_TARGET   grid side: the web server, host:path  (dreamhost:cryomics.org/underway)
#   UNDERWAY_PUBLISH_URL      where that is served                  (https://cryomics.org/underway/)
#   UNDERWAY_PUBLISH_MAX_MB   the size cap on a picture              (5)
#   ARCTIC_HISTORY_ROOT       grid side: the history clone           (/data/dev/arctic-history)
#   UNDERWAY_PYTHON           grid side: a Python >= 3.11 with PIL   (the history clone's .route-venv)
set -euo pipefail
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
if [[ -r $SITE ]]; then set -a; . "$SITE"; set +a; fi
WEBROOT=${UNDERWAY_WEBROOT:-${UNDERWAY_HOME:-/data/underway_server}/www}
REMOTE=${UNDERWAY_PUBLISH_REMOTE:-grid:/data/underway/www}
REMOTE_APP=${UNDERWAY_PUBLISH_APP:-/data/dev/underway/AMUNDSEN}
MIRROR=${UNDERWAY_PUBLISH_MIRROR:-/data/underway/www}
TARGET=${UNDERWAY_PUBLISH_TARGET:-dreamhost:cryomics.org/underway}
URL=${UNDERWAY_PUBLISH_URL:-https://cryomics.org/underway/}
MAX_MB=${UNDERWAY_PUBLISH_MAX_MB:-5}
HIST=${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}
PY=${UNDERWAY_PYTHON:-$HIST/.route-venv/bin/python}
RSYNC="rsync -az --partial --timeout=120 --info=stats1"
stats() { grep -E "Number of (regular files transferred|created|deleted)|Total transferred|Total file size" || true; }

history_layer() {
  # the History tab's wiki, map layer, bibliography, thumbnails and files,
  # rendered from grid's own database into the mirror; the ship's copy of
  # data/history/ is not pushed, so this one stands
  [[ -f $HIST/db/history/history.sqlite ]] || { echo "no history database at $HIST" >&2; return 1; }
  mkdir -p "$MIRROR"
  PYTHONPATH="$HERE:$HIST" "$PY" - "$MIRROR" <<'PYEOF'
import sys, json
from pathlib import Path
from dashboard import history
root = Path(sys.argv[1])
entry = history.publish(root)
if not entry:
    sys.exit("the history layer did not publish")
print("history layer:", json.dumps({k: entry[k] for k in ("pages", "artifacts", "topics") if k in entry}))
PYEOF
}

static_assets() {
  # the page's own assets, as build.py copies them: nothing here depends on
  # the ship's data, so the web server has them before the first push
  mkdir -p "$MIRROR/static"
  rsync -a --exclude 'tiles/' "$HERE/dashboard/static/" "$MIRROR/static/"
}

web_files() {
  # the web server's own two files: the page is relative to its directory
  # (the ship serves it under /underway/ too), so only these are host-specific
  local base; base=/${TARGET#*:}; base=${base#/}; base=/${base#*/}; [[ $base == "/${TARGET#*:}" ]] && base=/
  cat > "$MIRROR/.htaccess" <<HT
DirectoryIndex index.html
Options -Indexes
AddType application/json .json
AddType application/geo+json .geojson
<IfModule mod_rewrite.c>
  RewriteEngine On
  RewriteRule ^api(/|$) - [R=503,L]
</IfModule>
ErrorDocument 503 ${base%/}/api-off.json
<IfModule mod_headers.c>
  <FilesMatch "\.(json|html)$">
    Header set Cache-Control "no-store"
  </FilesMatch>
  <FilesMatch "\.(js|css|geojson|pbf|png|jpg|jpeg|webp|gif|pdf|mp4)$">
    Header set Cache-Control "public, max-age=3600"
  </FilesMatch>
</IfModule>
HT
  printf '{"error": "this copy of the dashboard is published to the web from the ship; the services that answer here (the chat, the live feed, uploads, alerts) run only aboard", "off": true}\n' > "$MIRROR/api-off.json"
}

public_manifest() {
  # the cameras stay aboard: the manifest the web server gets lists none, so
  # the tab says so instead of asking for videos that are not there
  local m=$MIRROR/data/manifest.json
  [[ -f $m ]] || return 0
  "$PY" - "$m" <<'PYEOF'
import json, sys
p = sys.argv[1]
m = json.load(open(p))
if m.get("cameras"):
    m["cameras"] = []
    m["public"] = True
    json.dump(m, open(p, "w"), indent=1)
PYEOF
}

case "${1:-}" in
  push)
    [[ -f $WEBROOT/index.html ]] || { echo "no web root at $WEBROOT" >&2; exit 1; }
    echo "== $WEBROOT -> $REMOTE (without the history layer, which grid publishes itself)"
    $RSYNC --delete --exclude 'data/history/' --exclude '.htaccess' --exclude 'api-off.json' \
      --exclude '*.tmp' --exclude '*.part' "$WEBROOT/" "$REMOTE/" | stats
    echo "== grid deploys"
    ssh "${REMOTE%%:*}" "UNDERWAY_PUBLISH_MIRROR=${REMOTE#*:} $REMOTE_APP/tools/publish-web.sh deploy"
    ;;
  history) history_layer ;;
  static) static_assets ;;
  deploy)
    echo "== the history layer, from $HIST"
    history_layer
    echo "== the page's assets"
    static_assets
    web_files
    public_manifest
    echo "== $MIRROR -> $TARGET (pictures up to $MAX_MB MB; no cameras, journal photographs or tiles)"
    $RSYNC --delete --max-size="${MAX_MB}m" \
      --exclude 'camera/' --exclude 'journal/' --exclude 'static/tiles/' \
      --exclude '*.tmp' --exclude '*.part' "$MIRROR/" "$TARGET/" | stats
    date -u +%Y-%m-%dT%H:%M:%SZ > "$MIRROR/.published"
    echo "published: $URL"
    ;;
  status)
    if [[ -f $WEBROOT/index.html ]]; then
      echo "ship web root $WEBROOT: page built $(date -r "$WEBROOT/index.html" +%Y-%m-%dT%H:%M)"
    fi
    if [[ -d $MIRROR ]]; then
      echo "mirror $MIRROR: $(du -sh "$MIRROR" 2>/dev/null | cut -f1)" \
           "$( [[ -f $MIRROR/index.html ]] && echo "page pushed $(date -r "$MIRROR/index.html" +%Y-%m-%dT%H:%M)" || echo "no page pushed yet")" \
           "$( [[ -f $MIRROR/.published ]] && echo "; last deploy $(cat "$MIRROR/.published")")"
    fi
    ssh "${TARGET%%:*}" "du -sh ${TARGET#*:} 2>/dev/null; ls -la ${TARGET#*:}/index.html ${TARGET#*:}/data/manifest.json 2>/dev/null" || true
    echo "served at $URL"
    ;;
  *) echo "usage: publish-web.sh {push|deploy|history|static|status}" >&2; exit 1 ;;
esac
