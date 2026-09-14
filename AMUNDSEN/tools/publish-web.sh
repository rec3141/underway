#!/usr/bin/env bash
# Publish the dashboard to the public web, in two hops. The ship's firewall
# lets it reach grid only, so the ship pushes its web root to grid; grid puts
# the history layer in from its own database (the ship's copy of the history
# is only what it pulled from grid, and the plates are already here) and
# copies the light parts to the web server: the page, its data, the wiki with its thumbnails and the
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
#   UNDERWAY_PUBLISH_REMOTE   ship side: where the web root goes  (grid:/data/underway_server/www)
#   UNDERWAY_PUBLISH_APP      ship side: this checkout on grid     (/data/dev/underway/AMUNDSEN)
#   UNDERWAY_PUBLISH_MIRROR   grid side: the mirror of the web root (/data/underway_server/www)
#   UNDERWAY_PUBLISH_TARGET   grid side: the web server, host:path  (dreamhost:cryomics.org/underway)
#   UNDERWAY_PUBLISH_URL      where that is served                  (https://cryomics.org/underway/)
#   UNDERWAY_PUBLISH_MAX_MB   a size cap on the history's files, if one is wanted (0: none)
#   ARCTIC_HISTORY_ROOT       grid side: the history clone           (/data/dev/arctic-history)
#   UNDERWAY_PYTHON           grid side: a Python >= 3.11 with PIL   (the history clone's .route-venv)
set -euo pipefail
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
if [[ -r $SITE ]]; then set -a; . "$SITE"; set +a; fi
WEBROOT=${UNDERWAY_WEBROOT:-${UNDERWAY_HOME:-/data/underway_server}/www}
REMOTE=${UNDERWAY_PUBLISH_REMOTE:-grid:/data/underway_server/www}
REMOTE_APP=${UNDERWAY_PUBLISH_APP:-/data/dev/underway/AMUNDSEN}
MIRROR=${UNDERWAY_PUBLISH_MIRROR:-/data/underway_server/www}
TARGET=${UNDERWAY_PUBLISH_TARGET:-dreamhost:cryomics.org/underway}
URL=${UNDERWAY_PUBLISH_URL:-https://cryomics.org/underway/}
MAX_MB=${UNDERWAY_PUBLISH_MAX_MB:-0}
GRID_HOME=${UNDERWAY_PUBLISH_STATE_DIR:-${MIRROR%/*}}
SOURCE=${UNDERWAY_PUBLISH_SOURCE_DIR:-$GRID_HOME/source}
TRACK_PY=${UNDERWAY_PUBLISH_TRACK_PYTHON:-$GRID_HOME/.venv/bin/python}
HIST=${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}
PY=${UNDERWAY_PYTHON:-$HIST/.route-venv/bin/python}
RSYNC="rsync -az --partial --timeout=120 --stats"
DRY_ARGS=()
if [[ ${2:-} == --dry-run && ${1:-} == push ]]; then
  DRY_ARGS=(--dry-run)
  RSYNC+=" --dry-run"
elif [[ $# -gt 1 ]]; then
  echo 'Only push accepts --dry-run' >&2; exit 2
fi
stats() { grep -E "Number of (regular files transferred|created|deleted)|Total transferred|Total file size|Total bytes (sent|received)" || true; }

rebuild_tracks() {
  [[ -f $MIRROR/data/manifest.json && -f $MIRROR/index.html ]] || { echo 'No incoming ship manifest/index' >&2; return 1; }
  [[ -d $SOURCE/Data/FULL_CSV && -d $SOURCE/Share ]] || { echo "No source observations at $SOURCE" >&2; return 1; }
  [[ -x $TRACK_PY ]] || { echo "Set up grid's rebuild Python at $TRACK_PY (README)" >&2; return 1; }
  mkdir -p "$GRID_HOME/db" "$GRID_HOME/cache"
  rsync -a "$SOURCE/runtime/" "$GRID_HOME/db/"
  local stage
  stage=$(mktemp -d "$GRID_HOME/.tracks-stage.XXXXXX")
  mkdir -p "$stage/data"
  cp "$MIRROR/data/manifest.json" "$stage/data/manifest.json"
  cp "$MIRROR/index.html" "$stage/index.html"
  # Immutable payloads and fingerprint indexes make unchanged history reusable
  # in the isolated build. Hard links avoid copying the whole track archive.
  if [[ -d $MIRROR/data/track ]]; then
    mkdir -p "$stage/data/track"
    rsync -a --link-dest="$MIRROR/data/track" "$MIRROR/data/track/" "$stage/data/track/"
  fi
  echo "== Rebuilding track windows on grid from $SOURCE (stage $stage)"
  if ! UNDERWAY_DATA_ROOT="$SOURCE/Data" UNDERWAY_SHARE_ROOT="$SOURCE/Share" \
    UNDERWAY_DB_DIR="$GRID_HOME/db" UNDERWAY_CACHE_DIR="$GRID_HOME/cache" \
    PYTHONPATH="$HERE" OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
    "$TRACK_PY" -m dashboard build --tracks-only --root "$stage"; then
    echo "Track rebuild failed; web deployment skipped. Stage retained at $stage" >&2
    return 1
  fi
  # Commit complete generated files before the manifest that references them.
  rsync -a --exclude manifest.json "$stage/data/" "$MIRROR/data/"
  mv "$stage/data/manifest.json" "$MIRROR/data/manifest.json"
  PYTHONPATH="$HERE" "$TRACK_PY" - "$MIRROR" <<'PYEOF'
import json, sys
from pathlib import Path
from dashboard.track import prune_track
root = Path(sys.argv[1])
prune_track(root, json.loads((root / 'data/manifest.json').read_text())['track'])
PYEOF
  rm -rf -- "$stage"
}

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
<IfModule mod_deflate.c>
  AddOutputFilterByType DEFLATE application/json application/geo+json
</IfModule>
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
  mkdir -p "$MIRROR/data/track"
  cat > "$MIRROR/data/track/.htaccess" <<'HT'
<IfModule mod_headers.c>
  <FilesMatch "^[0-9a-f]{24}\.json$">
    Header set Cache-Control "public, max-age=31536000, immutable"
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
  PYTHONPATH="$HERE" "$TRACK_PY" - "$m" "$MIRROR/index.html" "$HERE/dashboard/templates" <<'PYEOF'
import hashlib, json, sys, re
from pathlib import Path
from jinja2 import Environment, FileSystemLoader
p, index = Path(sys.argv[1]), Path(sys.argv[2])
m = json.loads(p.read_text())
m['cameras'] = []
m['public'] = True
p.with_suffix('.json.tmp').write_text(json.dumps(m, indent=1))
p.with_suffix('.json.tmp').replace(p)
if index.is_file():
    site = json.loads(re.search(r'window\.__SITE__ = (.*);', index.read_text()).group(1))
    site['default_window'] = m['default_window']
    assets = sorted(f for f in (index.parent / 'static').glob('*') if f.suffix in ('.js', '.css'))
    if assets:
        digest = hashlib.sha1()
        for asset in assets:
            digest.update(asset.name.encode())
            digest.update(asset.read_bytes())
        site['asset_version'] = digest.hexdigest()[:10]
    html = Environment(loader=FileSystemLoader(sys.argv[3]), autoescape=True).get_template('index.html.j2').render(site=site, m=m)
    index.with_suffix('.html.tmp').write_text(html)
    index.with_suffix('.html.tmp').replace(index)
PYEOF
}

case "${1:-}" in
  push)
    [[ -f $WEBROOT/index.html ]] || { echo "no web root at $WEBROOT" >&2; exit 1; }
    "$HERE/tools/publish-sources.sh" "${DRY_ARGS[@]}"
    echo "== $WEBROOT -> $REMOTE (without the history layer, which grid publishes itself)"
    # what grid makes for itself stays: the history layer is not sent, and
    # nothing under data/ that the ship does not send is deleted there, so
    # the track files generated on grid survive every push
    $RSYNC --delete --exclude 'data/history/' --exclude 'data/w-*.json' --exclude 'data/track/' --exclude '.htaccess' --exclude 'api-off.json' --exclude '.published' \
      --filter='P data/**' --exclude '*.tmp' --exclude '*.part' "$WEBROOT/" "$REMOTE/" | stats
    if [[ ${#DRY_ARGS[@]} -gt 0 ]]; then echo "Dry run complete; grid rebuild/deploy skipped"; exit 0; fi
    echo "== grid pulls master, rebuilds and deploys"
    # Keep grid's renderer current on the same cadence as its data. The lock
    # prevents replacing code during a running grid build/deployment.
    printf -v command 'flock -w 120 %q git -C %q pull --ff-only origin master && UNDERWAY_PUBLISH_MIRROR=%q %q rebuild-deploy' \
      "${REMOTE#*:}/../.publish.lock" "${REMOTE_APP%/*}" "${REMOTE#*:}" "$REMOTE_APP/tools/publish-web.sh"
    ssh "${REMOTE%%:*}" "$command"
    ;;
  history) history_layer ;;
  static) static_assets ;;
  rebuild) rebuild_tracks ;;
  deploy|rebuild-deploy)
    mkdir -p "$GRID_HOME"
    exec 7>"$GRID_HOME/.publish.lock"
    flock 7
    if [[ $1 == rebuild-deploy ]]; then rebuild_tracks; fi
    echo "== the history layer, from $HIST"
    history_layer
    echo "== the page's assets"
    static_assets
    web_files
    public_manifest
    echo "== $MIRROR -> $TARGET (no cameras, journal photographs or tiles)"
    # two passes: the page and its data, then the history's files (whole unless
    # a cap is set: an index of ten thousand artifacts is bigger than a picture)
    $RSYNC --delete \
      --exclude 'camera/' --exclude 'journal/' --exclude 'static/tiles/' --exclude 'data/history/files/' \
      --exclude '*.tmp' --exclude '*.part' "$MIRROR/" "$TARGET/" | stats
    if [[ ${MAX_MB} != 0 ]]; then
      echo "== the history's files, up to $MAX_MB MB each"
      $RSYNC --delete --max-size="${MAX_MB}m" "$MIRROR/data/history/files/" "$TARGET/data/history/files/" | stats
    else
      echo "== the history's files, all of them"
      $RSYNC --delete "$MIRROR/data/history/files/" "$TARGET/data/history/files/" | stats
    fi
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
  *) echo "usage: publish-web.sh {push|rebuild|rebuild-deploy|deploy|history|static|status}" >&2; exit 1 ;;
esac
