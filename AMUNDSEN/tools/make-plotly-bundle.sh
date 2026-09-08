#!/usr/bin/env bash
# Build dashboard/static/plotly.min.js: plotly.js's core with only the trace
# types the dashboard draws (scatter, bar, heatmap, scattermap). The full
# distribution is 4.8 MB (1.5 MB compressed); this bundle is 2.0 MB (0.6 MB).
#
# Needs node and npm. On the ship the registry is only reachable through the
# SOCKS tunnel (~/bin/grid-tunnel up); npm 10 takes a socks5 proxy URL, so
#   NPM_PROXY=socks5://127.0.0.1:1080 tools/make-plotly-bundle.sh
# Adding a trace type: put its lib module in ENTRY below and rebuild; the
# asset URL carries the file's size, so browsers pick the new file up at the
# next build.
set -euo pipefail

PLOTLY_VERSION=3.4.0
ESBUILD_VERSION=0.28.2
PROJECT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="${PLOTLY_BUNDLE_DIR:-$(mktemp -d)}"
OUT="$PROJECT/dashboard/static/plotly.min.js"

cd "$WORK"
[[ -f package.json ]] || printf '{"name":"underway-plotly","private":true,"version":"0.0.0"}\n' > package.json
proxy=()
[[ -n "${NPM_PROXY:-}" ]] && proxy=(--proxy "$NPM_PROXY" --https-proxy "$NPM_PROXY")
npm install --no-audit --no-fund --loglevel=warn "${proxy[@]}" "plotly.js@$PLOTLY_VERSION" "esbuild@$ESBUILD_VERSION"

cat > entry.js <<'JS'
// The dashboard's Plotly: the core with only the trace types the page draws.
const Plotly = require("plotly.js/lib/core");
Plotly.register([
  require("plotly.js/lib/scatter"),      // the panels, casts, history
  require("plotly.js/lib/bar"),          // the schedule timeline
  require("plotly.js/lib/heatmap"),      // the cast section
  require("plotly.js/lib/scattermap"),   // the map (MapLibre)
]);
module.exports = Plotly;
JS

./node_modules/.bin/esbuild entry.js --bundle --minify --format=iife --global-name=Plotly --legal-comments=none --define:global=globalThis \
    --banner:js="/*! plotly.js v$PLOTLY_VERSION — core with scatter, bar, heatmap and scattermap only; built by tools/make-plotly-bundle.sh. Copyright 2012-2026, Plotly, Inc. MIT license. */" \
    --outfile=plotly.min.js --log-level=warning

cp plotly.min.js "$OUT"
ls -la "$OUT"
printf 'gzip: %d kB\n' $(( $(gzip -6 -c "$OUT" | wc -c) / 1024 ))
echo "built in $WORK"
