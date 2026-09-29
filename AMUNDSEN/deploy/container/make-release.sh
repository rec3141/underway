#!/usr/bin/env bash
# Pack a frozen release of the dashboard onto a drive for another machine.
#
#   deploy/container/make-release.sh VERSION OUTDIR [--with-data]
#
# Builds the image from the current checkout (which must be clean and at the
# tag VERSION) and the game checkout (AMUNDSEN_GAME, default /data/dev/amundsen-game), saves it as OUTDIR/underway-VERSION.tar.gz, and copies the
# start/stop scripts, compose file and HOW-TO beside it. With --with-data it
# also packs a seed of this installation into OUTDIR/seed/, one tar per part
# (tiles, arctic-history, db, cache, report, game, ice), so the next machine starts
# where this one is instead of rebuilding for hours; the start scripts unpack
# it into data/ on the first start. Secrets are never packed; they go in again
# on the /settings page.
#
# The seed is a few large archives rather than the half-million small files of
# the tile pyramids: an SMB share or a USB drive pays for every file, both when
# this writes the release and when the keeper copies it onto the computer.
# Each part is staged under STAGE (default /data/underway-release-stage, on the
# same filesystem as the sources, so staging is hard links and costs no space);
# SQLite databases are replaced there by consistent snapshots, since the build
# writes them every minute, and symbolic links are packed as the files they
# point to.
set -euo pipefail
version=${1:?usage: make-release.sh VERSION OUTDIR [--with-data]}
out=${2:?usage: make-release.sh VERSION OUTDIR [--with-data]}
here=$(cd "$(dirname "$0")" && pwd)
app=$(cd "$here/../.." && pwd)

[[ -f /etc/underway/site.env ]] && { set -a; . /etc/underway/site.env; set +a; }
home=${UNDERWAY_HOME:-/data/underway_server}

cd "$app"
[[ -z $(git status --porcelain) ]] || { echo "the checkout has uncommitted changes" >&2; exit 1; }
[[ $(git rev-parse HEAD) == $(git rev-parse "refs/tags/$version^{commit}" 2>/dev/null) ]] \
  || { echo "HEAD is not the tag $version: git tag $version && git push origin $version" >&2; exit 1; }

mkdir -p "$out"
game=${AMUNDSEN_GAME:-/data/dev/amundsen-game}
[[ -f $game/server.py ]] || { echo "no game checkout at $game (set AMUNDSEN_GAME)" >&2; exit 1; }
docker build --build-context game="$game" -t "underway:$version" "$app"
echo "saving the image…"
docker save "underway:$version" | gzip -1 > "$out/underway-$version.tar.gz.tmp"
mv "$out/underway-$version.tar.gz.tmp" "$out/underway-$version.tar.gz"
# cp, not cp -p: the destination may not keep modes; the start scripts are run with bash

cp "$here"/{compose.yaml,start.sh,stop.sh,status.sh,start.bat,stop.bat,HOW-TO.txt} "$out/"
printf 'UNDERWAY_VERSION=%s\n' "$version" > "$out/.env"
mkdir -p "$out/data/config"

if [[ ${3:-} == --with-data ]]; then
  stage=${STAGE:-/data/underway-release-stage}
  rm -rf "$stage"; mkdir -p "$stage" "$out/seed"
  pack() {    # pack NAME SRC [rsync excludes...]
    local name=$1 src=$2; shift 2
    [[ -d $src ]] || { echo "skipping $name: no $src"; return; }
    echo "staging $name from $src"
    rsync -a --link-dest="$src/" "$@" "$src/" "$stage/$name/"
    # a file copy of a SQLite database caught mid-write is corrupt: snapshot each one
    "${UNDERWAY_PYTHON:-python3}" - "$src" "$stage/$name" <<'PY'
import sqlite3, sys
from pathlib import Path
src, dst = map(Path, sys.argv[1:])
for d in [*dst.rglob("*.db"), *dst.rglob("*.sqlite")]:
    s = src / d.relative_to(dst)
    d.unlink()
    with sqlite3.connect(f"file:{s}?mode=ro", uri=True) as a, sqlite3.connect(d) as b:
        a.backup(b)
    for side in ("-wal", "-shm", "-journal"):
        d.with_name(d.name + side).unlink(missing_ok=True)
PY
    echo "packing $name"
    tar -chf - -C "$stage/$name" --owner=0 --group=0 --numeric-owner . > "$out/seed/$name.tar.tmp"
    mv "$out/seed/$name.tar.tmp" "$out/seed/$name.tar"
    rm -rf "$stage/$name"
  }
  pack tiles "${UNDERWAY_TILES_DIR:-/data/gis/tiles}" --exclude='*.old' --exclude='*.new'
  # the Wiki needs the arctic_history package and db/; www.canada.ca is a scrape
  # whose file names Windows cannot hold
  pack arctic-history "${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}" --exclude=.git --exclude=www.canada.ca --exclude=tests
  pack db "$home/db" --exclude='codex_bot*'
  pack cache "$home/cache"
  pack report "$home/report"
  pack game "$game/runtime" --exclude=crew-routing --exclude=caddy-before-game.json
  # the ice camera's results (ice.sqlite and the batches' JSON), not its pictures (33 GB)
  pack ice "${UNDERWAY_ICE_ROOT:-$home/ice}" --exclude=/images --exclude='*.jpg' --exclude=worker.lock
  rmdir "$stage" 2>/dev/null || true
  ( cd "$out/seed" && ls -l *.tar | awk '{printf "%8.1f GB  %s\n", $5/1e9, $9}' )
fi
echo "release $version is in $out"
