#!/usr/bin/env bash
# Pack a frozen release of the dashboard onto a drive for another machine.
#
#   deploy/container/make-release.sh VERSION OUTDIR [--with-data]
#
# Builds the image from the current checkout (which must be clean and at the
# tag VERSION) and the game checkout (AMUNDSEN_GAME, default /data/dev/amundsen-game), saves it as OUTDIR/underway-VERSION.tar.gz, and copies the
# start/stop scripts, compose file and HOW-TO beside it. With --with-data it
# also seeds OUTDIR/data from this installation so the next machine starts with
# the map tiles, the Wiki snapshot, the game's boards and the ingest stores instead of rebuilding
# them: tiles, arctic-history (no .git), db, cache and report. Secrets are never
# copied; they go in again on the /settings page.
#
# OUTDIR may be an SMB share or an exFAT drive: files are copied without owner
# or permission bits, and symbolic links are replaced by the files they point to.
# The kit on it is only carried: the start scripts refuse to run from a drive or
# share, and the HOW-TO has the keeper copy the folder onto the computer first.
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
  cp_tree() { [[ -d $1 ]] && { echo "copying $1"; rsync -rtL --info=progress2 "${@:3}" "$1/" "$out/data/$2/"; } || echo "skipping $1: not here"; }
  cp_tree "${UNDERWAY_TILES_DIR:-/data/gis/tiles}" tiles
  cp_tree "${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}" arctic-history --exclude=.git
  cp_tree "$home/db" db --exclude=codex_bot*
  cp_tree "$home/cache" cache
  cp_tree "$home/report" report
  cp_tree "$game/runtime" game --exclude=crew-routing --exclude=caddy-before-game.json
  # the stores are written while this runs (the build every minute): a file copy of a
  # SQLite database caught mid-write is corrupt, so each one is copied again through
  # SQLite's online backup, into local scratch first (SQLite locking is unreliable on SMB)
  echo "taking consistent copies of the databases…"
  "${UNDERWAY_PYTHON:-python3}" - "$out/data" "$home/db" "${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}" "$home/report" "$game/runtime" <<'PY'
import shutil, sqlite3, sys, tempfile
from pathlib import Path
out = Path(sys.argv[1])
for src_root, dst_root in zip(map(Path, sys.argv[2:]), ("db", "arctic-history", "report", "game")):
    for src in [*src_root.rglob("*.db"), *src_root.rglob("*.sqlite")]:
        dst = out / dst_root / src.relative_to(src_root)
        if not dst.exists() or ".git" in src.parts:
            continue
        with tempfile.TemporaryDirectory() as tmp:
            snap = Path(tmp) / src.name
            with sqlite3.connect(f"file:{src}?mode=ro", uri=True) as a, sqlite3.connect(snap) as b:
                a.backup(b)
            shutil.copyfile(snap, dst)
        for side in ("-wal", "-shm", "-journal"):
            dst.with_name(dst.name + side).unlink(missing_ok=True)
        print("  ", dst.relative_to(out))
PY
fi
echo "release $version is in $out"
