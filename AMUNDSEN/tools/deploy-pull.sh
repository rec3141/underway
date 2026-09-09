#!/usr/bin/env bash
# Keep the deploy checkout on origin/master, and restart the long-running
# services when the Python they run changed. Run by underway-deploy.timer
# every few minutes, as root (the restarts need it); git runs as the
# checkout's owner so the tree never gains root-owned files.
#
#   deploy-pull.sh            fetch, fast-forward, restart what needs it
#
# What the pull brings in takes effect by itself except for two things: a
# change under dashboard/*.py needs the serving processes restarted (the
# build subprocess picks code up on its own), and a change under deploy/
# needs the unit files installed by hand — this script only says so.
set -euo pipefail
APP=${UNDERWAY_APP:-/data/underway/app}
OWNER=$(stat -c %U "$APP")
as_owner() { if [[ $(id -un) == "$OWNER" ]]; then "$@"; else runuser -u "$OWNER" -- "$@"; fi; }
cd "$APP"

# no link to GitHub: nothing to do until the next run
as_owner git fetch -q origin master 2>/dev/null || exit 0
old=$(git rev-parse HEAD); new=$(git rev-parse origin/master)
[[ $old == "$new" ]] && exit 0
as_owner git merge -q --ff-only origin/master
changed=$(git diff --name-only "$old" "$new")
echo "deploy: ${old:0:7} -> ${new:0:7}"
echo "$changed" | sed 's/^/  /'
if grep -q '^AMUNDSEN/dashboard/.*\.py$' <<<"$changed"; then
  echo "deploy: restarting the serving processes"
  systemctl restart underway-dashboard underway-telegram
fi
if grep -q '^AMUNDSEN/deploy/' <<<"$changed"; then
  echo "deploy: unit files changed; install them: sudo cp $APP/AMUNDSEN/deploy/*.service $APP/AMUNDSEN/deploy/*.timer /etc/systemd/system/ && sudo systemctl daemon-reload"
fi
