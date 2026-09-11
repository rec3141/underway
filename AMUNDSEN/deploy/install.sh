#!/usr/bin/env bash
# Install the dashboard's systemd units, filled in for this installation.
#
# The unit files in deploy/ are templates: every machine-specific value in
# them is a @NAME@ placeholder, and the values come from the installation's
# site file (/etc/underway/site.env, from deploy/site.env.example). This
# script renders each template, installs the ones whose content changed,
# reloads systemd and restarts the long-running servers whose unit changed;
# the timers' jobs pick a change up on their next run.
#
#   sudo deploy/install.sh            render, install, reload, restart what changed
#   sudo deploy/install.sh --enable   the same, then enable and start the core units
#   deploy/install.sh --check         show what would change, install nothing
#
# Run it again whenever site.env or anything under deploy/ changes; the deploy
# timer's journal says when a pull brought in unit changes.
set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
UNITDIR=/etc/systemd/system
CORE=(underway.timer underway-deploy.timer underway-dashboard.service)
mode=${1:-install}

[[ -r $SITE ]] || { echo "no $SITE: copy $HERE/site.env.example there and edit it" >&2; exit 1; }
set -a; . "$SITE"; set +a

: "${UNDERWAY_HOME:?set UNDERWAY_HOME in $SITE}" "${UNDERWAY_USER:?set UNDERWAY_USER in $SITE}" "${UNDERWAY_PYTHON:?set UNDERWAY_PYTHON in $SITE}"
getent passwd "$UNDERWAY_USER" >/dev/null || { echo "no user $UNDERWAY_USER" >&2; exit 1; }
user_home=$(getent passwd "$UNDERWAY_USER" | cut -d: -f6)
UNDERWAY_GROUP=$(id -gn "$UNDERWAY_USER")
UNDERWAY_CONFIG=${UNDERWAY_CONFIG:-$user_home/.config/underway}
UNDERWAY_MIRROR=${UNDERWAY_MIRROR:-/data/ship}
UNDERWAY_TILES_DIR=${UNDERWAY_TILES_DIR:-/data/gis/tiles}
ARCTIC_HISTORY_ROOT=${ARCTIC_HISTORY_ROOT:-/data/dev/arctic-history}
UNDERWAY_PORT=${UNDERWAY_PORT:-8042}
UNDERWAY_IFACE=${UNDERWAY_IFACE:-wlo1}
UNDERWAY_MDNS_NAME=${UNDERWAY_MDNS_NAME:-underway.local}
UNDERWAY_SITE=$SITE

APP=$UNDERWAY_HOME/app/AMUNDSEN
[[ -d $APP/dashboard ]] || { echo "no checkout at $APP: clone the repository there as $UNDERWAY_USER (git clone <url> $UNDERWAY_HOME/app)" >&2; exit 1; }
[[ -x $UNDERWAY_PYTHON ]] || { echo "UNDERWAY_PYTHON=$UNDERWAY_PYTHON is not an executable" >&2; exit 1; }

render() {
  local out
  out=$(sed -e "s|@UNDERWAY_HOME@|$UNDERWAY_HOME|g" \
            -e "s|@UNDERWAY_USER@|$UNDERWAY_USER|g" \
            -e "s|@UNDERWAY_GROUP@|$UNDERWAY_GROUP|g" \
            -e "s|@UNDERWAY_PYTHON@|$UNDERWAY_PYTHON|g" \
            -e "s|@UNDERWAY_CONFIG@|$UNDERWAY_CONFIG|g" \
            -e "s|@UNDERWAY_MIRROR@|$UNDERWAY_MIRROR|g" \
            -e "s|@ARCTIC_HISTORY_ROOT@|$ARCTIC_HISTORY_ROOT|g" \
            -e "s|@UNDERWAY_PORT@|$UNDERWAY_PORT|g" \
            -e "s|@UNDERWAY_IFACE@|$UNDERWAY_IFACE|g" \
            -e "s|@UNDERWAY_MDNS_NAME@|$UNDERWAY_MDNS_NAME|g" \
            -e "s|@UNDERWAY_SITE@|$UNDERWAY_SITE|g" "$1")
  if grep -q '@[A-Z_]*@' <<<"$out"; then
    echo "$1: a placeholder install.sh does not fill: $(grep -o '@[A-Z_]*@' <<<"$out" | sort -u | tr '\n' ' ')" >&2
    return 1
  fi
  printf '%s\n' "$out"
}

changed=()
for tpl in "$HERE"/*.service "$HERE"/*.timer; do
  name=$(basename "$tpl")
  new=$(render "$tpl")
  if [[ -f $UNITDIR/$name ]] && [[ $(cat "$UNITDIR/$name") == "$new" ]]; then continue; fi
  changed+=("$name")
  if [[ $mode == --check ]]; then
    echo "=== $name"
    diff <(cat "$UNITDIR/$name" 2>/dev/null) <(printf '%s\n' "$new") || true
  fi
done

if [[ $mode == --check ]]; then
  [[ ${#changed[@]} -eq 0 ]] && echo "the installed units are up to date"
  exit 0
fi
[[ $(id -u) == 0 ]] || { echo "run with sudo: sudo $0 $*" >&2; exit 1; }

# the state directories, owned by the service account
for d in db cache www chat camera360; do
  install -d -o "$UNDERWAY_USER" -g "$UNDERWAY_GROUP" "$UNDERWAY_HOME/$d"
done

for name in "${changed[@]}"; do
  render "$HERE/$name" > "$UNITDIR/$name.tmp"
  chmod 644 "$UNITDIR/$name.tmp"
  mv "$UNITDIR/$name.tmp" "$UNITDIR/$name"
  echo "installed $name"
done
systemctl daemon-reload

# a long-running server keeps its old unit until restarted; a oneshot job
# (a build, an alert run) takes the new one on its next start, and is never
# cut short here
for name in "${changed[@]}"; do
  [[ $name == *.service ]] || continue
  [[ $(systemctl show -p Type --value "$name") == oneshot ]] && continue
  systemctl try-restart "$name" && systemctl is-active -q "$name" && echo "restarted $name"
done

if [[ $mode == --enable ]]; then
  systemctl enable --now "${CORE[@]}"
  echo "enabled ${CORE[*]}; the others are enabled one by one once their credentials are in place (README, Taking over)"
fi
[[ ${#changed[@]} -eq 0 ]] && echo "the installed units were already up to date"
exit 0
