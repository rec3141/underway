#!/usr/bin/env bash
# Start the underway dashboard (Linux or macOS). Double-click or run: ./start.sh
# The first time it loads the image from this folder and makes the settings password.
# Run it from a copy on the computer's own disk, never from the drive it came on.
set -euo pipefail
cd "$(dirname "$0")"
say() { printf '\n== %s\n' "$*"; }

# The dashboard writes its database and site into data/ all the time: it must run
# from the computer's own disk, not from the drive or share the kit came on.
fs=$(stat -f -c %T . 2>/dev/null || echo unknown)
case "$PWD:$fs" in
  /media/*|/run/media/*|/Volumes/*|*:smb*|*:cifs|*:exfat|*:vfat|*:msdos|*:fuseblk|*:nfs*)
    say "This folder is on a USB drive or a network share ($PWD)."
    echo "   Copy the whole folder onto this computer first (for example into your home"
    echo "   folder), then run "bash start.sh" in the copy. See HOW-TO.txt, step 2."
    exit 1 ;;
esac

command -v docker >/dev/null || { say "Docker is not installed. See HOW-TO.txt, step 1."; exit 1; }
docker info >/dev/null 2>&1 || { say "Docker is installed but not running (or needs sudo). See HOW-TO.txt."; exit 1; }

. ./.env
if ! docker image inspect "underway:$UNDERWAY_VERSION" >/dev/null 2>&1; then
  say "Loading the dashboard image (a few minutes, once)…"
  docker load -i "underway-$UNDERWAY_VERSION.tar.gz"
fi

mkdir -p data/config
docker compose up -d

if [[ ! -s data/config/admin-password ]]; then
  say "Making the password for the settings page…"
  for _ in $(seq 30); do docker exec underway true 2>/dev/null && break; sleep 2; done
  pw=$(docker exec underway python3 -m dashboard set-admin-password --random | sed -n 's/^The \/settings password is now: //p')
  printf 'Settings page password: %s\n(change it on the settings page; delete this file afterwards)\n' "$pw" > ADMIN-PASSWORD.txt
  say "Settings page password: $pw   (also saved in ADMIN-PASSWORD.txt)"
fi

say "The dashboard is starting. Open one of these in a browser:"
for ip in $(hostname -I 2>/dev/null || ipconfig getifaddr en0 2>/dev/null); do
  [[ $ip == *:* || $ip == 172.* ]] || echo "   http://$ip/            settings: http://$ip/settings"
done
echo "   (on this computer: http://localhost/)"
