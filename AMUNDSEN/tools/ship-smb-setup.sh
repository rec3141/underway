#!/bin/bash
# One-time setup for the shipboard Samba shares on this box: //10.0.0.10/Data
# and //10.0.0.10/Share automounted at /mnt/ship/Data and /mnt/ship/Share,
# owned by the account that runs the dashboard.
# Run with sudo. It will PROMPT you for the share password and write it to a
# root-only credentials file — the password is never passed on the command line
# (where it would show in `ps`) and never stored in this script.
set -e
[ "$(id -u)" = 0 ] || { echo "run with sudo: sudo $0"; exit 1; }

SERVER=10.0.0.10
SHARES=(Data Share)
CREDS=/etc/ship-smb.creds
SITE=${UNDERWAY_SITE:-/etc/underway/site.env}
OWNER=$( [ -r "$SITE" ] && . "$SITE"; echo "${UNDERWAY_USER:-${SUDO_USER:-}}" )
[ -n "$OWNER" ] || { echo "set UNDERWAY_USER in $SITE, or run through sudo"; exit 1; }
UID_=$(id -u "$OWNER"); GID_=$(id -g "$OWNER")

if [ -f "$CREDS" ]; then
  echo "credentials file already exists at $CREDS (leaving it alone)"
else
  read -rp  "SMB username [science]: " U; U=${U:-science}
  read -rsp "SMB password: " P; echo
  umask 077
  printf 'username=%s\npassword=%s\n' "$U" "$P" > "$CREDS"
  chmod 600 "$CREDS"; chown root:root "$CREDS"
  unset P
  echo "wrote $CREDS (root-only, 0600)"
fi

for s in "${SHARES[@]}"; do
  mkdir -p "/mnt/ship/$s"
  line="//$SERVER/$s /mnt/ship/$s cifs credentials=$CREDS,uid=$UID_,gid=$GID_,file_mode=0664,dir_mode=0775,iocharset=utf8,vers=3.0,_netdev,nofail,noauto,x-systemd.automount 0 0"
  if grep -q "^//$SERVER/$s " /etc/fstab; then
    echo "fstab entry for $s already present"
  else
    cp /etc/fstab /etc/fstab.bak.$(date +%Y%m%d-%H%M%S)
    echo "$line" >> /etc/fstab
    echo "added fstab entry for $s"
  fi
done

systemctl daemon-reload

# daemon-reload generates the .automount units but does not activate them, so
# without this the mount points stay empty until the next boot.
for s in "${SHARES[@]}"; do
  unit="$(systemd-escape -p --suffix=automount "/mnt/ship/$s")"
  systemctl start "$unit" && echo "started $unit"
done
echo
echo "Done. The shares automount on first access:  ls /mnt/ship/Data"
echo "If that hangs or errors, the VPN is probably hijacking 10.0.0.x —"
echo "run:  sudo $(dirname "$0")/ship-routes.sh on"
