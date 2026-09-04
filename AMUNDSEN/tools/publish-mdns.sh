#!/usr/bin/env bash
# Publish NAME (default underway.local) as an mDNS alias for this host's IPv4
# on IFACE (default wlo1), re-publishing whenever the address changes. Gives
# the science party a stable http://underway.local on the ship LAN without a
# DNS entry from ship IT (Apple, Windows and Linux resolve mDNS; Android does
# not, so also hand out the IP).
NAME=${1:-underway.local}; IFACE=${2:-wlo1}
cur=""; pid=""
trap '[[ $pid ]] && kill "$pid" 2>/dev/null; exit 0' TERM INT
while :; do
  ip=$(ip -4 -o addr show "$IFACE" 2>/dev/null | awk '{print $4}' | cut -d/ -f1 | head -1)
  if [[ $ip && $ip != "$cur" ]]; then
    [[ $pid ]] && kill "$pid" 2>/dev/null
    avahi-publish -a -R "$NAME" "$ip" & pid=$!; cur=$ip
    echo "$NAME -> $ip"
  fi
  sleep 30
done
