#!/usr/bin/env bash
# Publish NAME (default underway.local) as an mDNS alias for this host's IPv4
# on each IFACE given (default wlo1), re-publishing whenever an address changes.
#
# avahi-publish cannot scope a record to one interface: every address given is
# announced on every interface. A client picks any of them, so only publish
# addresses every client can reach — on the ship that is the Wi-Fi address;
# wired clients use http://10.0.0.58/ directly. Gives
# the science party a stable http://underway.local on the ship LAN without a
# DNS entry from ship IT (Apple, Windows and Linux resolve mDNS; Android does
# not, so also hand out the IP).
NAME=${1:-underway.local}; shift; IFACES=("${@:-wlo1}")
cur=""; pids=()
trap 'kill "${pids[@]}" 2>/dev/null; exit 0' TERM INT
while :; do
  ips=$(for i in "${IFACES[@]}"; do ip -4 -o addr show "$i" 2>/dev/null | awk '{print $4}' | cut -d/ -f1; done | sort -u | tr '\n' ' ')
  if [[ $ips != "$cur" ]]; then
    kill "${pids[@]}" 2>/dev/null; pids=()
    for ip in $ips; do avahi-publish -a -R "$NAME" "$ip" & pids+=($!); echo "$NAME -> $ip"; done
    cur=$ips
  fi
  sleep 30
done
