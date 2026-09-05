#!/bin/bash
# Steer 10.0.0.0/24 (the ship's Samba and intranet) onto the ship LAN instead
# of the UM VPN.
#
# The VPN installs 10.0.0.0/25 and 10.0.0.128/25 via tun0 on every connect,
# swallowing the ship network. Longest prefix wins, so a /24 would lose to
# those /25s: four /26s are installed instead. They go on-link over a wired
# port that holds a 10.0.0.x address (the ship LAN, static 10.0.0.57/24) when
# one is up, otherwise via the Wi-Fi gateway, which also reaches the ship.
#
# Scoped to 10.0.0.0/24 only: UM DNS (10.0.1.x) and the rest of 10/8 keep
# routing over the VPN, so grid / clusters / arbutus are unaffected.
GW=192.168.3.1
WIFI=wlo1
NETS=(10.0.0.0/26 10.0.0.64/26 10.0.0.128/26 10.0.0.192/26)
# The VPN also installs /32 host routes (10.0.0.172 has been seen); a /32 beats
# any /26, so ship hosts that collide must be listed here to get their own /32.
EXTRA_HOSTS=(${SHIP_EXTRA_HOSTS:-})

need_root(){ [ "$(id -u)" = 0 ] || { echo "run with sudo: sudo $0 $1"; exit 1; }; }
wired(){ ip -4 -o addr show | awk '$2 ~ /^(en|eth)/ && $4 ~ /^10\.0\.0\./ {print $2; exit}'; }

case "$1" in
  on)
    need_root on
    dev=$(wired)
    for n in "${NETS[@]}" "${EXTRA_HOSTS[@]/%//32}"; do
      [[ -z $n ]] && continue
      if [[ -n $dev ]]; then ip route replace "$n" dev "$dev" && echo "  $n on-link via $dev"
      else ip route replace "$n" via "$GW" dev "$WIFI" && echo "  $n via $GW"; fi
    done
    ;;
  off)
    need_root off
    for n in "${NETS[@]}"; do ip route del "$n" 2>/dev/null && echo "  removed $n" || echo "  $n not present"; done
    ;;
  status)
    echo "routes for 10.0.0.x:"; ip route show | grep -E '^10\.0\.0\.' | sed 's/^/  /'
    echo "10.0.0.10 goes: $(ip route get 10.0.0.10 | head -1)"
    ;;
  *) echo "usage: $0 on|off|status"; exit 1 ;;
esac
