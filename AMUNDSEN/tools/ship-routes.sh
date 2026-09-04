#!/bin/bash
# Steer 10.0.0.0/24 (shipboard Samba) to the LOCAL gateway instead of the UM VPN.
#
# The UM VPN installs 10.0.0.0/25 and 10.0.0.128/25 via tun0, swallowing the
# ship's 10.0.0.x network. Longest-prefix-match wins, so a /24 would LOSE to
# those /25s — we install four /26s, which beat them.
#
# Scoped to 10.0.0.0/24 only: UM DNS (10.0.1.5/.6) and the rest of 10/8 keep
# routing over the VPN, so grid / clusters / arbutus are unaffected.
GW=192.168.3.1
DEV=wlo1
NETS=(10.0.0.0/26 10.0.0.64/26 10.0.0.128/26 10.0.0.192/26)

need_root(){ [ "$(id -u)" = 0 ] || { echo "run with sudo: sudo $0 $1"; exit 1; }; }

case "$1" in
  on)
    need_root on
    for n in "${NETS[@]}"; do
      ip route replace "$n" via "$GW" dev "$DEV" && echo "  added $n via $GW"
    done
    ;;
  off)
    need_root off
    for n in "${NETS[@]}"; do
      ip route del "$n" via "$GW" dev "$DEV" 2>/dev/null && echo "  removed $n" || echo "  $n not present"
    done
    ;;
  status)
    echo "routes for 10.0.0.x:"
    ip route show | grep -E '^10\.0\.0\.' | sed 's/^/  /'
    printf 'lookup 10.0.0.10 -> '
    ip route get 10.0.0.10 2>/dev/null | head -1 | grep -oE 'dev [a-z0-9]+'
    ;;
  *) echo "usage: ship-routes {on|off|status}  (on/off need sudo)"; exit 1 ;;
esac
