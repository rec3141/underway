#!/bin/bash
# Basic firewall setup for enhanced security
# This is a simplified version - the full Anthropic version has more rules

# Allow established connections
iptables -A OUTPUT -m conntrack --ctstate ESTABLISHED -j ACCEPT

# Allow DNS
iptables -A OUTPUT -p udp --dport 53 -j ACCEPT

# Allow HTTPS (for npm, API calls)
iptables -A OUTPUT -p tcp --dport 443 -j ACCEPT

# Allow HTTP (for some package managers)
iptables -A OUTPUT -p tcp --dport 80 -j ACCEPT

echo "Basic firewall rules applied"
