#!/bin/bash
set -euo pipefail
shopt -s extglob

echo ""
ip link show
echo ""
INTERFACES="$(ip link show | grep -o '^[0-9]\+: [^:]*' | awk '{print $2}')"

while true; do
  read -rp "Select interface (no default): " INTERFACE
  if [ -z "$INTERFACE" ]; then
    echo "Invalid interface, try again."
  elif [ "$(echo "$INTERFACES" | grep -o "$INTERFACE")" == "$INTERFACE" ]; then
    break;
  else
    echo "Invalid interface, try again."
  fi
done

while true; do
  read -rp "Type IP address/subnet (no default): " IP
  if [[ $IP =~ ^([0-9]+\.[0-9]+\.[0-9]+\.)([0-9]+)(\/[0-9]+)$ ]]; then
    DEFAULT_BROADCAST="${BASH_REMATCH[1]}255"
    DEFAULT_GATEWAY="${BASH_REMATCH[1]}1"
    break
  else
    echo "Invalid IP and/or subnet, try again."
  fi
done

while true; do
  read -rp "Type broadcast address (default $DEFAULT_BROADCAST): " BROADCAST
  case "$BROADCAST" in
    +([0-9])\.+([0-9])\.+([0-9])\.+([0-9])) break ;;
    '') BROADCAST="$DEFAULT_BROADCAST"; break ;;
    *) echo "Invalid broadcast address, try again."
  esac
done

while true; do
  read -rp "Type default gateway address (default $DEFAULT_GATEWAY): " GATEWAY
  case "$GATEWAY" in
    +([0-9])\.+([0-9])\.+([0-9])\.+([0-9])) break ;;
    '') GATEWAY="$DEFAULT_GATEWAY"; break ;;
    *) echo "Invalid gateway address, try again."
  esac
done

while true; do
  read -rp "Type DNS server address (default 8.8.8.8): " DNS
  case "$DNS" in
    +([0-9])\.+([0-9])\.+([0-9])\.+([0-9])) break ;;
    '') DNS=8.8.8.8; break ;;
    *) echo "Invalid DNS server address, try again."
  esac
done

ip addr add "$IP" broadcast "$BROADCAST" dev "$INTERFACE"
ip route add default via "$GATEWAY"
echo "nameserver $DNS" >> /etc/resolv.conf

ping google.com -c 1
