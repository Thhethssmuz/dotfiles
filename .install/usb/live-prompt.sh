#!/bin/bash
set -euo pipefail
shopt -s extglob

lsblk -o NAME,SIZE,FSTYPE,MOUNTPOINT,LABEL,TYPE,PARTTYPE,MODEL
while true; do
  read -rp "Select install disk (default /dev/sda): " DISK
  case "$DISK" in
    /dev/sd[a-z]) break ;;
    '')           DISK="/dev/sda"; break ;;
    *)            echo "invalid disk" ;;
  esac
done

while true; do
  read -rp "Select size of swap partition (default no-swap): " SWAP
  case "$SWAP" in
    +([0-9])[KMGT]) break ;;
    no-swap)        SWAP=""; break ;;
    '')             break ;;
    *)              echo "Invalid size, try again."
  esac
done

while true; do
  read -rp "Type hostname (no default): " HOSTNAME
  case "$HOSTNAME" in
    +([a-z0-9_-])) break ;;
    *)             echo "Invalid hostname" ;;
  esac
done

export DISK="$DISK"
export SWAP="$SWAP"
export HOSTNAME="$HOSTNAME"
