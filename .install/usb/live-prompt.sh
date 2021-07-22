#!/bin/bash
set -euo pipefail
shopt -s extglob

lsblk -o NAME,SIZE,FSTYPE,MOUNTPOINT,LABEL,TYPE,PARTTYPE,MODEL
while true; do
  read -rp "Select install disk (default /dev/nvme0n1): " DISK
  case "$DISK" in
    /dev/sd[a-z])
      PARTITION1="$DISK"1
      PARTITION2="$DISK"2
      break ;;

    /dev/nvme[0-9]n[0-9])
      PARTITION1="$DISK"p1;
      PARTITION2="$DISK"p2;
      break ;;

    '')
      DISK="/dev/nvme0n1";
      PARTITION1="/dev/nvme0n1p1";
      PARTITION2="/dev/nvme0n1p2";
      break ;;

    *)
      echo "invalid disk" ;;

  esac
done

free -h
while true; do
  read -rp "Select size of swap partition e.g.: '24G' (default no-swap): " SWAP
  case "$SWAP" in
    +([0-9])[KMGT]) break ;;
    no-swap)        SWAP=""; break ;;
    '')             break ;;
    *)              echo "Invalid size, try again."
  esac
done

lscpu | grep 'Vendor ID' -A10
while true; do
  read -rp "Select CPU profile, i.e.: amd or intel (default none): " CPU_PROFILE
  case "$CPU_PROFILE" in
    amd)   break ;;
    intel) break ;;
    none)  CPU_PROFILE=""; break;;
    '')    break ;;
    *)     echo "Invalid CPU profile" ;;
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
export PARTITION1="$PARTITION1"
export PARTITION2="$PARTITION2"
export SWAP="$SWAP"
export CPU_PROFILE="$CPU_PROFILE"
export HOSTNAME="$HOSTNAME"
