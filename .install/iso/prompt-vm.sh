#!/bin/bash

echo ""
while true; do
  read -rp "Enter hostname for auto-install: " HOSTNAME
  case "$HOSTNAME" in
    *-"$PROFILE") break ;;
    *) echo "Hostname should end with \`-$PROFILE'" ;;
  esac
done

echo ""
read -rp "Enter username for auto-install: " USERNAME
echo ""
read -rp "Enter root password for auto-install: " PASSWORD

export HOSTNAME
export USERNAME
export PASSWORD
