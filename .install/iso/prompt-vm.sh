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
read -rp "Enter username for auto-install [$(whoami)]: " USERNAME
if [ -z "$USERNAME" ]; then
  USERNAME="$(whoami)"
fi

echo ""
read -rp "Enter root password for auto-install [1234]: " PASSWORD
if [ -z "$PASSWORD" ]; then
  PASSWORD="1234"
fi

export HOSTNAME
export USERNAME
export PASSWORD
