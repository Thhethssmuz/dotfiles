#!/bin/bash
set -euo pipefail

while true; do
  read -rp "Type name of main user (default thhethssmuz): " USERNAME
  case "$USERNAME" in
    '') USERNAME="thhethssmuz"; break ;;
    *)  break ;;
  esac
done

export USERNAME="$USERNAME"
