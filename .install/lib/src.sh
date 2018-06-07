#!/bin/bash
set -euo pipefail

DIR=$(realpath "$(dirname "$0")")


list() {
  :
}

ignore() {
  :
}


check-updates() {
  :
}

update() {
  :
}


install() {
  sed 's/\.src$//' | while read -r line; do
    if [ -n "$line" ]; then
      sudo -u "$USERNAME" -i "$DIR/../custom/$line.sh install"
    fi
  done
}

explicit() {
  :
}

remove() {
  sed 's/\.src$//' | while read -r line; do
    if [ -n "$line" ]; then
      sudo -u "$USERNAME" -i "$DIR/../custom/$line.sh" "remove"
    fi
  done
}


status() {
  sed 's/\.src$//' | while read -r line; do
    if [ -n "$line" ]; then
      sudo -u "$USERNAME" -i "$DIR/../custom/$line.sh" "status"
    fi
  done
}

"$@"
