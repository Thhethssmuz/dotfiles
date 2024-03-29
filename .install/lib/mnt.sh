#!/bin/bash
set -euo pipefail

list() {
  systemctl list-units \
    --system --type mount \
    --no-pager --no-legend \
    --state active | \
    awk '{print $1}' | \
    grep -v '^dev-' | \
    grep -v '^proc-' | \
    grep -v '^run-' | \
    grep -v '^sys-' | \
    grep -v '^tmp\.' | \
    sed 's/\.mount/\.mnt/'
}

ignore() {
  "$(dirname "$0")/amt.sh" list | sed 's/\.amt/\.mnt/'
}


check-updates() {
  :
}

update() {
  :
}


install() {
  :
}

explicit() {
  :
}

remove() {
  :
}


status() {
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)  ;;
        /*) echo -e "${line:1}\x1einstall\x1eerror\x1eNot found" ;;
        *)  echo -e "$line\x1e\x1ewarn\x1eUnrecognised" ;;
      esac
    done
}

"$@"
