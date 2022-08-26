#!/bin/bash
set -euo pipefail

list() {
  systemctl list-units \
    --system --type automount \
    --no-pager --no-legend \
    --state active | \
    awk '{print $1}' | \
    grep -v '^proc-' | \
    sed 's/\.automount/\.amt/'
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
