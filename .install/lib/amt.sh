#!/bin/bash
set -euo pipefail

list() {
  systemctl list-units \
    --system --type automount \
    --no-pager --no-legend \
    --state active | \
    grep -v '^proc-' | \
    sed 's/\.automount.*/\.amt/'
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
        /*) echo "${line:1}/install/error/Not found" ;;
        *)  echo "$line//warn/Unrecognised" ;;
      esac
    done
}

"$@"
