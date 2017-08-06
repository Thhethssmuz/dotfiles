#!/bin/bash
set -euo pipefail

list() {
  systemctl list-unit-files \
    --system --type service \
    --no-pager --no-legend \
    --state enabled | sed 's/\.service.*/\.srv/'
}

ignore() {
  echo $'autovt@.srv\ngetty@.srv\nsystemd-timesyncd.srv'
}


check-updates() {
  :
}

update() {
  :
}


install() {
  sed 's/\.srv$/\.service/' | xargs --no-run-if-empty \
    sudo systemctl enable
}

explicit() {
  :
}

remove() {
  sed 's/\.srv$/\.service/' | xargs --no-run-if-empty \
    sudo systemctl disable
}


status() {
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)  ;;
        /*) echo "${line:1}/install/error/Not enabled" ;;
        *)  echo "$line//warn/Unrecognised" ;;
      esac
    done
}

"$@"
