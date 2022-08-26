#!/bin/bash
set -euo pipefail

list() {
  systemctl list-unit-files --system --type socket \
    --no-pager --no-legend --state enabled | \
    sed 's/\.socket.*/\.soc/'
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
  sed 's/\.soc$/\.socket/' | xargs --no-run-if-empty \
    sudo systemctl enable
}

explicit() {
  :
}

remove() {
  sed 's/\.soc$/\.socket/' | xargs --no-run-if-empty \
    sudo systemctl disable
}


status() {
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)  ;;
        /*) echo -e "${line:1}\x1einstall\x1eerror\x1eNot enabled" ;;
        *)  echo -e "$line\x1e\x1ewarn\x1eUnrecognised" ;;
      esac
    done
}

"$@"
