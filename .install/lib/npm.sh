#!/bin/bash
set -euo pipefail

: "${USERNAME:=$USER}"
: "${USERHOME:=$(sudo -Hu "$USERNAME" -s -- echo '$HOME')}"


list() {
  if [ -d "$USERHOME"/.local/lib/node_modules/ ]; then
    find "$USERHOME"/.local/lib/node_modules/ -mindepth 1 -maxdepth 2 -type d | \
      sed "s|$USERHOME/.local/lib/node_modules/||" | \
      grep "^\(@[^\/]*\/\)\?[^@][^/]*$" | \
      sed 's/$/\.npm/'
  fi
}

ignore() {
  :
}


check-updates() {
  (npm outdated -g --depth=0 2>/dev/null || :) | tail -n+2 | \
    awk '{print "npm " $1 " " $2 " -> " $4}'
}

update() {
  (npm outdated -g --depth=0 --parseable 2>/dev/null || :) | cut -d: -f4 | \
    xargs --no-run-if-empty npm -g 'install'
}


install() {
  sed 's/\.npm$//' | xargs --no-run-if-empty npm -g 'install'
}

explicit() {
  :
}

remove() {
  sed 's/\.npm$//' | xargs --no-run-if-empty npm -g uninstall
}


status() {
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)  ;;
        /*) echo -e "${line:1}\x1einstall\x1eerror\x1eNot installed" ;;
        *)  echo -e "$line\x1e\x1ewarn\x1eUnprovisioned" ;;
      esac
    done
}

"$@"
