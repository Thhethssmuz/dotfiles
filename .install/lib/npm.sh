#!/bin/bash
set -euo pipefail

list() {
  find /usr/lib/node_modules -mindepth 1 -maxdepth 1 -type d -print0 | \
    xargs --null basename -a | sed 's/$/\.npm/'
}

ignore() {
  echo $'node-gyp.npm\nnpm.npm\nrebuild.npm\nsemver.npm'
}


check-updates() {
  (npm outdated -g --depth=0 2>/dev/null || true) | tail -n+2 | \
    awk '{print "npm " $1 " " $2 " -> " $4}'
}

update() {
  (npm outdated -g --depth=0 --parseable 2>/dev/null || true) | cut -d: -f4 | \
    xargs --no-run-if-empty sudo npm 'install' -g
}


install() {
  sed 's/\.npm$//' | xargs --no-run-if-empty \
    sudo npm 'install' -g
}

explicit() {
  :
}

remove() {
  sed 's/\.npm$//' | xargs --no-run-if-empty \
    sudo npm uninstall -g
}


status() {
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)  ;;
        /*) echo "${line:1}/install/error/Not installed" ;;
        *)  echo "$line//warn/Unprovisioned" ;;
      esac
    done
}

"$@"
