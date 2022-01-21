#!/bin/bash
set -euo pipefail

: "${USERNAME:=$USER}"


list() {
  printf "%s " \
    "XDG_RUNTIME_DIR=\"/run/user/\$(id -u)\"" \
    "systemctl list-unit-files --user --type service" \
    "--no-pager --no-legend --state enabled" | \
    sudo -Hu "$USERNAME" -s | \
    sed 's/\.service.*/\.usr/'
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
  sed 's/\.usr$/\.service/' | xargs --no-run-if-empty \
    printf "%s " \
      "XDG_RUNTIME_DIR=\"/run/user/\$(id -u)\"" \
      "systemctl --user enable" | \
    sudo -u "$USERNAME" -i bash
}

explicit() {
  :
}

remove() {
  sed 's/\.usr$/\.service/' | xargs --no-run-if-empty \
    printf "%s " \
      "XDG_RUNTIME_DIR=\"/run/user/\$(id -u)\"" \
      "systemctl --user disable" | \
    sudo -u "$USERNAME" -i bash
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
