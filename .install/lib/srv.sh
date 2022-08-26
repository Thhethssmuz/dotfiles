#!/bin/bash
set -euo pipefail

list() {
  systemctl list-unit-files --system --type service \
    --no-pager --no-legend --state enabled | \
    sed 's/\.service.*/\.srv/'
}

ignore() {
  echo 'autovt@.srv'
  echo 'dbus-org.freedesktop.NetworkManager.srv'
  echo 'dbus-org.freedesktop.nm-dispatcher.srv'
  echo 'display-manager.srv'
  echo 'getty@.srv'
  echo 'NetworkManager-dispatcher.srv'
  echo 'systemd-timesyncd.srv'
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
        /*) echo -e "${line:1}\x1einstall\x1eerror\x1eNot enabled" ;;
        *)  echo -e "$line\x1e\x1ewarn\x1eUnrecognised" ;;
      esac
    done
}

"$@"
