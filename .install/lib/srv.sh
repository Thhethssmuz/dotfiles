#!/bin/bash
set -euo pipefail

list() {
  systemctl list-units --system --type service \
    --no-pager --no-legend --state active --state failed | \
    grep -v '^systemd-' | \
    grep -v '^lvm2-' | \
    grep -v '^getty@' | \
    sed 's/\.service.*/\.srv/'
}

ignore() {
  echo $'alsa-restore.srv\nkmod-static-nodes.srv\npolkit.srv\nrtkit-daemon.srv'
  echo $'udisks2.srv\nuser-runtime-dir@1000.srv\nuser@1000.srv'
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
