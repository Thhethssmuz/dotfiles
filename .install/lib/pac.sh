#!/bin/bash
set -euo pipefail

list() {
  pacman -Qenq | sed 's/$/.pac/'
}

ignore() {
  pacman -Qgq base base-devel xorg xorg-drivers xorg-apps 2>/dev/null | sed 's/$/.pac/'
}


check-updates() {
  checkupdates | sed 's/^/pac /' || :
}

update() {
  sudo pacman -Syu
}


install() {
  sed 's/\.pac$//' | xargs --no-run-if-empty \
    sudo pacman -S --noconfirm --needed
}

explicit() {
  sed 's/\.pac$//' | xargs --no-run-if-empty \
    sudo pacman -D --asexplicit
}

remove() {
  sed 's/\.pac$//' | xargs --no-run-if-empty \
    sudo pacman -Rs --noconfirm
}


status() {
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)
          ;;
        /*)
          if pacman -Qn "${line:1: -4}" >/dev/null 2>&1; then
            echo "${line:1}/explicit/info/Installed but not explicitly"
          else
            echo "${line:1}/install/error/Not installed"
          fi
          ;;
        *)
          echo "$line//warn/Unprovisioned"
          ;;
      esac
    done

  while read -r line; do
    echo "$line.pac/remove-orphans/info/Orphaned"
  done < <(pacman -Qtdnq || exit)
}

"$@"
