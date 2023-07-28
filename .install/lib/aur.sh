#!/bin/bash
set -euo pipefail

list() {
  pacman -Qemq | sed 's/$/.aur/'
}

ignore() {
  :
}


check-updates() {
  paru -Qau | while read -r line; do
    echo "aur ${line}"
  done
}

update() {
  paru -Syua
}


install() {
  sed 's/\.aur$//' | xargs --no-run-if-empty \
    paru -S --skipreview
}

explicit() {
  sed 's/\.aur$//' | xargs --no-run-if-empty \
    sudo pacman -D --asexplicit
}

remove() {
  sed 's/\.aur$//' | xargs --no-run-if-empty \
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
          if pacman -Qm "${line:1: -4}" >/dev/null 2>&1; then
            echo -e "${line:1}\x1eexplicit\x1einfo\x1eInstalled but not explicitly"
          else
            echo -e "${line:1}\x1einstall\x1eerror\x1eNot installed"
          fi
          ;;
        *)
          echo -e "$line\x1e\x1ewarn\x1eUnprovisioned"
          ;;
      esac
    done

  while read -r line; do
    echo -e "$line.aur\x1eremove-orphans\x1einfo\x1eOrphaned"
  done < <(pacman -Qtdmq || exit)
}

"$@"
