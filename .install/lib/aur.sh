#!/bin/bash
set -euo pipefail

list() {
  pacman -Qemq | sed 's/$/.aur/'
}

ignore() {
  :
}


check-updates() {
  local url="https://aur.archlinux.org/rpc/?v=5&type=multiinfo"

  for pac in $(pacman -Qqm); do
    url="$url&arg\\[\\]=$pac"
  done

  curl -s "$url" | jshon -e results -a -e Name -u -p -e Version -u | \
    sed 's/^$/-/' | paste -s -d ' \n' | sort | \
    comm -23 - <(expac -Q '%n %v' | sort) | while read -r line; do
      echo "aur ${line/ */} $(expac -Q '%v' "${line/ */}") -> ${line/* /}"
    done
}

update() {
  apacman -Syu --auronly
}


install() {
  sed 's/\.aur$//' | xargs --no-run-if-empty \
    apacman -S --noconfirm --needed
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
    echo "$line.aur/remove-orphans/info/Orphaned"
  done < <(pacman -Qtdmq || exit)
}

"$@"
