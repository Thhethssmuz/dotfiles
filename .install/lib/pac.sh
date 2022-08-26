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
    echo -e "$line.pac\x1eremove-orphans\x1einfo\x1eOrphaned"
  done < <(pacman -Qtdnq || exit)

  if [ "$(du -d0 /var/cache/pacman/ | awk '{print $1}')" -gt 15728640 ]; then
    echo -e "pacman-cache\x1e\x1eerror\x1e$(du -hd0 /var/cache/pacman/ | awk '{print $1}')"
  fi
}

"$@"
