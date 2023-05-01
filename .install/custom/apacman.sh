#!/bin/bash
set -euo pipefail

order() {
  echo "pre-aur"
}

install() {
  cd ~
  curl -L -O https://aur.archlinux.org/cgit/aur.git/snapshot/apacman.tar.gz
  tar xvzf apacman.tar.gz
  cd apacman
  makepkg
  sudo pacman -U --noconfirm apacman-*.pkg.tar.zst
  cd ~
  rm -rf apacman apacman.tar.gz
}

remove() {
  :
}

status() {
  if ! hash apacman 2>/dev/null; then
    echo -e "apacman.src\x1einstall\x1eerror\x1eNot installed"
  fi
}

"$@"
