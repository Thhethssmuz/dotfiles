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
  sudo pacman -U --noconfirm apacman-*.pkg.tar.xz
  cd ~
  rm -rf apacman apacman.tar.gz
}

remove() {
  :
}

status() {
  if ! hash apacman 2>/dev/null; then
    echo "apacman.src/install/error/Not installed"
  fi
}

"$@"
