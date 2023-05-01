#!/bin/bash
set -euo pipefail

order() {
  echo "pre-aur"
}

install() {
  cd ~
  git clone https://aur.archlinux.org/paru.git
  cd paru
  makepkg -si
  cd ~
  rm  -rf paru
}

remove() {
  :
}

status() {
  if ! hash paru 2>/dev/null; then
    echo -e "paru.src\x1einstall\x1eerror\x1eNot installed"
  fi
}

"$@"
