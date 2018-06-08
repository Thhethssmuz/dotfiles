#!/bin/bash
set -euo pipefail

PKGDIR=~/.config/sublime-text-3/Installed\ Packages

order() {
  echo "post-aur"
}

install() {
  mkdir -p "$PKGDIR"
  cd "$PKGDIR"
  curl -O "https://packagecontrol.io/Package Control.sublime-package"
}

remove() {
  :
}

status() {
  if ! [ -f "$PKGDIR/Package Control.sublime-package" ]; then
    echo "package-control.src/install/error/Not installed"
  fi
}

"$@"
