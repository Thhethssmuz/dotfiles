#!/bin/bash
set -euo pipefail

PKGDIR=~/.config/sublime-text-3/Installed\ Packages

order() {
  echo "post-aur"
}

install() {
  mkdir -p "$PKGDIR"
  curl "https://packagecontrol.io/Package%20Control.sublime-package" \
    --output - > "$PKGDIR/Package Control.sublime-package"
}

remove() {
  :
}

status() {
  if ! [ -f "$PKGDIR/Package Control.sublime-package" ]; then
    echo -e "package-control.src\x1einstall\x1eerror\x1eNot installed"
  fi
}

"$@"
