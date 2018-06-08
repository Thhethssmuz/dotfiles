#!/bin/bash
set -euo pipefail

order() {
  echo "post-aur"
}

install() {
  mkdir -p ~/.config/sublime-text-3/Installed\ Packages
  cd ~/.config/sublime-text-3/Installed\ Packages
  curl -O "https://packagecontrol.io/Package Control.sublime-package"
}

remove() {
  :
}

status() {
  if [ -f ~/.config/sublime-text-3/Installed\ Packages ]; then
    echo "sublime-text.src/install/error/Not installed"
  fi
}

"$@"
