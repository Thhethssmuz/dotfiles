#!/bin/bash
set -euo pipefail

order() {
  echo "post-pac"
}

install() {
  ~/.xmonad/scripts/mkicons.sh
}

remove() {
  :
}

status() {
  local i=0

  for icon in {dropbox-{off,ok,sync},pacman,volume-{high,low,medium,off}}.xpm; do
    [ -f ~/.xmonad/icons/"$icon" ] && ((i+=1))
  done

  if [ "$i" -eq "0" ]; then
    echo "status-bar-icons.src/install/error/Not installed"
  elif [ "$i" -lt "8" ]; then
    echo "status-bar-icons.src/install/error/Only partially installed"
  fi
}

"$@"
