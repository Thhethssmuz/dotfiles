#!/bin/bash
set -euo pipefail

order() {
  echo "pre-npm"
}

install() {
  echo "prefix=$HOME/.local" > "$HOME/.npmrc"
}

remove() {
  :
}

status() {
  if [ ! -f "$HOME/.npmrc" ]; then
    echo -e "initial-npmrc.src\x1einstall\x1eerror\x1eMissing"
  fi
}

"$@"
