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
    echo "initial-npmrc.src/install/error/Missing"
  fi
}

"$@"
