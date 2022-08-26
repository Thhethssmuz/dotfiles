#!/bin/bash
set -euo pipefail

DIR="$(dirname "$0")/.."


locate() {
  head -n1 "$1" | sed 's/^#\s*//'
}

render() {
  tail -n+2 "$1"
}


list() {
  :
}

ignore() {
  :
}


check-updates() {
  :
}

update() {
  :
}


install() {
  sed 's/\.fil$//' | while read -r line; do
    if [ -n "$line" ]; then
      render "$DIR/static/$line.static" | \
        sudo tee "$(locate "$DIR/static/$line.static")" > /dev/null
    fi
  done
}

explicit() {
  :
}

remove() {
  :
}


status() {
  local file path

  while read -r line; do
    [ -z "$line" ] && continue

    file="$DIR/static/${line:0: -4}.static"

    if [ -f "$file" ]; then
      path=$(locate "$file")

      if [ -f "$path" ]; then

        if ! render "$file" | diff "$path" - >/dev/null 2>&1; then
          echo -e "$line\x1einstall\x1eerror\x1eChanged"
        fi

      else
        echo -e "$line\x1einstall\x1eerror\x1eMissing"
      fi

    else
      echo -e "$line\x1e\x1eerror\x1eMissing static file"
    fi
  done
}

"$@"
