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
    render "$DIR/static/$line.static" | \
      sudo tee "$(locate "$DIR/static/$line.static")" > /dev/null
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
          echo "$line/install/error/Changed"
        fi

      else
        echo "$line/install/error/Missing"
      fi

    else
      echo "$line//error/Missing static file"
    fi
  done
}

"$@"
