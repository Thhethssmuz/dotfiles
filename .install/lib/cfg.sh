#!/bin/bash
set -euo pipefail

DIR="$(dirname "$0")/.."


locate() {
  head -n1 "$1" | sed 's/^#\s*//'
}

render() {
  eval "$(echo -e "cat <<EOF\\n$(tail -n+2 "$1")\\nEOF")"
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
  sed 's/\.cfg$//' | while read -r line; do
    render "$DIR/templates/$line.tmpl" | \
      sudo tee "$(locate "$DIR/templates/$line.tmpl")" > /dev/null
  done
}

explicit() {
  :
}

remove() {
  :
}


status() {
  local tmpl path

  while read -r line; do
    [ -z "$line" ] && continue

    tmpl="$DIR/templates/${line:0: -4}.tmpl"

    if [ -f "$tmpl" ]; then
      path=$(locate "$tmpl")

      if [ -f "$path" ]; then

        if ! render "$tmpl" | diff "$path" - >/dev/null 2>&1; then
          echo "$line/install/error/Changed"
        fi

      else
        echo "$line/install/error/Missing"
      fi

    else
      echo "$line//error/Missing template"
    fi
  done
}

"$@"
