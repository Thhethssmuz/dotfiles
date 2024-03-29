#!/bin/bash
set -euo pipefail

DIR="$(dirname "$0")/.."
: "${USERHOME:=$(sudo -Hu "$USERNAME" -s -- echo '$HOME')}"


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
    if [ -n "$line" ]; then
      sudo mkdir -p "$(dirname "$(locate "$DIR/templates/$line.tmpl")")"
      render "$DIR/templates/$line.tmpl" | \
        sudo tee "$(locate "$DIR/templates/$line.tmpl")" > /dev/null
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
  local tmpl path

  while read -r line; do
    [ -z "$line" ] && continue

    tmpl="$DIR/templates/${line:0: -4}.tmpl"

    if [ -f "$tmpl" ]; then
      path=$(locate "$tmpl")

      if [ -f "$path" ]; then

        if ! render "$tmpl" | diff "$path" - >/dev/null 2>&1; then
          echo -e "$line\x1einstall\x1eerror\x1eChanged"
        fi

      else
        echo -e "$line\x1einstall\x1eerror\x1eMissing"
      fi

    else
      echo -e "$line\x1e\x1eerror\x1eMissing template"
    fi
  done
}

"$@"
