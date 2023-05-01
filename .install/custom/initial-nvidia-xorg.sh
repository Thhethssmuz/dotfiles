#!/bin/bash
set -euo pipefail

TMPL="$(dirname "$0")/../templates/initial-nvidia-xorg.tmpl"

locate() {
  head -n1 "$1" | sed 's/^#\s*//'
}

render() {
  eval "$(echo -e "cat <<EOF\\n$(tail -n+2 "$1")\\nEOF")"
}

order() {
  echo "pre-cfg"
}

install() {
  render "$TMPL" | sudo tee "$(locate "$TMPL")" > /dev/null
}

remove() {
  :
}

status() {
  if [ ! -f "$(locate "$TMPL")" ]; then
    echo -e "initial-nvidia-xorg.src\x1einstall\x1eerror\x1eMissing"
  fi
}

"$@"
