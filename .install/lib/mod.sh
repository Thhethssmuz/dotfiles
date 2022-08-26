#!/bin/bash
set -euo pipefail

list() {
  find /etc/modules-load.d -type f -name '*.conf' -print0 | \
    xargs --null grep -v '\s*[#;]' | sed 's/$/.mod/'
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
  sed 's/\.mod$//' | while read -r line; do
    if [ -n "$line" ]; then
      echo "$line" | sudo tee "/etc/modules-load.d/$line.conf" > /dev/null
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
  comm --output-delimiter=/ -3 <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)  ;;
        /*) echo -e "${line:1}\x1einstall\x1eerror\x1eNot enabled" ;;
        *)  echo -e "$line\x1e\x1ewarn\x1eUnrecognised" ;;
      esac
    done
}

"$@"
