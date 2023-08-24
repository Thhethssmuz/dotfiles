#!/bin/bash
set -euo pipefail

declare -A KEYMAP_SETTINGS=(\
  ["System Locale"]="LANG=en_GB.UTF-8" \
  ["VC Keymap"]="dvorak" \
  ["X11 Layout"]="no" \
  ["X11 Model"]="pc104" \
  ["X11 Variant"]="dvorak" \
  )

order() {
  echo "post-pac"
}

install() {
  sudo localectl set-locale LANG=en_GB.UTF-8
  sudo localectl set-x11-keymap no pc104 dvorak
  sudo localectl set-keymap --no-convert dvorak
  echo "KEYMAP=dvorak" | sudo tee /etc/vconsole.conf > /dev/null
}

remove() {
  :
}

status() {
  local actual
  for key in "${!KEYMAP_SETTINGS[@]}"; do
    actual="$(localectl status | (grep "^\s*${key}:" || true) | sed 's/^[^:]*:\s*//')"
    if [ "$actual" != "${KEYMAP_SETTINGS[$key]}" ]; then
      echo -e "keymap.src\x1einstall\x1eerror\x1e${key}"
    fi
  done
}

"$@"
