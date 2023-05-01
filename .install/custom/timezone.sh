#!/bin/bash
set -euo pipefail

declare -A TIMEZONE_SETTINGS=(\
  ["Timezone"]="Europe/Oslo" \
  ["LocalRTC"]="no" \
  )

if [[ "$(hostname)" != *-vm? ]]; then
  TIMEZONE_SETTINGS["NTP"]="yes"
fi

order() {
  echo "post-pac"
}

install() {
  sudo timedatectl set-timezone Europe/Oslo
  if [[ "$(hostname)" != *-vm? ]]; then
    sudo timedatectl set-ntp true
  fi
}

remove() {
  :
}

status() {
  local actual
  for key in "${!TIMEZONE_SETTINGS[@]}"; do
    actual="$(timedatectl show | (grep "^${key}=" || true) | sed 's/^[^=]*=//')"
    if [ "$actual" != "${TIMEZONE_SETTINGS[$key]}" ]; then
      echo -e "timezone.src\x1einstall\x1eerror\x1e${key}"
    fi
  done
}

"$@"
