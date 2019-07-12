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
  timedatectl set-timezone Europe/Oslo
  if [[ "$(hostname)" != *-vm? ]]; then
    timedatectl set-ntp true
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
      echo "timezone.src/install/error/${key}"
    fi
  done
}

"$@"
