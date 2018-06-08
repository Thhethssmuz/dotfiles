#!/bin/bash
set -euo pipefail

order() {
  echo "post-aur"
}

install() {
  echo 'fs.inotify.max_user_watches = 100000' | \
    sudo tee /etc/sysctl.d/99-sysctl.conf >/dev/null
  sysctl --system

  rm -rf ~/.dropbox-dist
  install -dm0 ~/.dropbox-dist
}

remove() {
  :
}

status() {
  if [ "$(cat /proc/sys/fs/inotify/max_user_watches)" != "100000" ]; then
    echo "dropbox.src/install/error/Inotify watch limit not configured"
  fi

  # shellcheck disable=SC2012
  if ! [ "$(ls -ld ~/.dropbox-dist | cut -c-10)" = "d---------" ]; then
    echo "dropbox.src/install/error/Auto update not disabled"
  fi
}

"$@"
