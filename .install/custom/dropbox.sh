#!/bin/bash
set -euo pipefail

order() {
  echo "post-aur"
}

install() {
  echo 'fs.inotify.max_user_watches = 100000' | \
    sudo tee /etc/sysctl.d/99-sysctl.conf >/dev/null
  sudo sysctl --system

  sudo rm -rf ~/.dropbox-dist
  sudo /usr/bin/install -dm0 ~/.dropbox-dist
}

remove() {
  :
}

status() {
  if [ "$(cat /proc/sys/fs/inotify/max_user_watches)" != "100000" ]; then
    echo -e "dropbox.src\x1einstall\x1eerror\x1eInotify watch limit not configured"
  fi

  # shellcheck disable=SC2012
  if ! [ -d "$HOME/.dropbox-dist" ] || ! [ "$(ls -ld "$HOME"/.dropbox-dist | cut -c-10)" = "d---------" ]; then
    echo -e "dropbox.src\x1einstall\x1eerror\x1eAuto update not disabled"
  fi
}

"$@"
