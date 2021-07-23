#!/bin/bash
set -euo pipefail

order() {
  echo "post-pac"
}

install() {
  sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
}

remove() {
  :
}

status() {
  if [ "$(readlink -f /etc/resolv.conf)" != "/run/systemd/resolve/stub-resolv.conf" ]; then
    echo "systemd-resolv-stub.src/install/error/Not installed"
  fi
}

"$@"
