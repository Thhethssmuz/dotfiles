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
    echo -e "systemd-resolv-stub.src\x1einstall\x1eerror\x1eNot installed"
  fi
}

"$@"
