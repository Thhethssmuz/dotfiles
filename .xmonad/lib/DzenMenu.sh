#!/usr/bin/env bash

close() {
  dbus-send \
    --type=signal \
    --dest=org.DzenMenu.Server \
    /org/DzenMenu/Server \
    org.DzenMenu.Server.Close \
    string:"$1"
}

trap 'close $1' EXIT

"${@:2}"
