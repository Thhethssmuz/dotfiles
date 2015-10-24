#!/usr/bin/env bash

close-notif() {
  dbus-send \
    --type=method_call \
    --dest=org.freedesktop.Notifications \
    /org/freedesktop/Notifications \
    org.freedesktop.Notifications.CloseNotification \
    uint32:$1

  if [ $1 == "0" ]; then
    menu Close Notif
  else
    menu Refresh Notif
  fi
}

menu() {
  dbus-send \
    --type=signal \
    --dest=org.DzenMenu.Server \
    /org/DzenMenu/Server \
    org.DzenMenu.Server.$1 \
    string:$2
}

$1 ${@:2}
