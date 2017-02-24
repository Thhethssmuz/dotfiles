#!/usr/bin/env bash

notify-send \
  "New Mail:" \
  "$@" \
  -h string:sound-file:/usr/share/sounds/freedesktop/stereo/message.oga
