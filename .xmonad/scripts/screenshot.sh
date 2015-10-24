#!/usr/bin/env bash

. ~/.xmonad/scripts/config.sh

OUTPUT_DIR=~/Screenshots

screenshot() {
  mkdir -p $OUTPUT_DIR

  local file="$OUTPUT_DIR/$(date '+%Y-%m-%d %H:%M:%S').png"

  sleep 0.1

  scrot $@ "$file"

  notify-send \
    "$(basename "$file")" \
    -i notification-screen-capture \
    -h byte:suppress-log:1 \
    -h string:sound-file:/usr/share/sounds/freedesktop/stereo/screen-capture.oga
}

screenshot $@
