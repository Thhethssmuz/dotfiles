#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

title() {
  echo "$(date '+%a %b %d, %H:%M')"
}

slave() {
  cal -3
}

main() {
  dzen2 \
    -fg "$COLOR7" \
    -bg "$BACKGROUND" \
    -fn "$MAIN_FONT" \
    -xs "$SCREEN" \
    -x  "$(($WIDTH_LEFT - 80))" \
    -y  40 \
    -w  "$(($WIDTH_MIDDLE + 80))" \
    -ta c \
    -sa c \
    -e 'onstart=uncollapse;button3=exit;button4=scrollup;button5=scrolldown' \
    $@
}

$@
