#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

title() {
  echo "$(date '+%a %b %d, %H:%M')"
}

slave() {
  cal -3w
}

main() {
  dzen2 \
    -fg "$COLOR7" \
    -bg "$BACKGROUND" \
    -fn "$MAIN_FONT" \
    -xs "$SCREEN" \
    -x  "$(($WIDTH_LEFT - 140))" \
    -y  "$((HEIGHT + SPACE))" \
    -w  "$(($WIDTH_MIDDLE + 140))" \
    -ta c \
    -sa c \
    -e 'onstart=uncollapse;button3=exit;button4=scrollup;button5=scrolldown' \
    $@
}

$@
