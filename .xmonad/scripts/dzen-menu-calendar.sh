#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

title() {
  date '+%a %b %d, %H:%M'
}

slave() {
  TERM=rxvt-unicode-256color cal -3w --color=always | \
    sed -e "s/\x1b\\[7m/^fg($COLOR1)/" -e "s/\x1b\\[0m/^fg()/"
}

main() {
  dzen2 \
    -fg "$COLOR7" \
    -bg "$BACKGROUND" \
    -fn "$MAIN_FONT" \
    -xs "$SCREEN" \
    -x  "$((WIDTH_LEFT - 75))" \
    -y  "$((HEIGHT + SPACE))" \
    -w  "$((WIDTH_MIDDLE + 150))" \
    -ta c \
    -sa c \
    -e 'onstart=uncollapse;button3=exit;button4=scrollup;button5=scrolldown' \
    "$@"
}

"$@"
