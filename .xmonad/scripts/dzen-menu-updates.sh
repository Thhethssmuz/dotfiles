#! /usr/bin/env bash

# shellcheck source=/dev/null
. ~/.xmonad/scripts/config.sh

title() {
  local updates
  updates=$(~/.xmonad/scripts/updates.sh)
  echo "^p(+20)$updates update(s) available"
}

slave() {
  ~/.xmonad/scripts/updates.sh -l | awk '{print "^pa(20)- " $0}'
}

main() {
  dzen2 \
    -fg "$COLOR7" \
    -bg "$BACKGROUND" \
    -fn "$MAIN_FONT" \
    -xs "$SCREEN" \
    -x  "$((WIDTH_LEFT + WIDTH_MIDDLE + WIDTH_RIGHT - 280 - SPACE))" \
    -y  "$((HEIGHT + SPACE))" \
    -w  280 \
    -ta l \
    -sa l \
    -e 'onstart=uncollapse;button3=exit;button4=scrollup;button5=scrolldown' \
    "$@"
}

"$@"
