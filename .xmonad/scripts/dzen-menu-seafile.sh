#! /usr/bin/env bash

# shellcheck source=/dev/null
source ~/.xmonad/scripts/config.sh

title() {
  echo "Seafile Status"
}

slave() {
  seaf-cli status \
  | sed 's/ \+//g' \
  | grep -v $'#Name\tStatus\tProgress$' \
  | sed 's/\t/ /g' \
  | awk '{print "^pa(20)" $0}'
}

main() {
  dzen2 \
    -fg "$COLOR7" \
    -bg "$BACKGROUND" \
    -fn "$MAIN_FONT" \
    -xs "$SCREEN" \
    -x  "$((WIDTH_LEFT + WIDTH_MIDDLE + WIDTH_RIGHT - 320 - SPACE))" \
    -y  "$((HEIGHT + SPACE))" \
    -w  320 \
    -ta c \
    -sa l \
    -e 'onstart=uncollapse;button3=exit;button4=scrollup;button5=scrolldown' \
    -l 5
}

"$@"
