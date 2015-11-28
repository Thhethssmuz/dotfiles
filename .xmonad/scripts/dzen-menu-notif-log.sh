#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

title() {
  echo -n "^p(+20)Notifications"
  echo -n "^p(_RIGHT)^p(-35)"
  echo -n "^ca(1, /home/thhethssmuz/.xmonad/scripts/dbus.sh close-notif 0)"
  echo -n "^fg($COLOR1)x^fg()"
  echo -n "^ca()"
  echo
}

main() {
  dzen2 \
    -fg "$COLOR7" \
    -bg "$BACKGROUND" \
    -fn "$MAIN_FONT" \
    -xs "$SCREEN" \
    -x  "$(($WIDTH_LEFT - 20))" \
    -y  40 \
    -w  "$(($WIDTH_MIDDLE + 20))" \
    -ta l \
    -sa l \
    -e 'onstart=uncollapse;button3=exit;button4=scrollup;button5=scrolldown' \
    $@
}

$@
