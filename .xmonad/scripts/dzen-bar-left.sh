#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

dzen2 \
  -fg "$COLOR7" \
  -bg "$BACKGROUND" \
  -fn "$MAIN_FONT" \
  -xs "$SCREEN" \
  -x  0 \
  -y  0 \
  -w  "$WIDTH_LEFT" \
  -h  "$HEIGHT" \
  -ta l
