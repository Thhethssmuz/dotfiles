#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

dzen2 \
  -fg "$COLOR7" \
  -bg "$BACKGROUND" \
  -fn "$MAIN_FONT" \
  -xs "$SCREEN" \
  -x  "$WIDTH_LEFT" \
  -y  0 \
  -w  "$WIDTH_MIDDLE" \
  -h  "$HEIGHT" \
  -ta c
