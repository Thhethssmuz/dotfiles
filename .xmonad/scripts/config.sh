#!/bin/bash

###############################################################################
#
# Fonts
#
###############################################################################

FONT_NAME="Ubuntu"
FONT_SIZE=12

FONT="$FONT_NAME:size=$FONT_SIZE:Regular"
BOLD="$FONT_NAME:size=$FONT_SIZE:Bold"
MONO="$FONT_NAME Mono:size=$FONT_SIZE"
BOLD_MONO="$MONO:Bold"

MAIN_FONT="$BOLD_MONO"

###############################################################################
#
# Colours
#
###############################################################################

FOREGROUND="#FFFFFF"
BACKGROUND="#000000"
COLOR0="#2E3436"     # dark grey
COLOR1="#CC0000"     # dark red
COLOR2="#4E9A06"     # dark green
COLOR3="#C4A000"     # dark yellow
COLOR4="#3465A4"     # blue
COLOR5="#75507B"     # magenta
COLOR6="#06989A"     #
COLOR7="#D3D7CF"     # white
COLOR8="#555753"     # grey
COLOR9="#EF2929"     # bright red
COLOR10="#8AE234"    # bright green
COLOR11="#FCE94F"    # bright yellow
COLOR12="#729FCF"    # bright blue
COLOR13="#AD7FA8"    # bright magenta
COLOR14="#32E2E2"    #
COLOR15="#EEEEEC"    # bright white


###############################################################################
#
# Status bars
#
###############################################################################

# height of the status bar
HEIGHT=22

# space between the status bar and its menus
SPACE=20

# screen where the menu shall appear
SCREEN="$([[ "$(hostname)" = "sia-11" ]] && echo "0" || echo "1")"

# get screen res
if [ "$SCREEN" == '0' ]; then
  RES="$(xdpyinfo | grep dimensions | awk '{print $2}')"
else
  RES="$(xrandr | grep -w connected | grep -o '[0-9]\+x[0-9]\+' | head -n"$SCREEN" | tail -n1)"
fi
RESX="$(awk -F'x' '{print $1}' <<< "$RES")"
RESY="$(awk -F'x' '{print $2}' <<< "$RES")"

# width of the menu parts
WIDTH_LEFT="$((RESX / 8 * 3))"
WIDTH_MIDDLE="$((RESX / 8 * 2))"
WIDTH_RIGHT="$((RESX / 8 * 3))"
