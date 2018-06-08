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
BACKGROUND="$([[ "$(hostname)" = *-vm ]] && echo "#75507B" || echo "#000000")"
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

# width of the menu parts
WIDTH_LEFT=720
WIDTH_MIDDLE=480
WIDTH_RIGHT=720

# space between the status bar and its menus
SPACE=20

# screen where the menu shall appear
SCREEN=1
