#!/usr/bin/env bash

. ~/.xmonad/scripts/config.sh

###############################################################################
#
# Generate icons using current colour theme
#
###############################################################################

SRC=~/.xmonad/icons/src
DST=~/.xmonad/icons


# Make haskell icon separator
mk_haskell() {
  convert $SRC/hsep-fg.png +level-colors ,$COLOR12 /tmp/hsep-fg.tmp.png
  convert $SRC/hsep-bg.png +level-colors ,$COLOR4 /tmp/hsep-bg.tmp.png
  composite /tmp/hsep-fg.tmp.png /tmp/hsep-bg.tmp.png /tmp/hsep.tmp.png
  convert -resize x$HEIGHT -alpha remove -background $BACKGROUND /tmp/hsep.tmp.png $DST/hsep.xpm
  rm /tmp/hsep.tmp.png /tmp/hsep-fg.tmp.png /tmp/hsep-bg.tmp.png
}


# Make simple separators
mk_simple() {
  convert $SRC/sep-l.png \
    +level-colors ,$BACKGROUND \
    -background $COLOR4 \
    -alpha remove \
    -resize x$HEIGHT \
    $DST/sep-l.xpm

  convert $SRC/sep-l.png \
    +level-colors ,$COLOR4 \
    -background $BACKGROUND \
    -alpha remove \
    -resize x$HEIGHT \
    $DST/sep-li.xpm
}


# Make pacman icon
mk_pacman() {
  convert $SRC/pacman.png \
    +level-colors ,$COLOR15 \
    -background $BACKGROUND \
    -alpha remove \
    -resize x$FONT_SIZE \
    $DST/pacman.xpm
}

# Make dropbox icon
mk_dropbox() {
  convert $SRC/dropbox.png \
    -background $BACKGROUND \
    -alpha remove \
    -resize x$FONT_SIZE \
    $DST/dropbox.xpm
}

# Make redshift icon
mk_redshift() {
  convert $SRC/redshift.png \
    -background $BACKGROUND \
    -alpha remove \
    -resize x$FONT_SIZE \
    $DST/redshift.xpm
}

# Make layout icons
mk_layout() {
  for i in $(ls $SRC/layout_*.png); do
    convert $i \
      +level-colors $COLOR15 \
      -background $BACKGROUND \
      -alpha remove \
      -resize x$HEIGHT \
      $DST/$(basename -s .png $i).xpm
  done
}


mk_haskell
mk_simple
mk_pacman
mk_dropbox
# mk_redshift
mk_layout
