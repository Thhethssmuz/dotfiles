#!/usr/bin/env bash

# shellcheck source=/home/thhethssmuz/.xmonad/scripts/config.sh
source "$HOME/.xmonad/scripts/config.sh"

#
# Generate icons using current colour theme
#

SRC="$HOME/.xmonad/icons/src"
DST="$HOME/.xmonad/icons"


# Make haskell icon separator
mk_haskell() {
  magick "$SRC/hsep-fg.png" +level-colors ",$COLOR12" "/tmp/hsep-fg.tmp.png"
  magick "$SRC/hsep-bg.png" +level-colors ",$COLOR4" "/tmp/hsep-bg.tmp.png"
  composite "/tmp/hsep-fg.tmp.png" "/tmp/hsep-bg.tmp.png" "/tmp/hsep.tmp.png"
  magick -resize "x$HEIGHT" -alpha remove -background "$BACKGROUND" "/tmp/hsep.tmp.png" "$DST/hsep.xpm"
  rm "/tmp/hsep.tmp.png" "/tmp/hsep-fg.tmp.png" "/tmp/hsep-bg.tmp.png"
}


# Make simple separators
mk_simple() {
  magick "$SRC/sep-l.png" \
    +level-colors ",$BACKGROUND" \
    -background "$COLOR4" \
    -alpha remove \
    -resize "x$HEIGHT" \
    "$DST/sep-l.xpm"

  magick "$SRC/sep-l.png" \
    +level-colors ",$COLOR4" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$HEIGHT" \
    "$DST/sep-li.xpm"
}


# Make pacman icon
mk_pacman() {
  magick "$SRC/pacman.png" \
    +level-colors ",$COLOR15" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/pacman.xpm"
  magick "$SRC/pacman.png" \
    +level-colors ",$COLOR15" \
    -rotate 90 \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/aur.xpm"
}

# Make dropbox icons
mk_dropbox() {
  magick "$SRC/dropbox-plain.png" \
    +level-colors ",$COLOR15" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/dropbox-ok.xpm"

  magick "$SRC/dropbox-sync.png" \
    +level-colors ",$COLOR15" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/dropbox-sync.xpm"

  magick "$SRC/dropbox-plain.png" \
    +level-colors ",$COLOR0" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/dropbox-off.xpm"
}

# Make seafile
mk_seafile() {
  magick "$SRC/seafile.png" \
    +level-colors ",$COLOR15" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/seafile-ok.xpm"

  magick "$SRC/seafile.png" \
    +level-colors ",$COLOR0" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/seafile-off.xpm"
}

# Make redshift icon
mk_redshift() {
  magick "$SRC/redshift.png" \
    -background "$BACKGROUND" \
    -alpha remove \
    -resize "x$FONT_SIZE" \
    "$DST/redshift.xpm"
}

# Make layout icons
mk_layout() {
  for i in "$SRC"/layout_*.png; do
    magick "$i" \
      +level-colors "$COLOR15" \
      -background "$BACKGROUND" \
      -alpha remove \
      -resize "x$HEIGHT" \
      "$DST/$(basename -s .png "$i").xpm"
  done
}

# Make volume icons
mk_volume() {
  for i in "$SRC"/volume-*.png; do
    magick "$i" \
      +level-colors ",$COLOR15" \
      -background "$BACKGROUND" \
      -alpha remove \
      -resize "x$FONT_SIZE" \
      "$DST/$(basename -s .png "$i").xpm"
  done
}


# mk_haskell
# mk_simple
mk_pacman
mk_dropbox
mk_seafile
# mk_redshift
# mk_layout
mk_volume
