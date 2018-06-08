#!/bin/bash
set -euo pipefail

DICTDIR=/usr/share/hunspell

order() {
  echo "pre-aur"
}

install() {
  TMPDIR=/tmp/hunspell-no

  mkdir -p $TMPDIR
  cd $TMPDIR || exit

  wget "https://alioth.debian.org/frs/download.php/file/4168/no_NO-pack2-2.2.zip"

  SHASUM=cb39be9c866884e9c7130628c6d487edeb87548b
  if [ "$(shasum no_NO-pack2-2.2.zip | awk '{print $1}')" != "$SHASUM" ]; then
    echo "unable to verify shasum" 1>&2
    exit 1
  fi

  unzip "no_NO-pack2-2.2.zip"

  unzip "nb_NO.zip"
  cp "nb_NO.aff" "$DICTDIR/nb_NO.aff"
  cp "nb_NO.dic" "$DICTDIR/nb_NO.dic"

  unzip "nn_NO.zip"
  cp "nn_NO.aff" "$DICTDIR/nn_NO.aff"
  cp "nn_NO.dic" "$DICTDIR/nn_NO.dic"

  rm -rf "$TMPDIR"
}

remove() {
  :
}

status() {
  local i=0
  [ -f "$DICTDIR/nb_NO.aff" ] && ((i+=1))
  [ -f "$DICTDIR/nb_NO.dic" ] && ((i+=1))
  [ -f "$DICTDIR/nn_NO.aff" ] && ((i+=1))
  [ -f "$DICTDIR/nn_NO.dic" ] && ((i+=1))

  if [ "$i" -eq "0" ]; then
    echo "hunspell-no.src/install/error/Not installed"
  elif [ "$i" -lt "4" ]; then
    echo "hunspell-no.src/install/error/Only partially installed"
  fi
}

"$@"
