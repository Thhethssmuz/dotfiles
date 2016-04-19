#!/usr/bin/env bash

TMPDIR="/tmp/hunspell-no"
DICTDIR="/usr/share/hunspell"

mkdir $TMPDIR
cd $TMPDIR || exit

wget "https://alioth.debian.org/frs/download.php/file/4168/no_NO-pack2-2.2.zip"

if [ "$(shasum no_NO-pack2-2.2.zip | awk '{print $1}')" != "cb39be9c866884e9c7130628c6d487edeb87548b" ]; then
  echo "unable to verify shasum" 1>&2
  exit 1
fi

unzip "no_NO-pack2-2.2.zip"

unzip "nb_NO.zip"
sudo cp "nb_NO.aff" "$DICTDIR/nb_NO.aff"
sudo cp "nb_NO.dic" "$DICTDIR/nb_NO.dic"

unzip "nn_NO.zip"
sudo cp "nn_NO.aff" "$DICTDIR/nn_NO.aff"
sudo cp "nn_NO.dic" "$DICTDIR/nn_NO.dic"

rm -rf "$TMPDIR"
