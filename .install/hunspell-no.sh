#!/usr/bin/env bash

TMPDIR="/tmp/hunspell-no"
DICTDIR="/usr/share/hunspell"

mkdir $TMPDIR
cd $TMPDIR || exit

wget "https://alioth.debian.org/frs/download.php/file/4168/no_NO-pack2-2.2.zip"
unzip "no_NO-pack2-2.2.zip"

unzip "nb_NO.zip"
sudo cp "nb_NO.aff" "$DICTDIR/nb_NO.aff"
sudo cp "nb_NO.dic" "$DICTDIR/nb_NO.dic"

unzip "nn_NO.zip"
sudo cp "nn_NO.aff" "$DICTDIR/nn_NO.aff"
sudo cp "nn_NO.dic" "$DICTDIR/nn_NO.dic"

rm -rf "$TMPDIR"
