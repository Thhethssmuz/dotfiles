#!/usr/bin/env bash

set -euo pipefail

# Make font directory
if [ ! -e ~/.local/share/fonts ]; then
  mkdir -p ~/.local/share/fonts
fi

cd ~/.local/share/fonts


# Open Sans
if [ ! -e ~/.local/share/fonts ]; then
  mkdir -p ~/.local/share/fonts
fi

cd ~/.local/share/fonts

wget https://github.com/barberboy/open-sans/archive/master.zip
unzip master.zip
rm master.zip

for file in open-sans-master/TTF/*.ttf; do
  mv $file $(basename $file)
done

rm -r open-sans-master


# Ionicons
wget https://github.com/driftyco/ionicons/blob/master/fonts/ionicons.ttf?raw=true -O ionicons.ttf


# Font Awesome
wget https://github.com/FortAwesome/Font-Awesome/blob/master/fonts/fontawesome-webfont.ttf?raw=true -O fontawesome-webfont.ttf
