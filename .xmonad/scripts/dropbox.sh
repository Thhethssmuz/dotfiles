#!/usr/bin/env bash

if hash dropbox-cli 2>/dev/null; then

  case "$(dropbox-cli status)" in
    "Dropbox isn't running!") echo "$HOME/.xmonad/icons/dropbox-off.xpm"  ;;
    "Up to date")             echo "$HOME/.xmonad/icons/dropbox-ok.xpm"   ;;
    *)                        echo "$HOME/.xmonad/icons/dropbox-sync.xmp" ;;
  esac

else

  echo "$HOME/.xmonad/icons/dropbox-off.xpm"

fi
