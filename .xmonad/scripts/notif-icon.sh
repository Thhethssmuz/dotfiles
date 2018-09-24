#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

case $1 in
  notification-audio-next)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf3fd^fn()"
    ;;

  notification-audio-play)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf357^fn()"
    ;;

  notification-audio-previous)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf37c^fn()"
    ;;

  notification-audio-volume-high)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf123^fn()"
    ;;

  notification-audio-volume-low)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf3a1^fn()"
    ;;

  notification-audio-volume-medium)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf131^fn()"
    ;;

  notification-audio-volume-muted)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf3a2^fn()"
    ;;

  notification-audio-volume-off)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf3a2^fn()"
    ;;

  notification-battery-low)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf295^fn()"
    ;;

  notification-device-eject)
    echo -en ""
    ;;

  notification-device-firewire)
    echo -en ""
    ;;

  notification-display-brightness-full)
    echo -en ""
    ;;

  notification-display-brightness-high)
    echo -en ""
    ;;

  notification-display-brightness-low)
    echo -en ""
    ;;

  notification-display-brightness-medium)
    echo -en ""
    ;;

  notification-display-brightness-off)
    echo -en ""
    ;;

  notification-message-email)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf322^fn()"
    ;;

  notification-message-IM)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf2b6^fn()"
    ;;

  notification-network-ethernet-connected)
    echo -en ""
    ;;

  notification-network-ethernet-disconnected)
    echo -en ""
    ;;

  notification-network-wireless-disconnected)
    echo -en ""
    ;;

  notification-network-wireless-full)
    echo -en ""
    ;;

  notification-network-wireless-high)
    echo -en ""
    ;;

  notification-network-wireless-low)
    echo -en ""
    ;;

  notification-network-wireless-medium)
    echo -en ""
    ;;

  notification-network-wireless-none)
    echo -en ""
    ;;

  notification-power-disconnected)
    echo -en ""
    ;;

  notification-screen-capture)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\\uf2ad^fn()"
    ;;

  *)
    echo -en ""
    ;;

esac
