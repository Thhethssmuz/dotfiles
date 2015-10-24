#! /usr/bin/env bash

. ~/.xmonad/scripts/config.sh

case $1 in
  notification-audio-next)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf051^fn()"
    ;;

  notification-audio-play)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf04b^fn()"
    ;;

  notification-audio-previous)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf048^fn()"
    ;;

  notification-audio-volume-high)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf3ba^fn()"
    ;;

  notification-audio-volume-low)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf3b7^fn()"
    ;;

  notification-audio-volume-medium)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf3ba^fn()"
    ;;

  notification-audio-volume-muted)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf3b9^fn()"
    ;;

  notification-audio-volume-off)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf3b8^fn()"
    ;;

  notification-battery-low)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf112^fn()"
    ;;

  notification-device-eject)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf052^fn()"
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
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf0e0^fn()"
    ;;

  notification-message-IM)
    echo -en "^fn(Ionicons:size=$FONT_SIZE)\uf075^fn()"
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

  *)
    echo -en ""
    ;;

esac
