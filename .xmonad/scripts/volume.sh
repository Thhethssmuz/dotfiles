#!/usr/bin/env bash

. ~/.xmonad/scripts/config.sh


###############################################################################
#
# Get the current volume as a percentage
#
###############################################################################

get_current_volume() {
  local mixer=$(amixer -D pulse get Master | grep 'Front Left:')
  local muted=$(echo $mixer | grep -o '\[on\]')

  if [ "$muted" == "" ]; then
    echo "0"
  else
    echo $mixer | sed 's/.*\[\([0-9\+\)\(\.[0-9]\+\)\?%.*/\1/'
  fi
}


###############################################################################
#
# Get an icon indicating the current volume level
#
###############################################################################

get_current_icon() {
  local level=$(get_current_volume)
  local icon

  if [ "$level" == "0" ]; then
    icon="\uf3b9"
  elif (("$level" < 25)); then
    icon="\uf3b8"
  elif (("$level" >= 25)) && (("$level" < 50)); then
    icon="\uf3b7"
  else
    icon="\uf3ba"
  fi

  echo -e "^fn(Ionicons:size=$FONT_SIZE)$icon^fn()"
}


###############################################################################
#
# Get a gdbar indicating the current volume level
#
###############################################################################

get_current_bar() {
  local level=$(get_current_volume)
  if hash gdbar 2>/dev/null; then
    echo "$level" | gdbar -w 180 -h 3 -fg "$COLOR7" -bg "$COLOR8"
  elif hash dzen2-gdbar 2>/dev/null; then
    echo "$level" | dzen2-gdbar -w 180 -h 3 -fg "$COLOR7" -bg "$COLOR8"
  fi
}


###############################################################################
#
# Show a volume notification in the notification tray and play a sound_change
# sound at the current sound level
#
###############################################################################

volume_notification() {
  local icon=$(get_current_icon)
  local bar=$(get_current_bar)

  /home/thhethssmuz/.xmonad/scripts/status-bar.sh --update volume

  echo -e "$icon   $bar" | \
    ~/.xmonad/scripts/notify.sh \
      -ts /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga
}


###############################################################################
#
# Increase volume by 5%
#
# We use a precise number instead of a percentage as the left and right channel
# have a tendency to get out of sync when dealing with percentages.
#
###############################################################################

increase_volume() {
  amixer -D pulse -q set Master 3277+
  volume_notification
}


###############################################################################
#
# Decrease volume by 5%
#
###############################################################################

decrease_volume() {
  amixer -D pulse -q set Master 3277-
  volume_notification
}


###############################################################################
#
# Toggle mute
#
###############################################################################

toggle_mute() {
  amixer -D pulse -q set Master toggle
  volume_notification
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename $0) OPTION

Volume control.

OPTIONS:
  -i, --inc, --increase    increase volume by 5%
  -d, --dec, --decrease    decrease volume by 5%
  -m, --mute               toggle mute

      --level              print current volume as a percentage
      --icon               print dzen formatted icon indicating current volume
      --bar                print a gdbar indicating current volume level

  -h, --help               display this help and exit

EOF
}


###############################################################################
#
# Argument parsing
#
###############################################################################

main() {
  while [[ $# > 0 ]]; do

    case $1 in

      -d|--dec|--decrease)
        decrease_volume
        ;;

      -i|--inc|--increase)
        increase_volume
        ;;

      -m|--mute)
        toggle_mute
        ;;

      --level)
        get_current_volume
        ;;

      --icon)
        get_current_icon
        ;;

      --bar)
        get_current_bar
        ;;

      -h|--help)
        usage
        exit 0
        ;;

      --*)
        echo "$(basename $0): invalid option $1"
        echo "Try $(basename $0) --help for more info"
        exit 1
        ;;

      -??*)
        local tmp1=$(echo "$1" | sed 's/-\(.\).*/-\1/')
        local tmp2=$(echo "$1" | sed 's/-./-/')
        set -- "$tmp2" "${@:2}"
        set -- "$tmp1" "$@"
        continue
        ;;

      *)
        echo "$(basename $0): invalid option $1"
        echo "Try $(basename $0) --help for more info"
        exit 1
        ;;

    esac

    shift

  done
}

main $@
