#!/usr/bin/env bash

# shellcheck source=/home/thhethssmuz/.xmonad/scripts/config.sh
source ~/.xmonad/scripts/config.sh


###############################################################################
#
# Get the current volume as a percentage
#
###############################################################################

get_current_volume() {
  local mixer
  local muted

  mixer=$(amixer -D pulse get Master | grep 'Front Left:')
  muted=$(echo "$mixer" | grep -o '\[on\]')

  if [ "$muted" == "" ]; then
    echo "0"
  else
    echo "$mixer" | sed 's/.*\[\([0-9\+\)\(\.[0-9]\+\)\?%.*/\1/'
  fi
}


###############################################################################
#
# Get an icon indicating the current volume level
#
###############################################################################

get_current_icon() {
  local level
  level=$(get_current_volume)

  if [ "$level" == "0" ]; then
    echo "notification-audio-volume-off"
  elif (("$level" < 25)); then
    echo "notification-audio-volume-low"
  elif (("$level" < 50)); then
    echo "notification-audio-volume-medium"
  else
    echo "notification-audio-volume-high"
  fi
}
get_xpm_icon() {
  local level
  level=$(get_current_volume)

  if [ "$level" == "0" ]; then
    echo "$HOME/.xmonad/icons/volume-off.xpm"
  elif (("$level" < 25)); then
    echo "$HOME/.xmonad/icons/volume-low.xpm"
  elif (("$level" < 50)); then
    echo "$HOME/.xmonad/icons/volume-medium.xpm"
  else
    echo "$HOME/.xmonad/icons/volume-high.xpm"
  fi
}

###############################################################################
#
# Get a gdbar indicating the current volume level
#
###############################################################################

get_current_bar() {
  local level
  level=$(get_current_volume)
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
  local icon
  local bar

  icon=$(get_current_icon)
  bar=$(get_current_bar)

  notify-send \
    "$bar" \
    -i "$icon" \
    -t 1500 \
    -h byte:suppress-log:1 \
    -h string:sound-file:/usr/share/sounds/freedesktop/stereo/audio-volume-change.oga

  ~/.xmonad/scripts/status-bar.sh -u volume
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
Usage: $(basename "$0") OPTION

Volume control.

OPTIONS:
  -i, --inc, --increase    increase volume by 5%
  -d, --dec, --decrease    decrease volume by 5%
  -m, --mute               toggle mute

      --level              print current volume as a percentage
      --icon               print notif icon name indicating current volume
      --xpm-icon           print full path to xpm icon indicating current volume
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
  while [[ $# -gt 0 ]]; do

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

      --xpm-icon)
        get_xpm_icon
        ;;

      --bar)
        get_current_bar
        ;;

      -h|--help)
        usage
        exit 0
        ;;

      --*)
        echo "$(basename "$0"): invalid option $1"
        echo "Try $(basename "$0") --help for more info"
        exit 1
        ;;


      -??*)
        set -- "-${1:1:1}" "-${1:2}" "${@:2}"
        continue
        ;;


      *)
        echo "$(basename "$0"): invalid option $1"
        echo "Try $(basename "$0") --help for more info"
        exit 1
        ;;

    esac

    shift

  done
}

main "$@"
