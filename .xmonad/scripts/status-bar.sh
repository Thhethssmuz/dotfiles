#!/usr/bin/env bash

# shellcheck source=/dev/null
source ~/.xmonad/scripts/config.sh


FIFO=/tmp/status-bar.fifo
PID_FILE=/tmp/status-bar.pid
OUTPUT=~/.xmonad/scripts/dzen-bar-right.sh


#
# Render individual status indicators
#

render_keyboard_indicator() {
  # echo -n  "^ca(1, exec ~/.xmonad/scripts/dzen-menu-keyboard.sh)"
  echo -n  "^ca(4, exec ~/.xmonad/scripts/keyboard.sh --prev)"
  echo -n  "^ca(5, exec ~/.xmonad/scripts/keyboard.sh --next)"
  echo -ne "^fn(Font Awesome 6 Free:size=$FONT_SIZE)\\uf11c^fn() "
  echo -n  "$(~/.xmonad/scripts/keyboard.sh --code)"
  echo -n  "^ca()"
  echo -n  "^ca()"
}

render_updates_indicator() {
  local updates line
  updates=$(~/.xmonad/scripts/updates.sh -g)
  line+="^i($HOME/.xmonad/icons/pacman.xpm) $(awk '{print $1}' <<< "$updates") "
  line+="^i($HOME/.xmonad/icons/aur.xpm) $(awk '{print $2}' <<< "$updates") "
  line+="^fn(Font Awesome 6 Brands:size=$FONT_SIZE)\\uf3d4^fn() $(awk '{print $3}' <<< "$updates")"
  echo -ne "^ca(1, ~/.xmonad/scripts/dbus.sh menu Toggle Updates)$line^ca()"
}

render_volume_indicator() {
  echo -n "^ca(1, exec ~/.xmonad/scripts/volume.sh -m)"
  echo -n "^ca(4, exec ~/.xmonad/scripts/volume.sh -i)"
  echo -n "^ca(5, exec ~/.xmonad/scripts/volume.sh -d)"
  echo -n "$(~/.xmonad/scripts/notif-icon.sh "$(~/.xmonad/scripts/volume.sh --icon)") "
  echo -n "$(~/.xmonad/scripts/volume.sh --level)%"
  echo -n "^ca()^ca()^ca()"
}

render_dropbox_indicator() {
  if hash dropbox-cli 2>/dev/null; then

    echo -n "^ca(1, ~/.xmonad/scripts/dbus.sh menu Toggle Dropbox)"

    if [ "$(dropbox-cli status)" == "Dropbox isn't running!" ]; then
      echo -n "^i($HOME/.xmonad/icons/dropbox-off.xpm)"
    elif [ "$(dropbox-cli status)" == "Up to date" ]; then
      echo -n "^i($HOME/.xmonad/icons/dropbox-ok.xpm)"
    else
      echo -n "^i($HOME/.xmonad/icons/dropbox-sync.xpm)"
    fi

    echo -n "^ca()"

  else

    echo -n "^i($HOME/.xmonad/icons/dropbox-off.xpm)"

  fi
}

render_seafile_indicator() {
  if hash seaf-cli 2>/dev/null; then

    echo -n "^ca(1, ~/.xmonad/scripts/dbus.sh menu Toggle Seafile)"

    if ! seaf-cli status >/dev/null 2>&1; then
      echo -n "^i($HOME/.xmonad/icons/seafile-off.xpm)"
    else
      echo -n "^i($HOME/.xmonad/icons/seafile-ok.xpm)"
    fi

    echo -n "^ca()"

  else

    echo -n "^i($HOME/.xmonad/icons/seafile-off.xpm)"

  fi
}

render_power_indicator() {
  local status level icon

  for bat in /sys/class/power_supply/BAT*; do
    [ -e "$bat" ] || continue
    status=$(< "$bat/status")
    level=$(< "$bat/capacity")
  done

  if [ -z "$status" ]; then
    return
  fi

  if [ "$status" == "Discharging" ]; then
    if [[ "$level" -gt 95 ]]; then
      icon="\\uf240"
    elif [[ "$level" -gt 75 ]]; then
      icon="\\uf241"
    elif [[ "$level" -gt 50 ]]; then
      icon="\\uf242"
    elif [[ "$level" -gt 25 ]]; then
      icon="\\uf243"
    else
      icon="\\uf244"
    fi
  elif [ "$status" == "Not charging" ]; then
    icon="\\uf1e6"
  else
    icon="\\ue55b"
  fi

  echo -ne "^fn(Font Awesome 6 Free Solid:size=$FONT_SIZE)$icon^fn() $level%"
}

render_user_indicator() {
  echo -n "$(whoami)@$(hostname) "
}


#
# Update a status indicator at a given interval
#
# Arguments:
#   $1  indicator name
#   $2  interval in seconds
#

set_update_interval() {
  while true; do
    echo "$1" >> $FIFO
    sleep "$2"
  done &
  echo $! >> $PID_FILE
  disown
}


#
# Send an update signal to the status bar for a specific indicator
#
# Arguments:
#   $1  indicator name
#

update() {
  echo "$1" >> $FIFO
}


#
# Kill the status bar
#

kill_statusbar() {
  xargs kill < $PID_FILE
  rm -f $FIFO $PID_FILE
}


#
# Start the status bar
#

run() {

  local keyboard_state="_"
  local updates_state="_"
  local volume_state="_"
  local dropbox_state="_"
  local seafile_state="_"
  local power_state="_"
  local user_state="_ "

  render_all_indicators() {
    local all=(
      "$keyboard_state"
      "$updates_state"
      "$volume_state"
      "$dropbox_state"
      "$seafile_state"
      "$power_state"
      "$user_state"
    )
    echo "${all[*]}"
  }

  trap kill_statusbar EXIT

  echo > $PID_FILE
  if [ ! -e $FIFO ]; then
    mkfifo $FIFO
  fi

  ( sleep 2 && update keyboard ) &
  ( sleep 5 && set_update_interval updates 900 ) &
  ( sleep 5 && update volume ) &
  ( sleep 5 && set_update_interval dropbox 15 ) &
  ( sleep 5 && set_update_interval seafile 15 ) &
  ( sleep 2 && set_update_interval power 10 ) &
  ( sleep 1 && update user ) &

  while true; do
    render_all_indicators
    read -r line <$FIFO
    case $line in
      keyboard)  keyboard_state=$(render_keyboard_indicator) ;;
      updates)   updates_state=$(render_updates_indicator)   ;;
      volume)    volume_state=$(render_volume_indicator)     ;;
      dropbox)   dropbox_state=$(render_dropbox_indicator)   ;;
      seafile)   seafile_state=$(render_seafile_indicator)   ;;
      power)     power_state=$(render_power_indicator)       ;;
      user)      user_state=$(render_user_indicator)         ;;
      quit)      exit 0                                      ;;
    esac
  done | $OUTPUT
}


#
# Print usage
#

usage() {
  cat <<EOF
Usage: $(basename "$0") [OPTION]

Basic conky replacement that only updates on demand.

OPTIONS:
  -u, --update INDICATOR   update the specified indicator
  -k, --kill               send a kill signal to the status bar
  -h, --help               display this help and exit

INDICATORS:
  keyboard                 current keyboard layout indicator
  updates                  available updates indicator
  volume                   current volume indicator
  power                    power/battery indicator
  user                     current user indicator

EOF
}


#
# Argument parsing
#

main() {

  if [[ $# == 0 ]]; then
    run
  fi

  while [[ $# -gt 0 ]]; do

    case $1 in

      -u|--update)
        if [[ "$2" != "" ]]; then
          update "$2"
          shift
        else
          echo "expected argument after option $1" 1>&2
          exit 1
        fi
        ;;

      -k|--kill)
        update quit
        ;;

      -h|--help)
        usage
        exit 0
        ;;

      --*)
        echo "$(basename "$0"): invalid option $1" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

      -??*)
        set -- "-${1:1:1}" "-${1:2}" "${@:2}"
        continue
        ;;

      *)
        echo "$(basename "$0"): invalid option $1" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

    esac

    shift

  done
}

main "$@"
