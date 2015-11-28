#!/usr/bin/env bash

. ~/.xmonad/scripts/config.sh


FIFO=/tmp/status-bar.fifo
PID_FILE=/tmp/status-bar.pid
OUTPUT=~/.xmonad/scripts/dzen-bar-right.sh


###############################################################################
#
# Render individual status indicators
#
###############################################################################

render_keyboard_indicator() {
  # echo -n  "^ca(1, exec ~/.xmonad/scripts/dzen-menu-keyboard.sh)"
  echo -n  "^ca(4, exec ~/.xmonad/scripts/keyboard.sh --prev)"
  echo -n  "^ca(5, exec ~/.xmonad/scripts/keyboard.sh --next)"
  echo -ne "^fn(Font Awesome:size=$FONT_SIZE)\uf11c^fn() "
  echo -n  $(~/.xmonad/scripts/keyboard.sh --code)
  echo -n  "^ca()"
  echo -n  "^ca()"
}

render_updates_indicator() {
  local icon="^i($HOME/.xmonad/icons/pacman.xpm)"
  local n=$(~/.xmonad/scripts/updates.sh)
  echo -n "$icon $n"
}

render_volume_indicator() {
  echo -n "^ca(1, exec ~/.xmonad/scripts/volume.sh -m)"
  echo -n "^ca(4, exec ~/.xmonad/scripts/volume.sh -i)"
  echo -n "^ca(5, exec ~/.xmonad/scripts/volume.sh -d)"
  echo -n "$(~/.xmonad/scripts/notif-icon.sh $(~/.xmonad/scripts/volume.sh --icon)) "
  echo -n "$(~/.xmonad/scripts/volume.sh --level)%"
  echo -n "^ca()^ca()^ca()"
}

render_redshift_indicator() {
  echo "lol"
}

render_user_indicator() {
  echo -n "$(whoami)"
  echo -n " "
}


###############################################################################
#
# Update a status indicator at a given interval
#
# Arguments:
#   $1  indicator name
#   $2  interval in seconds
#
###############################################################################

set_update_interval() {
  while true; do
    sleep $2
    echo $1 >> $FIFO
  done &
  echo $! >> $PID_FILE
  disown
}


###############################################################################
#
# Send an update signal to the status bar for a specific indicator
#
# Arguments:
#   $1  indicator name
#
###############################################################################

update() {
  echo $1 >> $FIFO
}


###############################################################################
#
# Kill the status bar
#
###############################################################################

stop() {
  xargs kill < $PID_FILE
  rm -f $FIFO $PID_FILE
}


###############################################################################
#
# Start the status bar
#
###############################################################################

run() {

  local keyboard_state=$(render_keyboard_indicator)
  local updates_state=$(render_updates_indicator)
  local volume_state=$(render_volume_indicator)
  local user_state=$(render_user_indicator)

  render_all_indicators() {
    echo "$keyboard_state  $updates_state  $volume_state  $user_state"
  }

  trap stop EXIT

  echo > $PID_FILE
  if [ ! -e $FIFO ]; then
    mkfifo $FIFO
  fi

  set_update_interval updates 900

  while true; do
    render_all_indicators
    read line <$FIFO
    case $line in
      keyboard)  keyboard_state=$(render_keyboard_indicator) ;;
      updates)   updates_state=$(render_updates_indicator)   ;;
      volume)    volume_state=$(render_volume_indicator)     ;;
      user)      user_state=$(render_user_indicator)         ;;
      quit)      exit 0                                      ;;
    esac
  done | $OUTPUT
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename $0) [OPTION]

Basic conky replacement that only updates on demand.

OPTIONS:
  -u, --update INDICATOR   update the specified indicator
  -k, --kill               send a kill signal to the status bar
  -h, --help               display this help and exit

INDICATORS:
  keyboard                 current keyboard layout indicator
  updates                  available updates indicator
  volume                   current volume indicator
  user                     current user indicator

EOF
}


###############################################################################
#
# Argument parsing
#
###############################################################################

main() {

  if [[ $# == 0 ]]; then
    run
  fi

  while [[ $# > 0 ]]; do

    case $1 in

      -u|--update)
        if [[ $2 != "" ]]; then
          update $2
          shift
        else
          echo "expected argument after option $1"
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
