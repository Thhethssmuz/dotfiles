#!/usr/bin/env bash

. ~/.xmonad/scripts/config.sh


FIFO=/tmp/clock.fifo
PID_FILE=/tmp/clock.pid
OUTPUT=~/.xmonad/scripts/dzen-bar-middle.sh


###############################################################################
#
# Render individual status indicators
#
###############################################################################

render_clock_indicator() {
  # echo -n "^ca(1, /home/thhethssmuz/.xmonad/scripts/dzen_date.sh)"
  echo -n $(date '+%a %b %d, %H:%M:%S')
  # echo -n "^ca()"
}

render_notifications_indicator() {
  local n=$([ -e /tmp/notify.log ] && (wc -l < /tmp/notify.log) || echo "0" )

  if [ $n -gt "0" ]; then
    # echo -n "^ca(1, somestript)"
    echo -n "^fg($BACKGROUND)^bg($COLOR1)"
    echo -n " $n "
    echo -n "^bg()^fg()"
    # echo -n "^ca()"
  fi
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

  local clock_state=$(render_clock_indicator)
  local notifications_state=$(render_notifications_indicator)

  render_all_indicators() {
    echo "$clock_state $notifications_state"
  }

  trap stop EXIT

  echo > $PID_FILE
  if [ ! -e $FIFO ]; then
    mkfifo $FIFO
  fi

  set_update_interval clock 1

  while true; do
    render_all_indicators
    read line <$FIFO
    case $line in
      clock)         clock_state=$(render_clock_indicator)                 ;;
      notifications) notifications_state=$(render_notifications_indicator) ;;
      quit)          exit 0                                                ;;
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
  clock
  notifications            new notifications

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
