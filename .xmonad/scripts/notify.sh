#! /usr/bin/env bash


LOG=/tmp/notify.log
FIFO=/tmp/notify.fifo
OUTPUT=~/.xmonad/scripts/dzen-bar-middle.sh


###############################################################################
#
# Colorize multi-line input
#
# Arguments:
#   $1  foreground
#   $2  background
#
###############################################################################

colorize() {

  local head=""
  local tail=""

  if [ -n "$1" ]; then
    head+="^fg($1)"
    tail+="^fg()"
  fi

  if [ -n "$2" ]; then
    head+="^bg($2)"
    tail+="^bg()"
  fi

  while IFS= read line; do
    echo "$head$line$tail"
  done
}


###############################################################################
#
# Push notification to tray
#
# Arguments:
#   $1  duration
#
###############################################################################

tray() {

  # make fifo for pushing notifications to dzen, unless already open
  if [ ! -e $FIFO ]; then
    mkfifo $FIFO
    if [ $? == 0 ]; then
      ($OUTPUT < $FIFO; rm -f $FIFO) &
    fi
  fi

  # write to fifo
  (read line; echo $line; sleep $1) > $FIFO
}


###############################################################################
#
# Push notification to log
#
###############################################################################

log() {
  while IFS= read line; do
    echo $line >> $LOG
    ~/.xmonad/scripts/clock.sh --update notifications
  done
}


###############################################################################
#
# Push notification to its respective targets, i.e. sound, log and tray.
#
# Arguments:
#   $1  push_to_log
#   $2  push_to_tray
#   $3  duration
#   $4  play_sound
#   $5  sound_file
#
###############################################################################

notify() {

  if [ $4 == true ]; then
    paplay $5 &
  fi

  if [[ $1 == true && $2 == true ]]; then
    tee >(log) >(tray $3) > /dev/null
  elif [ $1 == true ]; then
    log
  elif [ $2 == true ]; then
    tray $3
  fi
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename $0) [OPTION]... [FILE]...

Display and log notifications.

OPTIONS:
  -t, --tray               display the notification in the tray
  -d, --duration SECONDS   the duration the notification will be displayed in
                           the tray (default is 2 seconds)
  -l, --log                push the notification to the log
  -s, --sound FILE         play notification sound

  -f, --foreground COLOR   set the foreground color (e.g.: #ff0000)
  -b, --background COLOR   set the background color (e.g.: #ff0000)

  -h, --help               display this help and exit

EOF
}


###############################################################################
#
# Argument parsing
#
###############################################################################

main() {

  # List of files specified as commandline arguments
  local files=()
  local file_counter=0

  local push_to_log=false

  local push_to_tray=false
  local duration=2

  # Colors of the text we want to output. Leave empty to use default.
  local fg_color=''
  local bg_color=''

  # Play notification sound
  local play_sound=false
  local sound_file=''


  while [[ $# > 0 ]]; do

    case $1 in

      -t|--tray)
        push_to_tray=true
        ;;

      -d|--duration)
        if [[ $2 != "" ]]; then
          duration=$2
          shift
        else
          echo "expected argument after option $1"
          exit 1
        fi
        ;;

      -l|--log)
        push_to_log=true
        ;;

      -s|--sound)
        if [[ $2 != "" ]]; then
          sound_file=$2
          play_sound=true
          shift
        else
          echo "expected argument after option $1"
          exit 1
        fi
        ;;

      -f|--foreground)
        if [[ $2 != "" ]]; then
          fg_color=$2
          shift
        else
          echo "expected argument after option $1"
          echo "note that color arguments must be quoted"
          exit 1
        fi
        ;;

      -b|--background)
        if [[ $2 != "" ]]; then
          bg_color=$2
          shift
        else
          echo "expected argument after option $1"
          echo "note that color arguments must be quoted"
          exit 1
        fi
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

      -?)
        echo "$(basename $0): invalid option $1"
        echo "Try $(basename $0) --help for more info"
        exit 1
        ;;

      *)
        files[file_counter++]=$1
        ;;

    esac

    shift

  done


  local args=( $push_to_log $push_to_tray $duration $play_sound $sound_file )

  if [ $file_counter -eq "0" ]; then
    # no files was specified through the commandline, read from stdin
    colorize "$fg_color" "$bg_color" | notify ${args[@]}
  else
    # read specified files
    cat ${files[@]} | colorize "$fg_color" "$bg_color" | notify ${args[@]}
  fi
}

main $@
