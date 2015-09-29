#!/usr/bin/env bash

. ~/.xmonad/scripts/config.sh


OUTPUT_DIR=~/Screenshots


###############################################################################
#
# Take a screenshot
#
# Arguments
#   $1  select
#
###############################################################################

screenshot() {

  if [ ! -e $OUTPUT_DIR ]; then
    mkdir $OUTPUT_DIR
  fi

  local file="$OUTPUT_DIR/$(date --utc '+%Y-%m-%d %H:%M:%S').png"
  scrot $1 "$file"

  echo -e "^fn(Font Awesome:size=$FONT_SIZE)\uf030^fn() $file" | \
    ~/.xmonad/scripts/notify.sh \
      -ts /usr/share/sounds/freedesktop/stereo/screen-capture.oga
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename $0) OPTION

Small wrapper for scrot.

OPTIONS:
  -s, --select             interactively choose a window or rectangle with
                           the mouse

  -h, --help               display this help and exit

EOF
}


###############################################################################
#
# Argument parsing
#
###############################################################################

main() {
  local selection=""

  while [[ $# > 0 ]]; do

    case $1 in

      -s|--select)
        selection="-s"
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

  screenshot $selection
}

main $@
