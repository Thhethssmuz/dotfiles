#!/bin/bash

ESPEED=370
ELANG=en
TLANG=eng

speek() {
  local selected
  selected="$(xsel | tr '\n' ' ' | tr -dc '[:print:][æøåÆØÅ]' && echo)"

  if [ -z "$selected" ]; then
    selected="$(scrot -s - 2>/dev/null | tesseract -l "$TLANG" - - 2>/dev/null)"
  fi

  espeak --stdin -s "$ESPEED" -v "$ELANG" <<< "$selected "
}

stop() {
  pkill -9 espeak
}

set-language() {
  case "$1" in

    en)
      ELANG=en
      TLANG=eng
      ;;

    no)
      ELANG=nb
      TLANG=nor
      ;;

    *)
      echo "$(basename "$0"): invalid option \`$1'" 1>&2
      echo "Try $(basename "$0") --help for more info" 1>&2
      exit 1
      ;;

  esac
}

usage() {
  cat <<EOF
Usage:
  $(basename "$0") (-h | --help)
  $(basename "$0") [-l <language>] <cmd>

COMMAND
  speek
  stop

OPTIONS:
  -l, --lang <language>    select language, i.e. "en" or "no"
  -h, --help               display this help and exit

EOF
}

main() {
  local cmd

  while [[ $# -gt 0 ]]; do

    case "$1" in

      --lang=*)
        set-language "${1#*=}"
        ;;
      -l|--lang)
        shift
        set-language "$1"
        ;;
      -l*)
        set-language "${1:2}"
        ;;

      -h|--help)
        usage
        exit 0
        ;;

      --*)
        echo "$(basename "$0"): invalid option \`$1'" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

      -??*)
        set -- "-${1:1:1}" "-${1:2}" "${@:2}"
        continue
        ;;

      -*)
        echo "$(basename "$0"): invalid option \`$1'" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

      speek)
        cmd=speek
        ;;

      stop)
        cmd=stop
        ;;

      *)
        echo "$(basename "$0"): invalid command \`$1'" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

    esac

    shift

  done

  if [ -z "$cmd" ]; then
    echo "$(basename "$0"): missing command"
    echo "Try $(basename "$0") --help for more info"
    exit 1
  fi

  "$cmd"
}

main "$@"
