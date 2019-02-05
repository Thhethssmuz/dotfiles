#!/usr/bin/env bash


#
# List all available updates
#

list() {
  make --no-print-directory -C ~/.install check-updates
}

group() {
  local updates pac aur npm
  updates=$(list)
  pac=$(grep -c '^pac' <<< "$updates")
  aur=$(grep -c '^aur' <<< "$updates")
  npm=$(grep -c '^npm' <<< "$updates")
  echo "$pac $aur $npm"
}

count() {
  list | wc -l
}


#
# Print usage
#

usage() {
  cat <<EOF
Usage: $(basename "$0") OPTION

List available updates.

OPTIONS:
  -h, --help               show this help and exit.
  -l, --list               get a list of all available updates
  -g, --group              group by package types

EOF
}


#
# Argument parsing
#

main() {

  local list=false
  local group=false

  while [[ $# -gt 0 ]]; do

    case $1 in

      -l|--list)
        list=true
        ;;

      -g|--group)
        group=true
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

      -*)
        echo "$(basename "$0"): invalid option \`$1'" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

      *)
        echo "$(basename "$0"): invalid argument $1" 1>&2
        echo "Try $(basename "$0") --help for more info" 1>&2
        exit 1
        ;;

    esac

    shift

  done


  if [ $list == true ]; then
    list
  elif [ $group == true ]; then
    group
  else
    count
  fi
}

main "$@"
