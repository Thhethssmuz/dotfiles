#!/usr/bin/env bash


###############################################################################
#
# List all available updates
#
###############################################################################

list() {
  if hash checkupdates 2>/dev/null; then
    checkupdates
  elif hash apt-show-versions 2>/dev/null; then
    apt-show-versions -u | cut -d':' -f1
  fi
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename $0) OPTION

Update information.

OPTIONS:
  -l, --list               get a list of all available updates

  -h, --help               display this help and exit

EOF
}


###############################################################################
#
# Argument parsing
#
###############################################################################

main() {

  local list=false

  while [[ $# > 0 ]]; do

    case $1 in

      -l|--list)
        list=true
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


  if [ $list == true ]; then
    list
  else
    list | wc -l
  fi
}

main $@
