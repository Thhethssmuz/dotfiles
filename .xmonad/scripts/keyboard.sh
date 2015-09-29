#!/usr/bin/env bash


# list of my used keyboard layouts, ordering used for layout switching
MY_LAYOUTS=( 'no(dvorak)' 'no' 'us(dvorak)' 'us' )

# prettified layout codes that will show up in the status bar
declare -A MY_LAYOUT_CODES=(
  ['no(dvorak)']='no¹'
  ['no']='no²'
  ['us(dvorak)']='us¹'
  ['us']='us²'
)

# prettified layout names that will show up in the layout list
declare -A MY_LAYOUT_NAMES=(
  ['no(dvorak)']='Norwegian Dvorak'
  ['no']='Norwegian'
  ['us(dvorak)']='US International Dvorak'
  ['us']='US International'
)


###############################################################################
#
# Get the xkb code for the current active keyboard layout
#
###############################################################################

get_xkb_layout_code() {
  setxkbmap -print -v 10 | grep 'layout:' | awk '{print $2}'
}


###############################################################################
#
# Get a prettified code and name for the current active layout
#
###############################################################################

get_code() {
  local xkbcode=$(get_xkb_layout_code)
  echo ${MY_LAYOUT_CODES[$xkbcode]}
}

get_name() {
  local xkbcode=$(get_xkb_layout_code)
  echo ${MY_LAYOUT_NAMES[$xkbcode]}
}


###############################################################################
#
# List all my layouts
#
###############################################################################

list() {
  for i in ${MY_LAYOUTS[@]}; do
    echo ${MY_LAYOUT_NAMES[$i]}
  done
}


###############################################################################
#
# Swap between my layouts
#
###############################################################################

next() {
  local current=$(get_xkb_layout_code)
  local i

  for i in ${!MY_LAYOUTS[@]}; do
    if [ ${MY_LAYOUTS[$i]} == $current ]; then
      break
    fi
  done

  if [ $i == $((${#MY_LAYOUTS[@]} - 1)) ]; then
    i=0
  else
    ((i++))
  fi

  setxkbmap ${MY_LAYOUTS[$i]}

  ~/.xmonad/scripts/status-bar.sh --update keyboard
}

previous() {
  local current=$(get_xkb_layout_code)
  local i

  for i in ${!MY_LAYOUTS[@]}; do
    if [ ${MY_LAYOUTS[$i]} == $current ]; then
      break
    fi
  done

  if [ $i == 0 ]; then
    i=$((${#MY_LAYOUTS[@]} - 1))
  else
    ((i--))
  fi

  setxkbmap ${MY_LAYOUTS[$i]}

  ~/.xmonad/scripts/status-bar.sh --update keyboard
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename $0) OPTION

Keyboard layout switcher.

OPTIONS:
  -n, --next               change to next layout
  -p, --prev               change to previous layout
      --set XKB_CODE       set layout

      --code               print current layout code
      --name               print current layout name
      --list               print my list of layouts

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

      -n|--next)
        next
        ;;

      -p|--prev)
        previous
        ;;

      --code)
        get_code
        ;;

      --name)
        get_name
        ;;

      --list)
        list
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
