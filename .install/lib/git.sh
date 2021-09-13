#!/bin/bash
set -euo pipefail

: "${USERNAME:=$USER}"
: "${USERHOME:=$(sudo -Hu "$USERNAME" -s -- echo '$HOME')}"


list() {
  find "$USERHOME" -maxdepth 1 -type d | while read -r line; do (
    cd "$line" 2>/dev/null || exit 0
    [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == true ]] || exit 0
    [[ $(git rev-parse --show-toplevel) == "$line" ]] || exit 0

    if [ "$line" == "$USERHOME" ]; then
      echo "dotfiles.git"
    else
      basename "$line.git"
    fi
  ) done
}

ignore() {
  :
}


check-updates() {
  :
}

update() {
  :
}


install() {
  :
}

explicit() {
  :
}

remove() {
  :
}


prompt() {
  local dir cmd prompt

  if [ "${1%.git}" == "dotfiles" ]; then
    dir="$USERHOME"
  else
    dir="$USERHOME/${1%.git}"
  fi

  cmd="cd \"$dir\" && . ~/.prompt && space=' ' __promptline_git"

  if prompt="$(sudo -u "$USERNAME" bash <<< "$cmd")"; then
    echo "$1//error/$prompt"
  elif [ "$prompt" != " master ✔" ]; then
    echo "$1//success/$prompt"
  fi
}

status() {
  comm --output-delimiter=/ <(list | sort -u) <(sort -u) | \
    comm -13 <(ignore | sort -u) - | \
    while read -r line; do
      case "$line" in
        /)   ;;
        //*) prompt "${line:2}" ;;
        /*)  echo "${line:1}//error/Not installed" ;;
        *)   echo "${line}//warn/Unprovisioned"; prompt "${line}" ;;
      esac
    done
}

"$@"
