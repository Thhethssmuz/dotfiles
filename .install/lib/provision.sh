#!/bin/bash
set -euo pipefail

TYPES="pac aur npm  cfg fil  mod srv soc usr usc mnt amt  git"
DIR=$(realpath "$(dirname "$0")")


forall() {
  for type in $TYPES; do
    "$DIR/$type.sh" "$@"
  done
}

teeall() {
  local tmp tmp2 order
  tmp="$(cat)"
  tmp2="$( (grep '\.src$' || true) <<< "$tmp")"

  declare -A hooks
  for target in $tmp2; do
    order="$("$DIR/../custom/${target%.src}.sh" "order")"
    hooks[$order]+="$(echo "${hooks[$order]+}" && echo "$target")"
  done

  for type in $TYPES; do
    tmp2="$( (grep '\.'"$type"'$' || true) <<< "$tmp")"

    if [ "$1" != "status" ]; then
      while read -r line; do
        if [ -f "$DIR/../hooks/$line.pre-$1" ]; then
          echo "running pre-$1 hook for $line..."
          "$DIR/../hooks/$line.pre-$1"
        fi
      done <<< "$tmp2"
    fi

    test "${hooks["pre-$type"]+isDefined}" && \
      "$DIR/src.sh" "$@" <<< "${hooks["pre-$type"]}"

    "$DIR/$type.sh" "$@" <<< "$tmp2"

    test "${hooks["post-$type"]+isDefined}" && \
      "$DIR/src.sh" "$@" <<< "${hooks["post-$type"]}"

    if [ "$1" != "status" ]; then
      while read -r line; do
        if [ -f "$DIR/../hooks/$line.post-$1" ]; then
          echo "running post-$1 hook for $line..."
          "$DIR/../hooks/$line.post-$1"
        fi
      done <<< "$tmp"
    fi
  done
}

filter() {
  teeall status | while IFS=$'\x1e' read -r entity targets _ _; do
    if [[ "$targets" == *"$1"* ]]; then
      echo "$entity"
    fi
  done
}


check-updates() {
  forall check-updates | sort -k2
}

update() {
  forall update
}


install() {
  filter install | teeall install
}

explicit() {
  filter explicit | teeall explicit
}

remove-orphans() {
  filter remove-orphans | teeall remove
}


status() {
  teeall status | while IFS=$'\x1e' read -r entity _ level message; do
    case "$level" in
      success) echo -e "$entity \\e[1;32m$message\\e[0m" ;;
      info)    echo -e "$entity \\e[1;35m$message\\e[0m" ;;
      warn)    echo -e "$entity \\e[1;33m$message\\e[0m" ;;
      error)   echo -e "$entity \\e[1;31m$message\\e[0m" ;;
      *)       echo -e "$entity $message" ;;
    esac
  done | sort -t. -k1,1
}

"$@"
