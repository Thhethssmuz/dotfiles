#!/usr/bin/env bash


###############################################################################
#
# Attempt to gracefully close all applications, so to not have applications nag
# about not being closed properly on next start up.
#
# ...I'm looking at you chromium.
#
###############################################################################

close() {

  pkill --oldest chromium
  pkill --oldest sublime_text
  sleep 2

  for win in $(wmctrl -l | awk '{print $1}'); do
    wmctrl -i -c "$win"
    sleep 0.25
  done
}


###############################################################################
#
# system commands
#
###############################################################################

restart() {
  close
  systemctl restart
}

poweroff() {
  close
  # systemctl poweroff
  # shutdown # needs sudo (or explicitly allow to be run without)
  # poweroff
}

lock() {
  if hash slock 2>/dev/null; then
    slock
  elif hash slimlock 2>/dev/null; then
    slimlock
  elif hash gnome-screensaver-command 2>/dev/null; then
    gnome-screensaver-command --lock
  fi
}

logout() {
  close
  hdotool key 'shift+alt+F12'
}

rexmonad() {
  ghc --make ~/.xmonad/xmonad.hs -threaded -i"$HOME/.xmonad/lib" -dynamic \
    -fforce-recomp -o ~/.xmonad/xmonad-x86_64-linux && xmonad --restart
}


###############################################################################
#
# Print usage
#
###############################################################################

usage() {
  cat <<EOF
Usage: $(basename "$0") OPTION

System control.

OPTIONS:
      --lock               enable lockscreen
      --logout
      --restart
      --poweroff
      --rexmonad           recompile and restart xmonad

  -h, --help               display this help and exit

EOF
}


###############################################################################
#
# Argument parsing
#
###############################################################################

main() {

  local cmd

  while [[ $# -gt 0 ]]; do

    case $1 in

      --lock)     cmd="lock"    ;;
      --logout)   cmd="logout"  ;;
      --restart)  cmd="restart" ;;
      --poweroff) cmd="poweroff";;
      --rexmonad) cmd="rexmonad";;

      -h|--help)
        usage
        exit 0
        ;;

      --*)
        echo "$(basename "$0"): invalid option $1"
        echo "Try $(basename "$0") --help for more info"
        exit 1
        ;;

      -??*)
        set -- "${1:0:2}" "-${1:2}" "${@:2}"
        continue
        ;;

      *)
        echo "$(basename "$0"): invalid option $1"
        echo "Try $(basename "$0") --help for more info"
        exit 1
        ;;

    esac

    shift

  done

  if [[ $cmd != "" ]]; then
    $cmd
  else
    usage
    exit 0
  fi
}

main "$@"
