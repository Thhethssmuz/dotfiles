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

restart_xmonad() {
  # shellcheck disable=SC2009
  if ps aux | grep "$HOME/.xmonad/scripts/status-bar.sh" | grep -v grep >/dev/null; then
    ps aux | \
      grep "$HOME/.xmonad/scripts/status-bar.sh" | \
      grep -v grep | \
      awk '{print $2}' | \
      xargs kill
  fi
  xmonad --restart
}
compile_xmonad() {
  ghc --make ~/.xmonad/xmonad.hs -threaded -i"$HOME/.xmonad/lib" -dynamic \
    -fforce-recomp -o ~/.xmonad/xmonad-x86_64-linux
}
rexmonad() {
  compile_xmonad && restart_xmonad
}

rescreen() {
  SCREENS="$(while read -r output hex; do
    printf "%s" "$output $(xxd -r -p <<< "$hex");"
  done < <(xrandr -q --verbose | awk '
    !/^[ \t]/ {if (output && hex) print output, hex;output=$1;hex=""}
    /[:.]/ && h {sub(/.*000000f[ce]00/, "", hex);hex=substr(hex, 0, 26) "0a";sub(/0a.*/, "", hex);h=0}
    h {sub(/[ \t]+/, ""); hex=hex $0}
    /EDID.*:/ {h=1}
    END {if (output && hex) print output, hex}'))"

  echo "$SCREENS"
  case "$SCREENS" in
    'eDP1 N140HCR-GA2;DP2 Dell U4919DW;')
      xrandr --output DP2 --mode 5120x1440 --primary
      xrandr --fb 5120x1440
      xrandr --setmonitor DP2-V1 2560/0x1440/1+0+0 DP2
      xrandr --setmonitor DP2-V2 2560/1x1440/1+2560+0 none
      restart_xmonad
      "$HOME"/.xmonad/scripts/keyboard.sh --set 'no(dvorak)'
      ;;

    'eDP1 N140HCR-GA2;')
      xrandr --delmonitor DP2-V2
      xrandr --delmonitor DP2-V1
      xrandr --fb 1920x1080
      restart_xmonad
      ;;

    'DisplayPort-0 BenQ XL2720T;DisplayPort-1 BenQ GW2760;HDMI-A-1 BenQ GW2760;')
      ;;

    *) echo 'unknown screen layout' ;;
  esac
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
      --rescreen           reset sreen layout
      --rex                restart xmonad
      --xmonad             recomplie xmonad
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

      --lock)     cmd="lock";;
      --logout)   cmd="logout";;
      --restart)  cmd="restart";;
      --poweroff) cmd="poweroff";;
      --rescreen) cmd="rescreen";;
      --rex)      cmd="restart_xmonad";;
      --xmonad)   cmd="compile_xmonad";;
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
