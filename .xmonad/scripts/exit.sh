#!/usr/bin/env bash


###############################################################################
#
# Attempt to gracefully close all applications, so to not have applications nag
# about not being closed properly on next start up.
#
# ...I'm looking at you chromium.
#
###############################################################################

closeall() {

  pkill --oldest chromium
  pkill --oldest sublime_text
  sleep 2

  for win in $(wmctrl -l | awk '{print $1}'); do
    wmctrl -i -c $win
    sleep 0.25
  done
}


###############################################################################
#
# Close all applications and restart the machine.
#
###############################################################################

restart() {
  closeall
  systemctl restart
}


###############################################################################
#
# Close all applications and power down.
#
###############################################################################

poweroff() {
  closeall
  systemctl poweroff
}


$1
