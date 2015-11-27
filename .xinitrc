xrdb ~/.Xresources

###############################################################################
#
# Keyboard / Mouse
#
###############################################################################

# Set numlock status to on at start.
numlockx &

# Explicitly set keyboard layout with `setxkbmap` to work around glitch with
# `xdotool` that assumes my keyboard is querty :/
setxkbmap "no(dvorak)"

# Hide mouse cursor when inactive for a short period.
#unclutter -grab -noevents -root &

# Set mouse sensitivity
xinput \
  set-prop \
  'pointer:La-VIEW Technology Mionix NAOS 8200      ' \
  'Coordinate Transformation Matrix' \
  1.5, 0, 0, 0, 1.5, 0, 0, 0, 1


###############################################################################
#
# Desktop
#
###############################################################################

# Set background image.
hsetroot -fill ~/Pictures/wallpaper.jpg &

# Enable window transparency and effects.
compton &

# Dim brightness and screen colours at night.
redshift &

# Turn off bell sound
xset b off

# Turn off screen saver
xset s off

# Blank the screen after 25 minutes of inactivity.
xset dpms 1500 1500 1500

# Lock screen after 30
xautolock -locker "~/.xmonad/scripts/system.sh --lock" -time 30 &


###############################################################################
#
# Other
#
###############################################################################

# Start the pulse audio daemon.
pulseaudio --start

# dropbox
dropbox

# Start status bars
~/.xmonad/scripts/status-bar.sh &


###############################################################################
#
# Main
#
###############################################################################

# Start the window manager
exec xmonad
