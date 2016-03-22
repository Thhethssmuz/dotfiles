#!/bin/bash

[ -f ~/.xprofile ] && source ~/.xprofile

# Start the pulse audio daemon.
pulseaudio --start

# dropbox
dropbox

# Start the window manager
exec xmonad
