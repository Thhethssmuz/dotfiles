#!/bin/bash

[ -f ~/.xprofile ] && source ~/.xprofile

# Start the pulse audio daemon.
pulseaudio --start

# dropbox
hash dropbox 2>/dev/null && dropbox

# Start the window manager
exec xmonad
