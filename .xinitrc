#!/bin/bash

[ -f ~/.xprofile ] && source ~/.xprofile

# Start the window manager
exec xmonad
