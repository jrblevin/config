#!/bin/sh

# Compiz Fusion
compiz --replace &

# Set the background color
xsetroot -solid '#111111' &

# Start an xterm running screen
xterm -e 'screen -d -R' &

# Start window manager
xfce4-session
