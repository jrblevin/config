#!/bin/sh

# Compiz Fusion
compiz --replace &

# Set the background color
xsetroot -solid '#292827' &

# "Natural Scrolling"
xmodmap -e 'pointer = 1 2 3 5 4' &

# Start an xterm running screen
xterm -e 'screen -d -R' &

# Start window manager
xfce4-session
