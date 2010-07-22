#!/bin/sh

# Load local modmap
test -r $HOME/.Xmodmap && xmodmap $HOME/.Xmodmap

# Load .Xresources
test -r $HOME/.Xresources && xrdb -load .Xresources

# Start ssh-agent
eval `ssh-agent -s`

# Set the background color
xsetroot -solid '#111111'

# Start an xterm running screen
xterm -e 'screen -d -R' &

# Start twm
awesome

# Terminate ssh-agent
eval `ssh-agent -k`
