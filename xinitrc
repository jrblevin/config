#!/bin/sh

# Load local modmap
test -r $HOME/.Xmodmap && xmodmap $HOME/.Xmodmap

# Load .Xresources
test -r $HOME/.Xresources && xrdb -load .Xresources

# Start ssh-agent
eval `ssh-agent -s`

# Set the background color
xsetroot -solid Black

# Start an xterm
xterm &

# Start awesome
exec awesome

# Terminate ssh-agent
eval `ssh-agent -k`
