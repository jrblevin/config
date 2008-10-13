#!/bin/sh

# Load local modmap
test -r $HOME/.Xmodmap && xmodmap $HOME/.Xmodmap

# Load .Xresources
test -r $HOME/.Xresources && xrdb -load .Xresources

# Start ssh-agent
eval `ssh-agent -s`

# Set the background color
xsetroot -solid Black

# Start Emacs and an xterm running screen
emacs &
xterm -e screen -R &

# Start awesome
awesome

# Terminate ssh-agent
eval `ssh-agent -k`
