## ~/.bashrc                                      -*-shell-script-*-
#
# Jason Blevins <jblevins@xbeta.org>
#
# Created: Raleigh, June 4, 2004
# Last Modified: June 10, 2010 14:58 EDT

# If not running interactively, don't do anything.
[ -z "$PS1" ] && return

# Don't put duplicate lines in the history (needs bash version 3).
export HISTCONTROL=erasedups

# Prevent concurrent bash sessions from overwriting the history.
shopt -s histappend

# Check the window size after each command and update if necessary.
shopt -s checkwinsize

# xterm titlebar
if [[ $TERM == "xterm" ]]; then
    echo -ne "\033]0;$USER@$HOSTNAME\007"
fi

# Set the prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Provide a more convenient PATH
export PATH=${HOME}/bin:/usr/local/bin:${PATH}

# Enable color output of some common commands
if [ -x /usr/bin/dircolors ]; then
    eval `dircolors -b`
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
fi

# Tab completion.
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# SSH hostname completion.
SSH_COMPLETE=( $(cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | egrep -v [0123456789]) )
complete -o default -W "${SSH_COMPLETE[*]}" ssh

# Editor setup
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -t"
export ALTERNATE_EDITOR='vi'
