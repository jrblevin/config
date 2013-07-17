#!/usr/bin/zsh
#
# Jason Blevins <jrblevin@sdf.org>
# Carrboro, November 16, 2008
# Last Modified: March 11, 2013 12:16 EDT

### System-Specific Configuration

# Architecture-specific settings
ARCH=`uname -m`
if [[ ${ARCH} == "x86_64" ]]; then
    LIB64="64"
    ICS_ARCH="intel64"
else
    ICS_ARCH="ia32"
fi

# Operating-system-specific settings
OS=`uname -s`
if [[ $OS == "Darwin" ]]; then
    # MacPorts
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export MANPATH=/opt/local/share/man:$MANPATH
    export DISPLAY=:0.0
    # Color ls
    export CLICOLOR=1
    # Use Spotlight database for locate
    function locate { mdfind "kMDItemDisplayName == '$@'wc"; }
    # Completion dump file
    ZCOMPDUMP=$HOME/.zcompdump.osx
elif [[ $OS == "Linux" ]]; then
    # less input preprocessor
    eval `lessfile`
    ZCOMPDUMP=$HOME/.zcompdump.linux
    # Intel Compilers
    if [ -f /opt/intel/bin/compilervars.sh ]; then
        source /opt/intel/bin/compilervars.sh ${ICS_ARCH}
    fi
fi

# Host-specific settings
FQDN=`hostname -f`
HOST=`hostname -s`
if [[ $FQDN =~ "econ.ohio-state.edu" ]]; then
    # mpd
    export MPD_HOST=localhost
    export MPD_PORT=6600
else
    # mpd
    export MPD_HOST=192.168.1.2
    export MPD_PORT=6600
fi


### Basic ZSH Configuration

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# Emacs keybindings (-v for vi)
bindkey -e

# settings
setopt autocd                   # cd by typing directory name
setopt hist_ignore_dups         # ignore repeated commands
setopt nobeep                   # stop yelling at me

# set common environment variables
export EDITOR="emacsclient -a vi" # default editor

# tab completion
autoload -U compinit
compinit -d $ZCOMPDUMP

# zmv
autoload zmv

# colorful completion listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Special characters that are to be considered part of words.
# Default: WORDCHARS="*?_-.[]~=&;!#$%^(){}<>/"
WORDCHARS="*?[]~&;!%^(){}<>"


### Useful Commands and Aliases

if ls -F --color=auto >&/dev/null; then
  eval `dircolors -b`
  alias ls="ls --color=auto -F"
else
  alias ls="ls -F"
fi
alias grep='grep --color=auto'
alias make='make -j'
alias ll='ls -l'
alias ee="emacs -nw"
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias gd='git diff'
alias gdc='git diff --cached'
alias grl='git rlog'
alias gst='git status'
alias gci='git commit'

# duf: human readable, sorted disk usage
# http://www.earthinfo.org/linux-disk-usage-sorted-by-size-and-human-readable/
alias duf='du -sk * | sort -nr | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\'

# Suffix Aliases
alias -s c=vim
alias -s f90=vim
alias -s txt=vim
alias -s tex=vim
alias -s text=vim
alias -s html=elinks
alias -s com=elinks
alias -s net=elinks
alias -s org=elinks

### Prompt

autoload -U colors
colors
setopt prompt_subst

# Configuration
DEFAULT_USER='jblevins'

# Context: user@hostname
prompt_context() {
  local user=`whoami`
  local context="%m"
  if [[ $UID -ne 0 && "$user" != "$DEFAULT_USER" ]]; then
    context="$user$context"
  fi
  echo -n "$context"
}

# Git branch and status
prompt_git() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  branch="${ref/refs\/heads\//}$dirty"
  echo -n "%{$fg[yellow]%} :$branch"
  # if [[ -n $(git status -s 2> /dev/null) ]]; then
  #   echo -n "%{$fg[red]%} :$branch"
  # else
  #   echo -n "%{$fg[yellow]%} :$branch"
  # fi
}

# Prompt symbol
prompt_symbol() {
  if [[ $UID -ne 0 ]]; then
      echo -n "%"
  else
      echo -n "#"
  fi
}

PROMPT='%{$fg[green]%}$(prompt_context) %{$fg[blue]%}%~$(prompt_git) %{$reset_color%}%$(prompt_symbol) '


### Specific Programs

# Ruby
export PATH=/var/lib/gems/1.8/bin:$PATH
export RUBYLIB=$HOME/lib/ruby:/usr/local/lib/site_ruby/1.8:/usr/lib/ruby/1.8

# GFortran
export PATH=/opt/gcc-trunk/bin:${PATH}

# Open MPI
export PATH=/opt/openmpi/bin:${PATH}


### Paths

# Library path
if [ -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH="/usr/local/lib:/opt/gcc-trunk/lib${LIB64}:/opt/openmpi/lib:"
else
  LD_LIBRARY_PATH="/usr/local/lib:/opt/gcc-trunk/lib${LIB64}:/opt/openmpi/lib:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH

# Anything in ~/bin has priority
export PATH=${HOME}/bin:${PATH}

# Set the xterm title
if [[ $TERM == "xterm" ]]; then
    print -Pn "\e]2;$USER@$HOST\a"
fi


### SSH

# SSH Agent (http://www.cygwin.com/ml/cygwin/2001-06/msg00537.html)
SSH_ENV="$HOME/.ssh-agent.$HOST"

function start_agent {
     echo -n "Initializing SSH agent..."
     ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
     echo " OK"
     chmod 600 "${SSH_ENV}"
     . "${SSH_ENV}" > /dev/null
     /usr/bin/ssh-add;
}

# Source SSH settings
if [ -f "${SSH_ENV}" ]; then
     . "${SSH_ENV}" > /dev/null
     #ps ${SSH_AGENT_PID} doesn't work under cywgin
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
         start_agent;
     }
else
     start_agent;
fi


### Windowing System

# Automatically start X
if [ -z "$DISPLAY" ] && [[ $(tty) == "/dev/tty1" ]]; then
   startx
fi
