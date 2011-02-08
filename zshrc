#!/usr/bin/zsh
#
# Jason Blevins <jrblevin@sdf.org>
# Carrboro, November 16, 2008

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
export EDITOR="emacsclient -t"  # default editor
export ALTERNATE_EDITOR=""      # alternate editor

# aliases
if ls -F --color=auto >&/dev/null; then
  eval `dircolors -b`
  alias ls="ls --color=auto -F"
else
  alias ls="ls -F"
fi
alias grep='grep --color=auto'
alias ll='ls -l'
alias ee="emacs -nw"
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'

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

# less input preprocessor
eval `lessfile`

# tab completion
autoload -U compinit
compinit

# colorful completion listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Special characters that are to be considered part of words.
# Default: WORDCHARS="*?_-.[]~=&;!#$%^(){}<>/"
WORDCHARS="*?[]~&;!%^(){}<>"

# current Git branch
git_branch() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo " :${ref#refs/heads/}"
}

# prompt
autoload -U colors
colors
setopt prompt_subst
PROMPT='%{$fg[green]%}%m %{$fg[blue]%}%~%{$fg[yellow]%}$(git_branch) %{$reset_color%}%% '

# Ruby
export PATH=/var/lib/gems/1.8/bin:$PATH
export RUBYLIB=$HOME/lib/ruby:/usr/local/lib/site_ruby/1.8:/usr/lib/ruby/1.8

# mpd
export MPD_HOST=127.0.0.1
export MPD_PORT=6600

# GFortran
export PATH=/opt/gcc-trunk/bin:${PATH}

# Architecture-specific settings
ARCH=`uname -m`
if [[ ${ARCH} == "x86_64" ]]; then
    LIB64="64"
    ICS_ARCH="intel64"
else
    ICS_ARCH="ia32"
fi

# Intel Compilers
export PATH=/opt/intel/bin:${PATH}

# Library path
if [ -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH="/usr/local/lib:/opt/gcc-trunk/lib${LIB64}:/opt/intel/lib/${ICS_ARCH}:/opt/intel/mkl/lib/${ICS_ARCH}"
else
  LD_LIBRARY_PATH="/usr/local/lib:/opt/gcc-trunk/lib${LIB64}:/opt/intel/lib/${ICS_ARCH}:/opt/intel/mkl/lib/${ICS_ARCH}:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH

# Anything in ~/bin has priority
export PATH=${HOME}/bin:${PATH}

# Set the xterm title
if [[ $TERM == "xterm" ]]; then
    print -Pn "\e]2;$USER@$HOST\a"
fi
