#!/usr/bin/zsh
#
# Jason Blevins <jrblevin@sdf.lonestar.org>
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

# programs
export EDITOR=vi                # default editor

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
    ref=$(git-symbolic-ref HEAD 2> /dev/null) || return
    echo " :${ref#refs/heads/}"
}

# prompt
autoload -U colors
colors
setopt prompt_subst
PROMPT='%{$fg[green]%}%m %{$fg[blue]%}%c%{$fg[yellow]%}$(git_branch)%{$reset_color%} %% '

# Provide a more convenient PATH
export PATH=${HOME}/bin:${PATH}

# pw script
export PW_FILE=/home/jrblevin/config/private/pwsafe.gpg

# determine platform
ARCH=`uname -m`

# Ruby
export PATH=/var/lib/gems/1.8/bin:$PATH
export RUBYLIB=$HOME/lib/ruby:/usr/local/lib/site_ruby/1.8:/usr/lib/ruby/1.8

# GFortran
export PATH=/opt/gcc-trunk/bin:${PATH}

if [[ ${ARCH} == "x86_64" ]]; then
    LIB64="64"
fi

if [ -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH="/opt/gcc-trunk/lib${LIB64}"
else
  LD_LIBRARY_PATH="/opt/gcc-trunk/lib${LIB64}:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH
