#!/usr/bin/zsh
#
# Jason Blevins <jrblevin@sdf.lonestar.org>
# Carrboro, November 16, 2008 09:36 EST
#
# Last Modified: November 16, 2008 15:15 EST

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# Emacs keybindings (-v for vi)
bindkey -e

# settings
setopt correct_all              # correct misspelled commands
setopt autocd                   # cd by typing directory name
setopt hist_ignore_dups         # ignore repeated commands
setopt nobeep                   # stop yelling at me

# programs
export EDITOR=vi                # default editor

# aliases
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
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

# Don't treat slashes as part of words
WORDCHARS="${WORDCHARS:s#/#}"

# Provide a more convenient PATH
export PATH=${HOME}/bin:${PATH}
