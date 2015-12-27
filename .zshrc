#!/usr/bin/zsh
#
# Jason Blevins <jrblevin@sdf.org>
# Carrboro, November 16, 2008
# Last Modified: December 26, 2015

### System-Specific Configuration

# Basic $PATH
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH

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
    # GFortran
    export PATH=/opt/gcc-trunk/bin:${PATH}
    # Open MPI
    export PATH=/opt/openmpi/bin:${PATH}
    # Library path
    if [ -z "$LD_LIBRARY_PATH" ]; then
        LD_LIBRARY_PATH="/opt/gcc-trunk/lib${LIB64}:/opt/openmpi/lib:"
    else
        LD_LIBRARY_PATH="/opt/gcc-trunk/lib${LIB64}:/opt/openmpi/lib:$LD_LIBRARY_PATH"
    fi
    export LD_LIBRARY_PATH
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
export EDITOR="$HOME/bin/EDITOR" # default editor

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
alias l='ls -l'
alias ll='ls -l'
alias lh='ls -alh'
alias lr='ls -talr'
alias bc='bc -l'
alias ee="emacs -nw"
alias ec="emacsclient"
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias gd='git diff'
alias gdc='git diff --cached'
alias grl='git rlog'
alias gst='git status'
alias gci='git commit'
alias gca='git commit --amend'
alias gcm='git commit -m'
alias gitx='open -a GitX .'
alias mutt="cd ~/inbox; mutt"
alias sdr="screen -d -R"
alias latexmk="latexmk -g -synctex=1 -pdf -pvc"

# Protect OS X tags
# http://brettterpstra.com/2014/07/03/mavericks-tags-and-coreutils-a-warning/
alias mv=/bin/mv
alias cp=/bin/cp

# SSH shortcuts
alias ss1="ssh ap-jb01"
alias ssm="ssh blevins-mac"
alias ssn="ssh newton"
alias ssl="ssh newton.local"
alias ssg="ssh gauss"
alias ssr="ssh roark"

# duf: human readable, sorted disk usage
# http://www.earthinfo.org/linux-disk-usage-sorted-by-size-and-human-readable/
alias duf='du -sk * | sort -nr | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\'

# dnf: human readable, sorted file counts
alias dnf='find . -maxdepth 1 -type d | sed -e "s/^.\///" | while read -r dir; do num=$(find "$dir" -type f | wc -l); printf "%d\t%s\n" "$num" "$dir"; done | sort -nr'

# Clean up after latex
alias latexclean='for ext in aux log bbl brf blg toc dvi fls fdb_latexmk synctex.gz out nav snm fff ttt; do rm -f *.$ext; done'

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
if [[ $OS == "Darwin" ]]; then
    if [[ ! -z $(which ruby1.9) ]]; then
      export PATH=/opt/local/lib/ruby1.9/gems/1.9.1/bin:${PATH}
      export RUBYLIB=/opt/local/lib/ruby1.9/gems/1.9.1/gems:/opt/local/lib/ruby1.9/site_ruby/1.9.1
    fi
fi

### Paths

# Library path
if [ -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH="/usr/local/lib"
else
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
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

### Per-Directory ZSH configuration

function chpwd() {
  if [ -r $PWD/.zsh_config ]; then
    source $PWD/.zsh_config
  fi
}

# k
source ~/.zsh/k/k.sh
