#!/usr/bin/zsh
#
# Jason Blevins <jblevins@xbeta.org>
# Carrboro, November 16, 2008

### Basic ZSH Configuration

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000
unsetopt SHARE_HISTORY
unsetopt INC_APPEND_HISTORY
setopt APPEND_HISTORY # Append commands to history when a shell exits
setopt HIST_SAVE_NO_DUPS

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

### Git

alias ga='git add'
alias gap='git add --patch'
alias gb='git branch -a'
alias gc='git commit -v'
alias gcm='git commit -m'
alias gca='git commit -v --amend'
alias gcan='git commit -v --amend --no-edit'
alias gcb='git checkout -b'
alias gcm='git checkout master'
alias gcn='git checkout next'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdw='git diff --word-diff'
alias gitx='open -a GitX .'
alias gpl='git plog'
alias grl='git rlog'
alias gst='git status'
alias gm='git merge'
alias gri='git rebase -i'
alias grim='git rebase -i master'
alias gsts='git stash show --patch'
alias gstp='git stash pop'
alias gstd='git stash drop'
alias gli='git ls-files . --ignored --exclude-standard --others'

### Useful Commands and Aliases

if ls -F --color=auto >&/dev/null; then
  if whence dircolors >/dev/null; then
    eval "$(dircolors -b)"
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
    alias ls="ls --color=auto -F"
  else
    export CLICOLOR=1
    zstyle ':completion:*:default' list-colors ''
    alias ls="ls -F"
  fi
else
  alias ls="ls -F"
fi
alias grep='grep --color=auto'
alias less='less -R'
alias make='make -j'
if whence lsd >/dev/null; then
    alias l='lsd -alh --git'
    alias ll='lsd -alh --git'
    alias lh='lsd -alh --git'
    alias lr='lsd -talhr --git'
    alias lsd='lsd -alhd --git */'
    alias k='lsd -alh --git'
else
    alias l='ls -l'
    alias ll='ls -l'
    alias lh='ls -alh'
    alias lr='ls -talr'
    alias lsd='ls -d */'
    alias k='ls -alh'
fi
alias bc='bc -l'
alias ee="emacs -nw"
alias ec="emacsclient"
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias mutt="cd ~/inbox; mutt"
alias sdr="screen -d -R"
alias latexmk="latexmk -g -synctex=1 -pdf -pvc"

tma() {
    if tmux list-sessions 2>/dev/null; then
        tmux -CC attach
    else
        tmux -CC
    fi
}

# Protect OS X tags
# http://brettterpstra.com/2014/07/03/mavericks-tags-and-coreutils-a-warning/
alias mv=/bin/mv
alias cp=/bin/cp

# SSH shortcuts
alias ssg="ssh gauss"
alias ssl="ssh leibniz"
alias ssm="ssh macpro"

# duf: human readable, sorted disk usage
# http://www.earthinfo.org/linux-disk-usage-sorted-by-size-and-human-readable/
alias duf='du -sk * | sort -nr | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\'

# dnf: human readable, sorted file counts
alias dnf='find . -maxdepth 1 -type d | sed -e "s/^.\///" | while read -r dir; do num=$(find "$dir" -type f | wc -l); printf "%d\t%s\n" "$num" "$dir"; done | sort -nr'

# Clean up after latex
alias latexclean='for ext in aux log bbl brf blg toc dvi fls fdb_latexmk synctex.gz out nav snm fff ttt vrb; do rm -f *.$ext; done'

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

eval "$(starship init zsh)"


### SSH

# SSH Agent (http://www.cygwin.com/ml/cygwin/2001-06/msg00537.html)
SSH_ENV="$HOME/.ssh-agent.$HOST"

function start_agent {
     echo -n "Initializing SSH agent..."
     ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
     echo " OK"
     chmod 600 "${SSH_ENV}"
     . "${SSH_ENV}" > /dev/null
     /usr/bin/ssh-add --apple-use-keychain;
}

# Source SSH settings
if [ -f "${SSH_ENV}" ]; then
     . "${SSH_ENV}" > /dev/null
     ps -x | grep ${SSH_AGENT_PID} | grep ssh-agent > /dev/null || {
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

# zsh-autosuggestions
source ~/.zsh/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=1'

# Archival
compress_and_delete () { tar zcvf "$1.tar.gz" "$1" && rm -rf "$1" }

# History search

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^P" up-line-or-beginning-search
bindkey "^N" down-line-or-beginning-search
bindkey "\e[A" up-line-or-beginning-search
bindkey "\e[B" down-line-or-beginning-search

### Git Python Projects

# Activate a Python project by name: cd, source venv, rename tmux window
activate() {
    [[ -z "$1" ]] && echo "Please provide a project name" && return 1
    local project="$1" project_dir=""
    for base in ~/git ~/git/*; do
        project_dir=$(find "$base" -maxdepth 3 -type d -name "$project" 2>/dev/null | while read dir; do
            [[ -d "$dir/.git" && ( -d "$dir/.venv" || -d "$dir/venv" ) ]] && echo "$dir" && break
        done)
        [[ -n "$project_dir" ]] && break
    done
    [[ -z "$project_dir" ]] && echo "Project $project not found" && return 1
    cd "$project_dir"
    [[ -d .venv ]] && source .venv/bin/activate
    [[ -d venv ]] && source venv/bin/activate
    tmux rename-window "$project"
}

# Completion for activate: projects under ~/git with .git and a venv
_activate() {
    local -a projects
    for dir in ~/git/*/; do
        [[ -d "$dir/.git" && ( -d "$dir/.venv" || -d "$dir/venv" ) ]] && projects+=(${dir:h:t})
    done
    for dir in ~/git/*/*/; do
        [[ -d "$dir/.git" && ( -d "$dir/.venv" || -d "$dir/venv" ) ]] && projects+=(${dir:h:t})
    done
    _describe 'project' projects
}
compdef _activate activate
