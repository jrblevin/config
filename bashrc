## ~/.bashrc                                      -*-shell-script-*-
#
# Jason Blevins <jrblevin@sdf.lonestar.org>
#
# Created: Raleigh, June 4, 2004
# Last Modified: April 17, 2008 10:33 EDT

## General stuff

# If not running interactively, don't do anything.
[ -z "$PS1" ] && return

# Don't put duplicate lines in the history (needs bash version 3).
export HISTCONTROL=erasedups

# Prevent concurrent bash sessions from overwriting the history.
shopt -s histappend

# Check the window size after each command and update if necessary.
shopt -s checkwinsize

# Make less more friendly for non-text input files (see lesspipe(1)).
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"


## Customizations

# Set the prompt to 'user@host:/dir$' (with colors).
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir.
case $TERM in
   xterm*)
           PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
           ;;
   *)
           ;;
esac

# Enable color ls.
if [ "$TERM" != "dumb" ]; then
    eval `dircolors -b`
    alias ls='ls --color=auto'
fi

# Enable tab completion.
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# SSH hostname completion.
SSH_COMPLETE=( $(cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | egrep -v [0123456789]) )
complete -o default -W "${SSH_COMPLETE[*]}" ssh


# Aliases
# -------

# Letter friendly ps2pdf:
alias ps2pdf='ps2pdf -sPAPERSIZE=letter'

# Fast Emacs
alias e="emacs -q -nw --no-splash"
alias ee="emacs -q -nw --no-site --no-splash"


# Editor setup
# ------------

export EDITOR="vi"


# Paths
# -----

# Provide a more convenient PATH
export PATH=${HOME}/bin:${PATH}

# MANPATH
export MANPATH=/usr/share/man:/usr/local/share/man


# AMPL
# ----

PATH="/usr/local/ampl:${PATH}"; export PATH


# Intel Fortran Compiler
# ----------------------

INTEL_PATH="/opt/intel"
MACHINE=`uname -m`

INTEL_LICENSE_FILE="${INTEL_PATH}/licenses/l_ifxs_ncom_JVF7HFHZ.lic"
export INTEL_LICENSE_FILE

if [[ ${MACHINE} == "i686" ]]; then
    FC_PATH="${INTEL_PATH}/fc/current"
    IDB_PATH="${INTEL_PATH}/idb/current"
else
    if [[ ${MACHINE} == "x86_64" ]]; then
	FC_PATH="${INTEL_PATH}/fce/current"
	IDB_PATH="${INTEL_PATH}/idbe/current"
    fi
fi
PATH="${FC_PATH}/bin:${IDB_PATH}/bin:${PATH}"
export PATH

if [ -z "${MANPATH}" ]; then
    MANPATH="${FC_PATH}/man:${IDB_PATH}/man";
    export MANPATH;
else
    MANPATH="${FC_PATH}/man:${IDB_PATH}/man:${MANPATH}";
    export MANPATH;
fi

if [ -z "${LD_LIBRARY_PATH}" ]; then
    LD_LIBRARY_PATH="${FC_PATH}/lib:${IDB_PATH}/lib";
    export LD_LIBRARY_PATH;
else
    LD_LIBRARY_PATH="${FC_PATH}/lib:${IDB_PATH}/lib:${LD_LIBRARY_PATH}";
    export LD_LIBRARY_PATH;
fi


# Ruby
# ----

export PATH=/var/lib/gems/1.8/bin:$PATH
export RUBYLIB=$HOME/lib/ruby:/usr/local/lib/site_ruby/1.8:/usr/lib/ruby/1.8


# Password Manager
# ----------------

export PW_FILE=~/config/private/pwsafe.gpg
export PW_LEN=16
