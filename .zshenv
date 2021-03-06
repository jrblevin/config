#!/usr/bin/zsh
#
# Jason Blevins <jblevins@xbeta.org>
# Columbus, June 6, 2017

# Basic $PATH. Anything in ~/bin has priority.
PATH=${HOME}/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH

# Library path: add /usr/local/lib
if [ -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH="/usr/local/lib"
else
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH

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
    # MacTeX
    export PATH="/Library/TeX/texbin:$PATH"
    # ARM Homebrew
    export PATH="/opt/homebrew/bin:$PATH"
    # Color ls
    export CLICOLOR=1
    # Use Spotlight database for locate
    function locate { mdfind "kMDItemDisplayName == '$@'wc"; }
    # Completion dump file
    ZCOMPDUMP=$HOME/.zcompdump.osx
    # Intel compilers
    export LM_LICENSE_FILE=${LM_LICENSE_FILE}:28518@license5.osc.edu
    # PGI
    export PGI_TOP=/opt/pgi
    export PGI_VERSION=2019
    export PATH=$PGI_TOP/osx86-64/$PGI_VERSION/bin:$PGI_TOP/osx86-64/$PGI_VERSION/mpi/mpich/bin:$PATH
    export MANPATH=$MANPATH:$PGI/osx86-64/$PGI_VERSION/man
    export LM_LICENSE_FILE=${LM_LICENSE_FILE}:/opt/pgi/license.dat
elif [[ $OS == "Linux" ]]; then
    # less input preprocessor
    eval `lessfile`
    ZCOMPDUMP=$HOME/.zcompdump.linux
    # GFortran
    export PATH=/opt/gcc-trunk/bin:${PATH}
    # PGI
    export PGI=/opt/pgi
    export PGI_VERSION=15.10
    export PATH=$PGI/linux86-64/$PGI_VERSION/bin:$PATH
    export MANPATH=$MANPATH:$PGI/linux86-64/$PGI_VERSION/man
    export LM_PROJECT=PAS0501
    export LM_LICENSE_FILE=$LM_LICENSE_FILE:7496@license2.osc.edu
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

# Intel Compilers
if [ -f /opt/intel/bin/compilervars.sh ]; then
    source /opt/intel/bin/compilervars.sh ${ICS_ARCH}
fi

# Emacs
if [[ $OS == "Darwin" ]]; then
    EMACS25=/Applications/Emacs\ 25.app/Contents/MacOS/Emacs-x86_64-10_9
    EMACS24=/Applications/Emacs\ 24.5.app/Contents/MacOS/Emacs
fi

# Ruby
if [[ $OS == "Darwin" ]]; then
    if [[ ! -z $(which rbenv) ]]; then
        eval "$(rbenv init -)"
    fi
fi

# Python
if [[ $OS == "Darwin" ]]; then
    if [[ ! -z $(which python2.7) ]]; then
        export PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}
    fi
fi

# Private settings
source $HOME/.zsh_private
