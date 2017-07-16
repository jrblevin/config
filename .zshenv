#!/usr/bin/zsh
#
# Jason Blevins <jrblevin@sdf.org>
# Columbus, June 6, 2017

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
    # MacTeX
    export PATH=/Library/TeX/texbin:$PATH
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
    # PGI
    export PGI="/Applications/Free PGI.app/Contents/Resources/pgi"
    export PGI_VERSION=16.3
    export PATH="$PGI/osx86-64/$PGI_VERSION/bin:$PATH"
    export MANPATH="$MANPATH:$PGI/linux86-64/$PGI_VERSION/man"
    export PGI_MINOSX=$(sw_vers -productVersion)
    export PGI_MINOSXNUM="1011"
    export PGI_ISCLANG31="703"
    export PGI_GCCDIR64="/Applications/Xcode.app/Contents/Developer/usr/lib/llvm-gcc/4.2.1"
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

# Emacs
if [[ $OS == "Darwin" ]]; then
    EMACS25=/Applications/Emacs\ 25.app/Contents/MacOS/Emacs-x86_64-10_9
    EMACS24=/Applications/Emacs\ 24.5.app/Contents/MacOS/Emacs
fi

# Private settings
source $HOME/.zsh_private
