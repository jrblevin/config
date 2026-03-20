#!/usr/bin/zsh
#
# Jason Blevins <jblevins@xbeta.org>
# Columbus, June 6, 2017

# Basic $PATH. Anything in ~/bin has priority.
PATH=${HOME}/bin:${HOME}/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH

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
    # Homebrew
    export PATH="/usr/local/sbin:/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
    export PATH="$(brew --prefix python)/libexec/bin:$PATH"
    # Color ls
    export CLICOLOR=1
    # Use Spotlight database for locate
    function locate { mdfind "kMDItemDisplayName == '$@'"; }
    # Completion dump file
    ZCOMPDUMP=$HOME/.zcompdump.osx
elif [[ $OS == "Linux" ]]; then
    # less input preprocessor
    eval `lessfile`
    ZCOMPDUMP=$HOME/.zcompdump.linux
    # CUDA
    NVHPCSDK=/opt/nvidia/hpc_sdk; export NVHPCSDK
    MANPATH=$MANPATH:$NVHPCSDK/Linux_x86_64/24.1/compilers/man; export MANPATH
    PATH=$NVHPCSDK/Linux_x86_64/24.1/compilers/bin:$PATH; export PATH
    # GFortran
    export PATH=/opt/gcc-trunk/bin:${PATH}
    # Open MPI
    export PATH=/opt/openmpi/bin:${PATH}
    # Gfortran and OpenMPI Library Path
    if [ -z "$LD_LIBRARY_PATH" ]; then
        LD_LIBRARY_PATH="/opt/gcc-trunk/lib${LIB64}:/opt/openmpi/lib:"
    else
        LD_LIBRARY_PATH="/opt/gcc-trunk/lib${LIB64}:/opt/openmpi/lib:$LD_LIBRARY_PATH"
    fi
    export LD_LIBRARY_PATH
fi

# Intel Compilers
if [ -f /opt/intel/oneapi/setvars.sh ]; then
    source /opt/intel/oneapi/setvars.sh > /dev/null
elif [ -f /opt/intel/bin/compilervars.sh ]; then
    source /opt/intel/bin/compilervars.sh ${ICS_ARCH}
fi

# Visual Studio Code
if [[ $OS == "Darwin" ]]; then
    export PATH="${PATH}:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
fi

# Ruby
if [[ $OS == "Darwin" ]]; then
    if whence rbenv >/dev/null; then
        eval "$(rbenv init -)"
    fi
fi

# Coreutils
if [[ $OS == "Darwin" ]]; then
    export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
fi

# LM Studio CLI (lms)
if [[ $OS == "Linux" ]]; then
    export PATH="$PATH:$HOME/.cache/lm-studio/bin"
fi

# Private settings
source $HOME/.zsh_private
