syntax on                               " Enable syntax highlighting.
colorscheme less                        " Select a color scheme.
set textwidth=78                        " Wrap text at column 78.
set showcmd                             " Show commands as they are typed.
set ruler                               " Show line and column numbers.
set nocompatible                        " Use Vim extensions.
set incsearch                           " Incremental searching.
set hlsearch                            " Highlight search patterns.
set expandtab                           " Spaces instead of tabs.
                                        " Use CTRL-V TAB to insert a tab.
set tabstop=4                           " Set the tab width to four spaces.

set wildmode=longest:full               " Tab completion of filenames.
set wildmenu                            " Show completions in a status bar.

" Return to the last known cursor position.
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

" Fortran settings
au BufNewFile,BufRead *.f90
\ setlocal tabstop=3
let fortran_do_enddo=1
