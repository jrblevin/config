syntax on                               " Enable syntax highlighting.
colorscheme tango                       " Select a color scheme.
set textwidth=78                        " Wrap text at column 78.
set showcmd                             " Show commands as they are typed.
set ruler                               " Show line and column numbers.
set nocompatible                        " Use Vim extensions.
set incsearch                           " Incremental searching.
set hlsearch                            " Highlight search patterns.
set expandtab                           " Spaces instead of tabs.
                                        " Use CTRL-V TAB to insert a tab.
set tabstop=4                           " Set the tab width to four spaces.
set exrc                                " Load directory-specific vimrc.
set visualbell                          " Flash the screen on errors.

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
let fortran_free_source=1
let fortran_fold=1

augroup mkd
  autocmd BufRead *.mdwn  set ai formatoptions=tcroqn2 comments=n:>
  autocmd BufRead *.text  set ai formatoptions=tcroqn2 comments=n:>
augroup END
