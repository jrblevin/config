syntax on                              " Enable syntax highlighting.
set textwidth=75                       " Wrap text at column 75.
set showcmd                            " Show commands as they are typed.
set showmatch                          " Show matching brackets.

" Filetypes
au BufRead,BufNewFile *tmp/mutt* :set ft=mail

" Email
autocmd FileType mail        set textwidth=72

