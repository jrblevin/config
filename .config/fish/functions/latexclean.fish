function latexclean
    for ext in aux log bbl brf blg toc dvi fls fdb_latexmk synctex.gz out nav snm fff ttt vrb
        rm -f *.$ext
    end
end
