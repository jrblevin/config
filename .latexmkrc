$pdf_previewer = "open -a /Applications/Skim.app"; 
$clean_ext = "aux log bbl brf blg toc out";
$pdflatex = 'pdflatex -8bit -etex -shell-escape -file-line-error -halt-on-error -synctex=1 %O %S';
$new_viewer_always [0];
$pvc_view_file_via_temporary [1];
