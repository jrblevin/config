;; gauss-mode.el - major mode for editing Gauss programs with GNU Emacs
;;
;; This major mode for GNU Emacs provides support for editing Gauss
;; source files.  It automatically indents for block structures, line
;; continuations (e.g., ...), and comments.  The usual paren matching
;; support is included.  
;;
;; In addition to automatic indentation when typing "do while" blocks,
;; "if" blocks, "proc" 's, etc., TAB indents an existing or new line to 
;; the correct level and META-; sets up a comment line.  META-return starts
;; a newline at column one, ignoring the automatic indentation.
;; To properly indent an existing region in a *.prg file, use the command
;; M-x indent-region.
;;
;; To enter gauss-mode automatically when editing .prg files, put 
;; something like this in your .emacs file.
;;
;;   (autoload 'gauss-mode "gauss-mode" "Enter Gauss-mode." t)
;;   (setq auto-mode-alist (cons '("\\.prg$" . gauss-mode) auto-mode-alist))
;;   (setq gauss-mode-hook '(lambda () (setq fill-column 74)))
;;
;; Enjoy.
;;
;; THIS FILE IS A COMPLETE HACK by Steve Berry (March 1994).  It is
;; modified from octave-mode.el, which is modified from
;; matlab-mode.el.  Warning: I am no lisp programmer. 
;; The support for
;; syntax-highlighting (ie font-lock-mode) was added by Neel Krishnaswami
;; in July 1999. 
;;
;; Indenting and syntax-colouring algorithm changed by Ott Toomet
;; (otoomet (a) econ au dk) in September 2002
;;
;; * * * * * * * * * * * * * * * * * * * * * * * * * 
;;
;; Octave-mode.el was modified by John W. Eaton (jwe@che.utexas.edu) from
;; the file matlab-mode.el which is:
;;
;; Copyright (C) 1991 Matthew R. Wette.
;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made the last modification and the date of such modification.
;;   3. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.
;;
;; ------------------------------------------------
;; Constants used in all Gauss-mode buffers.
(defconst gauss-indent-level 3
  "*The indentation in Gauss-mode.")

(defconst gauss-comment2-column 40
  "*The goal comment column in Gauss-mode buffers.")

(defconst gauss-comment1-indent-level 3
  "*The indentation of rows of /* */ comment relative to comment start.")

;; Syntax Table
(defvar gauss-mode-syntax-table nil
  "Syntax table used in Gauss-mode buffers.")

(if gauss-mode-syntax-table
    ()
  (setq gauss-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." gauss-mode-syntax-table)
  (modify-syntax-entry ?/ "." gauss-mode-syntax-table)
  (modify-syntax-entry ?* "." gauss-mode-syntax-table)
  (modify-syntax-entry ?+ "." gauss-mode-syntax-table)
  (modify-syntax-entry ?- "." gauss-mode-syntax-table)
  (modify-syntax-entry ?= "." gauss-mode-syntax-table)
  (modify-syntax-entry ?< "." gauss-mode-syntax-table)
  (modify-syntax-entry ?> "." gauss-mode-syntax-table)
  (modify-syntax-entry ?& "." gauss-mode-syntax-table)
  (modify-syntax-entry ?| "." gauss-mode-syntax-table)
  (modify-syntax-entry ?' "." gauss-mode-syntax-table)
					; ' is matrix transpose, not string quote!
  (modify-syntax-entry ?@ "\"" gauss-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14b" gauss-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" gauss-mode-syntax-table)
  (set-syntax-table gauss-mode-syntax-table))


;; Abbrev Table
(defvar gauss-mode-abbrev-table nil
  "Abbrev table used in Gauss-mode buffers.")

(define-abbrev-table 'gauss-mode-abbrev-table ())


;; Syntax Highlighting

(defvar gauss-compiler-directives
  (mapconcat 'identity
	     '( "#define\\>"	    "#definecs\\>"
		"#undef\\>"	    "#ifdef\\>"
		"#ifndef\\>"	    "#iflight\\>"
		"#ifdos\\>"	    "#ifos2win\\>"
		"#ifunix\\>"	    "#else\\>"
		"#endif\\>"	    "#include\\>"
		"#lineson\\>"	    "#linesoff\\>"
		"#srcfile\\>"	    "#srcline\\>")
	     "\\|"))

(defvar gauss-keywords
  (mapconcat 'identity
	     '("\\<proc\\>"	"\\<keyword\\>"	
	       "\\<endp\\>"	"\\<retp\\>"
	       "\\<do\\>"	"\\<while\\>"
	       "\\<until\\>"	"\\<endo\\>"
	       "\\<for\\>"	"\\<endfor\\>"
	       "\\<break\\>"	"\\<continue\\>"
	       "\\<if\\>"	"\\<elseif\\>"
	       "\\<else\\>"	"\\<endif\\>"
	       "\\<goto\\>"	"\\<gosub\\>")
	     "\\|"))

(defvar gauss-types
  (mapconcat 'identity
	     '("\\<matrix\\>"	"\\<string\\>"
	       "\\<declare\\>"	"\\<let\\>"
	       "\\<fn\\>"	"\\<local\\>")
	     "\\|"))

(defvar gauss-identifier "[a-zA-Z0-9_]+")

(defvar gauss-local-declaration
  (concat "\\<local\\s-+"
	  "\\("
	  gauss-identifier
	  "\\s-*\\(,\\s-*" gauss-identifier "\\s-*\\)*"
	  "\\);"))

(defvar gauss-type-declaration-1
  (concat "\\<declare\\s-+"
	  "\\(matrix\\|string\\)?"
	  "\\("
	  gauss-identifier
	  "\\s-*\\(,\\s-*" gauss-identifier "\\s-*\\)*"
	  "\\);"))

(defvar gauss-type-declaration-2
  (concat "\\<declare\\s-+"
	  "\\(" gauss-types "\\)?"
	  "\\s-*"
	  "\\(" gauss-identifier "\\)"
	  "\\s-*"
	  "\\(=\\|!=\\|:=\\|\?=\\)"
	  "[^;]+;"))

(defvar gauss-proc-declaration
  (concat "\\<proc\\s-*"
	  "\\(([0-9]+)\\)?"
	  "\\s-*[=]?\\s-*"
	  "\\(" gauss-identifier "\\)"))

(defvar gauss-font-lock-keywords
  (list
   (cons gauss-compiler-directives 'font-lock-warning-face)
   (cons gauss-types 'font-lock-type-face)
   (cons gauss-keywords 'font-lock-keyword-face)
   (cons gauss-proc-declaration
	 '(2 font-lock-function-name-face nil nil))
   (cons gauss-local-declaration
	 '(1 font-lock-variable-name-face nil nil))
   (cons gauss-type-declaration-1
	 '(2 font-lock-variable-name-face nil nil))
   (cons gauss-type-declaration-2
	 '(2 font-lock-variable-name-face nil nil))
   ))

;; Mode Map
(defvar gauss-mode-map ()
  "Keymap used in gauss-mode.")

(if gauss-mode-map
    ()
  (setq gauss-mode-map (make-sparse-keymap))
  (define-key gauss-mode-map "\r" 'gauss-return)
  (define-key gauss-mode-map "\t" 'gauss-indent-line)
  (define-key gauss-mode-map "\M-;" 'gauss-comment)
  (define-key gauss-mode-map "\M-\r" 'newline))


;; Gauss Mode
(defun gauss-mode ()
  "Major mode for editing Gauss source files.  Version 1.1a, 22/IX 2002
Will run gauss-mode-hook if it is non-nil.  Auto-fill-mode seems to work.
Filling does not work (yet).
  Comments are indented as follows:  
@ .. @ comments are put on col gauss-comment2-col (default 40)
/* ... */ comments are indented as ordinary code lines.  If the comment 
exceeds one line, continuation is indented by 3.

Special Key Bindings:
\\{gauss-mode-map}
Variables:
  gauss-indent-level                   Level to indent blocks (default 3).
  gauss-comment2-column                Goal column for @..@ comments (40).
Commands:
  gauss-mode                           Enter Gauss major mode.
  gauss-return                         Handle return with indenting (RET).
  gauss-indent-line                    Indent line for structure (TAB).
  gauss-comment                        Add comment to current line.
To add automatic support put something like the following in your .emacs file:
  \(autoload 'gauss-mode \"gauss-mode\" \"Enter Gauss-mode.\" t\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.m[201z$\" . gauss-mode\) \
auto-mode-alist\)\)
  \(setq gauss-mode-hook '\(lambda \(\) \(setq fill-column 74\)\)\)"

  (interactive)
  (kill-all-local-variables)
  (use-local-map gauss-mode-map)
  (setq major-mode 'gauss-mode)
  (setq mode-name "Gauss")
  (setq local-abbrev-table gauss-mode-abbrev-table)
  (set-syntax-table gauss-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'gauss-indent-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[%@][ \t]*")
  (make-local-variable 'comment2-column)
  (setq comment-column 'gauss-comment2-column)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'gauss-comment-indent)
  (make-local-variable 'fill-column)
  (setq fill-column default-fill-column)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gauss-font-lock-keywords
			     nil  	         ;; Keywords-only?
			     t  		 ;; Fold case?
			     nil		 ;; Syntax-alist
			     nil		 ;; Syntax-begin    
			    ))
  (run-hooks 'gauss-mode-hook))


(defun gauss-return ()
  "Handle carriage return in Gauss-mode."
  (interactive)
  (if (gauss-block-end-line)
      (gauss-indent-line))
  (newline)
  (gauss-indent-line))

(defun gauss-comment ()
  "Add a comment to the following line, or format if one already exists."
  (interactive)
  (cond
   ((gauss-empty-line)
    (gauss-indent-line)
    (insert "@   @") 
    (backward-char) (backward-char) (backward-char) )
   ((gauss-comment-line))
   (t
    (end-of-line)
    (re-search-backward "[^ \t^]" 0 t)
    (forward-char)
    (delete-horizontal-space)
    (if (< (current-column) gauss-comment2-column)
        (indent-to gauss-comment2-column)
      (insert " "))
    (insert "@   @") 
     (backward-char) (backward-char) (backward-char) )))

(defun gauss-comment-indent ()
  "Indent a comment line in Gauss-mode."
  (gauss-calc-indent))

(defun gauss-indent-line ()
  "Indent a line in Gauss-mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (gauss-calc-indent)))
  (skip-chars-forward " \t"))

(defun gauss-indent-type ()
  "Display type of current or previous nonempty line.  Used in debugging."
  (interactive)
  (message (concat "gauss-ident-type: " gauss-last-indent-type)))

(defun gauss-fill-region (from to &optional justify-flag)
  "Fill the region of comments.
Prefix arg (non-nil third arg, if called from program)
means justify as well."
  (interactive "r\nP")
  (messages "gauss-fill-region not implemented yet."))

(defvar gauss-last-indent-type "unknown"
  "String to tell line type.")

(defun gauss-calc-indent ()
  "Return the appropriate indentation for this line as an int."
  (interactive)
  (if (gauss-comment2-line)
      gauss-comment2-column
					; if the current line is a comment2
					; (@..@ comment), indent to
					; comment-column, no matter what the
					; previous lines are
    ;;Now find indent of the previous non-comment line
    (save-excursion
      ;; extra indentation for current line, indentation will base on that plus
      ;; previous-line
      (set 'this-line-indent
	   (cond
	    ((gauss-block-end-line) 
	     (- (current-indentation) gauss-indent-level))
	    (t
					; no special syntax from current line
	     0)
	    )
	   )
      ;; indenting, based on previous line
      (setq prev-indent
	   (if (gauss-prev-not-comment2-line)
	       (cond
		((= (point) 1)
		 0)
		((gauss-continuation-beg-line)
		 (+ (current-indentation) (* 2 gauss-indent-level)))
		((gauss-continuation-end-line)
		 (- (current-indentation) (* 2 gauss-indent-level)))
		((gauss-comment1-end-line)
		 (- (current-indentation) gauss-comment1-indent-level))
		((gauss-comment1-beg-line)
		 (+ (current-indentation) gauss-comment1-indent-level))
		((gauss-block-beg-line)
		 (+ (current-indentation) gauss-indent-level))
		(t
					; it include normal syntax lines
					; continuation lines
		 (current-indentation))
		)
	     0
					; if first line, prev indent = 0
	     )
	   )
      (set 'indent (+ this-line-indent prev-indent))
      (if (< indent 0) (setq indent 0))
      indent
      ))
)

(defun gauss-prev-not-comment-line ()
  "find a previous line, not comment nor empty (it may though contain"
  "comment but must contain a syntax part too).  Return point or nil if"
  "the first line of buffer.  Move point to that line."
  (set 'flob 0)
  (while (and
	  (= 0 (set 'flob (forward-line -1)))
					; flob=1 -> first line of buffer
	  (cond
	   ((gauss-comment-line)
	    t)
					; continue search if comment
	   ((gauss-empty-line)
	    t)
					; and if emty
	   ((gauss-comment1-end-line)
	    (search-backward "/*" 1 t)
	    (gauss-eoln-no-comment-point)
	    (bolp))
	   ) ;cond
	  ) ;and
    ) ;while
  (if (= 0 flob)
      (point)
					; point, if not flob
    nil
					; no previous line
    )
  )

(defun gauss-prev-not-comment2-line ()
  "find a previous line, not comment2 nor empty.  Return point or nil if"
  "the first line of buffer and put point on that line."
  (set 'flob 0)
  (while (and
	  (= 0 (set 'flob (forward-line -1)))
					; flob=1 -> first line of buffer
	  (or
	   (gauss-comment-line)
	   (gauss-empty-line)
	   )
	  ))
  (if (= 0 flob)
      (point)
					; point, if not flob
    nil
					; no previous line
    )
  )

(defun gauss-empty-line ()
  "Returns t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun gauss-continuation-beg-line ()
  "Returns t if current line ends w/o	; (and optional comment)"
  "and previous line ends with ;.  Previous line may have a complete"
  "comment but not start of it."
  (save-excursion
    (and
     ;; this line do not end with ;
     (progn
       (gauss-eoln-no-comment-point)
       (and
	(not (bolp))
	(not (= (char-before) ?\;)))
					; this line does not end with ;
       )
     (progn
       (or
	(not (gauss-prev-not-comment-line))
					; either first line
	(progn
	  ;; or previous line ends with ;
	  (gauss-eoln-no-comment-point)
	  (and
	   (not (bolp))
					; not empty line
	   (= (char-before) ?\;)
					; ends with ;
	   (not (gauss-comment1-beg-line))
					; not comment start
	   );and
	  )
	)
       ))
     ))

(defun gauss-continuation-end-line ()
  "Return t if current line is a continuation of previous lines"
  (save-excursion
    (gauss-eoln-no-comment-point)
    (and
     (= (char-before) ?\;)
					; this line ends w/;
     (progn
       (and
	(gauss-prev-not-comment2-line)
					; not first line
	(progn
	  (gauss-eoln-no-comment-point)
	  (if (not (bolp))
	      (/= (char-before) ?\;)
					; and previous line is a
					; continuation line
	    nil); if
	  ); progn
	(not (gauss-comment1-end-line))
					; prev line not comment-end line
	  
	); and
       );progn
     )
))

(defun gauss-comment1-beg-line ()
  "Return t if current line contains beginning of /* ... */ comment, but not"
  "the end of it."
  (save-excursion
    (beginning-of-line)
    (and
     (search-forward "/*" (gauss-eoln-point) t)
					; line contains start of comment
     (not (search-forward "*/" (gauss-eoln-point) t))
					; but not end of it
    )
    ))

(defun gauss-comment1-end-line ()
  "Return t if current line contains end of /* ... */ comment, but not"
  "the beginning of it."
  (save-excursion
    (beginning-of-line)
    (setq line-start (point))
    (and
     (setq comment-end (search-forward "*/" (gauss-eoln-point) t))
					; line contains end of comment
     (not (search-backward "/*" line-start t)))
    ))

(defun gauss-comment1-line ()
  "Return t if line is a pure /* .. */ comment line with no syntax part"
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*/\\*.*\\*/[ \t]*$")
    ))

(defun gauss-comment2-line ()
  "Return t if line is a pure @..@ comment line with no syntax part"
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*@.*@[ \t]*$")
    ))

(defun gauss-comment-line ()
  "Return t if line is pure comment line with no syntax part"
  (save-excursion
    (beginning-of-line)
    (or
     (gauss-comment1-line)
     (gauss-comment2-line)
     )
    ))

(defun gauss-eoln-point ()
  "Returns point for end-of-line in Gauss-mode."
  (save-excursion
    (end-of-line)
    (point)))

(defun gauss-boln-point ()
  "Return point for beginning of line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun gauss-eoln-no-comment-point ()
  "Returns the end-of-line, in the case of trailing comment,"
  "the point before whitespaces before comment.  Puts point on that place."
  (end-of-line)
  (set 'bol (gauss-boln-point))
  (while (search-backward "/*" bol t))
      					; find /* comments
  (while (search-backward "@" bol t))
					; find @@ comments
  (if (re-search-backward "[^ \t]" bol t)
      (forward-char 1)
					; jump to last whitespace
    (beginning-of-line)
					; jump to beginning if the line was
					; empty
    )
  (point)
  )

(defun gauss-block-beg-line ()
  "Returns t if line contains beginning of Gauss block."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "[ \t]*" gauss-block-beg-kw))
    ))

(defconst gauss-block-beg-kw
  "\\<\\(do\\|for\\|until\\|if\\|else\\|elseif\\|proc\\)\\>"
  "Regular expression for keywords which begin blocks in Gauss-mode.")

(defun gauss-block-end-line ()
  "Returns t if line contains end of Gauss block."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "\\([^@\n]*[ \t]\\)?" gauss-block-end-kw))))

(defconst gauss-block-end-kw
  "\\(endfor\\|endo\\|endif\\|endp\\|else\\|elseif\\)"
  "Regular expression for keywords which end blocks.")

(defun gauss-block-beg-end-line ()
  "Returns t if line contains matching block begin-end in Gauss-mode."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat
                 "\\([^%@\n]*[ \t]\\)?" gauss-block-beg-kw 
                 "." "\\([^%@\n]*[ \t]\\)?" gauss-block-end-kw))))

(defun gauss-in-comment ()
  "Returns t if point is in a comment."
  (save-excursion
    (and (/= (point) (point-max)) (forward-char))
    (search-backward "[%@]" (save-excursion (beginning-of-line) (point)) t)))

(provide 'gauss-mode)

;; --- last line of gauss-mode.el --- 

