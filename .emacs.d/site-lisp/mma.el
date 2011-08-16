;;; Emacs Edit mode for this file is -*- Emacs-lisp -*-

;; mma.el --- programming mode for writing Mathematica package files
;; $Id: mma.el,v 1.1 2003/01/23 08:10:55 halfmann Exp $

;; Copyright (C) 1999 Tim Wichmannn

;; Author: Tim Wichmann <wichmann@itwm.uni-kl.de>
;; Contributions: Jens Schmidt (his file sh-light.el was the starting point
;;                              for mma.el)
;; Maintainer: Tim Wichmann <wichmann@itwm.uni-kl.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; This file is not part of GNU Emacs but the same permissions apply.


;;; Commentary:

;; This Emacs mode provides a programming mode for writing Mathematica
;; package files.
;; It provides
;;  o a simple indentation machine
;;  o font-lock support
;;  o function-menu support for XEmacs
;;  o imenu support for Emacs
;;  o some simple functions for activating/deactivating debug code

;; The indentation machine works as follows:
;; -----------------------------------------
;;
;; If point is on the leading whitespace of the current line, indents as far 
;; as the preceding non-empty line and moves point to the first non-whitespace
;; character of the line.
;; If point is not on the leading whitespace of the current line, indents by
;; steps of `mma-indentation'.
;; If called with prefix argument, aligns rest of current line with the next
;; whitespace character position in previous lines:
;; 
;;     ---------- Buffer: foo.m ----------
;;     MyOptionName -> MyValue
;;     Another-!--> Value
;;     ---------- Buffer: foo.m ----------
;; 
;; Calling with prefix argument yields:
;; 
;;     ---------- Buffer: foo.m ----------
;;     MyOptionName -> MyValue
;;     Another      -!--> Value
;;     ---------- Buffer: foo.m ----------
;; 
;; (Difficult to explain. Just try it and see the result.)
;; If called with negative prefix argument, aligns with whitespace in 
;; following lines.
;; The variable `mma-indent-align-at-eolp' specifies if `mma-indent-line'
;; indents by steps of `mma-indentation' (value is nil) or aligns to
;; whitespace (value is non-nil) if point is at the end of line."
;; 
;; Activating/Deactivating debug code:
;; -----------------------------------
;;
;; Since Mathematica does not provide a source code debugger, I often find
;; myself writing pieces of code like
;;   Print[ "value1= ", somevariable ];
;; when I am debugging my Mathematica functions, removing them as soon as
;; the bug is fixed - and inserting them back on if another bug occurs.
;; To simplify this procedure you can mark debug code with a special marker
;; comment like this
;;   (*: DEBUG MyFunction :*)
;;   Print[ "value1= ", somvevariable ];
;;   (*: ENDDEBUG :*)
;; (Use the function `mma-debug-insert' to insert a debug code skeleton).
;; Each debug code has a marker, in the above example it is "MyFunction".
;; The functions `mma-debug-on' and `mma-debug-off' activate or deactivate
;; your debug code. Both take a regexp as argument and act only on debug code
;; which marker matches the regexp. Thus, (mma-debug-off "MyFun.*") would turn
;; off the above debug code (i.e., it inserts additional comments surrounding
;; the Print statement). (mma-debug-on ".*") turns on all debug code.
;; The variable `mma-debug-fontify' determines whether to re-fontify
;; the buffer after a change to the debug code.
;; Customizing the variables `mma-debug-start-1', `mma-debug-start-2', and
;; `mma-debug-end' you can change the special markers for you favorite
;; style.

;;; Installation: 

;; To use mma-mode you should add the following to your .emacs file:
;;	(autoload 'mma-mode "mma.el" "Mathematica package file mode" t)
;;      (setq auto-mode-alist
;;            (cons '("\\.m\\'" . mma-mode) auto-mode-alist))
;;

;; Note: Mathematica is a registered trademark of Wolfram Research, Inc.

;;;; Code:

(defconst mma-version "$Revision: 1.1 $"
  "The Revision number of mma.el.
You should add this number when reporting bugs.")

(defvar mma-emacs-flavor nil
  "A symbol describing the emacs flavor (currently 'emacs and 'xemacs)")  

(defun mma-get-emacs-flavor ()
  "Determine flavor of current emacs.
Sets variable `mma-emacs-flavor'."
  (setq mma-emacs-flavor
	(if (string-match "XEmacs\\|Lucid" emacs-version)
	    'xemacs
	  'emacs)))

(mma-get-emacs-flavor)

(defgroup mathematica nil
  "Emacs interface for editing Mathematica package files."
  :group 'languages)

(cond
 ((eq mma-emacs-flavor 'xemacs)
  (fset 'mma-mark-active 'mark)
  (fset 'mma-debug-fontify 'font-lock-fontify-buffer))
 ((eq mma-emacs-flavor 'emacs)
  (fset 'mma-mark-active (function (lambda () mark-active)))
  (fset 'mma-debug-fontify 'font-lock-fontify-block)))

;;;; - variables.
(defvar mma-mode-syntax-table nil
  "Syntax table used in mma mode.")

(if mma-mode-syntax-table
    ()
  (setq mma-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?( "()1" mma-mode-syntax-table)
  (modify-syntax-entry ?) ")(4" mma-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" mma-mode-syntax-table)
  (modify-syntax-entry ?$ "_" mma-mode-syntax-table)
  (modify-syntax-entry ?% "." mma-mode-syntax-table)
  (modify-syntax-entry ?& "." mma-mode-syntax-table)
  (modify-syntax-entry ?+ "." mma-mode-syntax-table)
  (modify-syntax-entry ?- "." mma-mode-syntax-table)
  (modify-syntax-entry ?/ "." mma-mode-syntax-table)
  (modify-syntax-entry ?< "." mma-mode-syntax-table)
  (modify-syntax-entry ?= "." mma-mode-syntax-table)
  (modify-syntax-entry ?> "." mma-mode-syntax-table)
  (modify-syntax-entry ?| "." mma-mode-syntax-table)
  (modify-syntax-entry ?_ "." mma-mode-syntax-table)
  (modify-syntax-entry ?\' "." mma-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" mma-mode-syntax-table))

(defvar mma-mode-map ()
  "Key map to use in mma mode.")

(if mma-mode-map
    ()
  (setq mma-mode-map (make-sparse-keymap))
  (define-key mma-mode-map [?\C-c ?i] 'mma-debug-insert)
  (define-key mma-mode-map [?\C-c ?d] 'mma-debug-on)
  (define-key mma-mode-map [?\C-c ?o] 'mma-debug-off)
  (define-key mma-mode-map [?\C-c ?c] 'comment-region))

(defcustom mma-indentation 2
  "*The width for further indentation in mma mode."
  :type 'integer
  :initialize 'custom-initialize-default
  :group 'mathematica)

;;;(make-regexp '("If" "While" "Print" "Module" "With" "Block" "Switch" 
;;;		  "Return""Begin" "End" "BeginPackage" "EndPackage"
;;;		  "Which" "Do" "For" "Throw" "Catch" "Check" "Break" 
;;;		  "Continue" "Goto" "Label" "Abort" "Message"))
;;;;;;;;

(defvar mma-font-lock-keywords-1
  (list
   '("\\(^[a-zA-Z]\\w*\\)\\([ \t]*=[ \t]*Compile\\|\\[\\([ \t]*\\]\\|.*\\(_\\|:\\)\\)\\)"
     1 font-lock-function-name-face)

   '("\\<\\(Abort\\|B\\(egin\\(\\|Package\\)\\|lock\\|reak\\)\\|C\\(atch\\|heck\\|ontinue\\)\\|Do\\|End\\(\\|Package\\)\\|For\\|Goto\\|If\\|Label\\|M\\(essage\\|odule\\)\\|Print\\|Return\\|Switch\\|Throw\\|W\\(hi\\(ch\\|le\\)\\|ith\\)\\)\\>" 1 font-lock-keyword-face)
   ;; a missing `;' in Mathematica code may cause magic effects. So 
   ;; highlight it:
   '("\\(;\\)" 1 font-lock-keyword-face))
  "Subdued level highlighting for mma mode.")

(defvar mma-font-lock-keywords-2
  (append
   mma-font-lock-keywords-1
   '(
     ("\\(\\(-\\|:\\)>\\|//[.@]?\\|/[.@;:]\\|@@\\|#\\(#\\|[0-9]*\\)\\|&\\)" 1 font-lock-keyword-face append)
     ("([*]:[a-zA-Z-]*:[*])" 0 font-lock-keyword-face t) 
;;; This pattern is just for internal use...
;;;     ("([*]\\(:FILE-ID:\\).*:[*])" 1 font-lock-keyword-face t)
     ("\\(!=\\|=\\(!=\\|==?\\)\\)" 1 font-lock-reference-face)))

   "Gaudy level highlighting for mma mode.")

(defvar mma-font-lock-keywords mma-font-lock-keywords-1
  "Default expressions to highlight in mma mode.")

;;;; - Indentation.
(defcustom mma-indent-align-at-eolp nil
  "*If non-nil, `mma-indent-line' aligns to provious lines' tab-stops. 
Specifies if `mma-indent-line' indents by steps of `mma-indentation' (value
is nil) or aligns to whitespace (value is non-nil) if point is at the end
of line. See also documentation of `mma-indent-line'."
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'mathematica)

(defun mma-indent-line (&optional midline-does-tab)
  "Indent as far as preceding non-empty line, then by steps of `mma-indentation'.
If point is on the leading whitespace of the current line, indents as far as
the preceding non-empty line and moves point to the first non-whitespace
character of the line.
If point is not on the leading whitespace of the current line, indents by
steps of `mma-indentation'.
If called with prefix argument, aligns rest of current line with the next
whitespace character position in previous lines:

    ---------- Buffer: foo.m ----------
    MyOptionName -> MyValue
    Another-!--> Value
    ---------- Buffer: foo.m ----------

Calling with prefix argument yields:

    ---------- Buffer: foo.m ----------
    MyOptionName -> MyValue
    Another      -!--> Value
    ---------- Buffer: foo.m ----------

(Difficult to explain. Just try it and see the result.)
This behaviour is especially useful for aligning Options[]-specifications.
If called with negative prefix argument, aligns with whitespace in following
lines.
The variable `mma-indent-align-at-eolp' specifies if `mma-indent-line' indents
by steps of `mma-indentation' (value is nil) or aligns to whitespace (value
is non-nil) if point is at the end of line."
  (interactive "P")
  (let* (found
	 (previous (save-excursion
		     (while (and (not found)
				 (= (forward-line -1) 0))
		       (back-to-indentation)
		       (if (not (eolp))
			   (setq found t)))
		     (if found
			 (current-column)
		       0)))
	 (point (point))
	 move-to
	 current)
    
    (setq found nil)
    (save-excursion
      (cond 
       ;; newline-and-indent
       ((eq this-command 'newline-and-indent)
	(indent-to previous))
       
       ;; 'pre
       ((or (< (current-column)
	       (setq current (progn (back-to-indentation)
				    (current-column))))
	    (and (= (current-column) 0)
		 (= current 0)
		 (> previous 0)))
	(delete-region (point)
		       (progn (beginning-of-line) (point)))
	(indent-to previous))
       ;; now point is at the current indentation

       ;; empty line
       ((eolp)
	(indent-to (max previous (* (1+ (/ current mma-indentation))
				    mma-indentation))))
       
       ;; end of line or mid-of-line and prefix
       ;; (note the side-effect of the progn!)
       ((or (and (progn  (goto-char point)
			 (setq current (current-column))
			 (skip-chars-forward " \t")
			 (eolp))
		 mma-indent-align-at-eolp)
	    midline-does-tab)
	;; if we are in the middle of a word the following delete-region
	;; is a nop.
	(delete-region point (point))
	(goto-char point)
	(save-excursion
	  (let ((search-dir (if (> (prefix-numeric-value midline-does-tab) 0)
				-1
			      1)))
	    (while (and (not found)
			(= (forward-line search-dir) 0))
	      (move-to-column current)
	      (if (eolp)
		  ()
		(skip-chars-forward "^ \t\n")
		(skip-chars-forward " \t")
		(if (not (eolp))
		    (setq found t))))
	    (setq move-to (current-column))))
	(indent-to (if found
		       move-to
		     (max previous (* (1+ (/ current mma-indentation))
				      mma-indentation))))
	(setq move-to (point)))
       ;; now (point) is at point
       
       ;; middle of the line
       (t
	(setq current (progn (back-to-indentation) (current-column)))
	(delete-region (point)
		       (progn (beginning-of-line) (point)))
	(indent-to (* (1+ (/ current mma-indentation)) mma-indentation)))))

    (if move-to
	(goto-char move-to)
      (if (< (current-column) (current-indentation))
	  (skip-chars-forward " \t")))))



;;;; - functionmenu / imenu support

(defconst mma-function-name-regexp-1
  "^\\([a-zA-Z0-9]+\\)"
  "Very generic regexp to match any potiential Mathematica function.")

(defconst mma-function-name-regexp-2
  "\\[[ \t]*\\(\\][ \t]*:=\\|[a-zA-Z0-9,{}? ]*\\(_\\|:\\)\\)"
  "Regexp which matches Mathematica functions with definition at
same line.")

(defconst mma-function-name-regexp-3
  "[ \t]*Compile\\["
  "Regexp which matches Mathematica Compile-Functions.")

(defconst mma-function-name-regexp-4
  "\\[[ \t]*\\([a-zA-Z0-9_{}? ]+\\).*\\^:="
  "Regexp which matches Mathematica up-values.")

(cond
 ;; set up function menu support only if running on XEmacs
 ((eq mma-emacs-flavor 'xemacs)

(require 'func-menu)

(setq fume-function-name-regexp-alist 
      (append fume-function-name-regexp-alist
	      '((mma-mode . mma-function-name-regexp-1))))

(defun fume-find-next-mma-function-name (buffer)
  "Scan buffer for MMA function definitions.

Scans buffer BUFFER for MMA function definitions.
Used for variable `fume-find-function-name-method-alist' to provide
functionmenu support for MMA buffers.

For MMA upvalues, the name of the inner symbol (i.e. the symbol where the
definition is attached) is returned. To distinguish upvalues from normal
function definitions, the character ^ as prepended to the name. Thus, the
upvalue definition

    MyFunction[ MySymbol ] ^:= definition

is added to the menu as ^MySymbol."
  (set-buffer buffer)
  ;; Search for the function
  (let (beg end search-end res)
    (while (and (not res)
		(re-search-forward mma-function-name-regexp-1 nil t))
      (end-of-line)
      (setq search-end (point))
      (beginning-of-line)
      (setq beg (point))
      (forward-sexp)
      (setq end (point))
      ;; Check for upvalue (i.e. Rule[___] ^:=)
      ;; This check has to be done BEFORE check for regular functions!
      (if (re-search-forward mma-function-name-regexp-4 search-end t)
	  (setq res (cons (concat "^" (match-string 1)) beg))
	;; Search for regular functions
	(if (re-search-forward mma-function-name-regexp-2 search-end t)
	    (setq res (cons (buffer-substring beg end) beg))
	  ;; Search for Compiled functions on the same line
	  (if (re-search-forward mma-function-name-regexp-3 search-end t)
	      (setq res (cons (buffer-substring beg end) beg))
	    ;; Search for Compiled function on several lines
	    (forward-line 1)
	    (save-excursion
	      (end-of-line)
	      (setq search-end (point)))
	    (if (re-search-forward mma-function-name-regexp-3 
				   search-end t)
		(setq res (cons (buffer-substring beg end) beg)))))))
    res))

(set-default 'fume-find-function-name-method-alist
      (append fume-find-function-name-method-alist
	      '((mma-mode . fume-find-next-mma-function-name))))

) 

 ;; set up imenu support only if running on Emacs
 ((eq mma-emacs-flavor 'emacs)

(require 'cl)

(defun mma-imenu-create-index (&optional regexp)
  "Scan buffer for MMA function definitions.

Scans the current buffer for MMA function definitions and returns a list
of name/marker pairs as required by `imenu'.
Used for variable `imenu-create-index-function' to provide imenu support
for MMA buffers.

For MMA upvalues, the name of the inner symbol (i.e. the symbol where the
definition is attached) is returned. To distinguish upvalues from normal
function definitions, the character ^ as prepended to the name. Thus, the
upvalue definition

    MyFunction[ MySymbol ] ^:= definition

is added to the menu as ^MySymbol."
  (let ((index-alist '())
	 prev-pos char)
    (goto-char (point-min))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (save-match-data
      (while (re-search-forward mma-function-name-regexp-1 nil t)
	 (let (beg end search-end res)
	   (imenu-progress-message prev-pos)
	   (end-of-line)
	   (setq search-end (point))
	   (beginning-of-line)
	   (setq beg (if imenu-use-markers (point-marker) (point)))
	   (forward-sexp)
	   (setq end (point))
	   ;; Check for upvalue (i.e. Rule[___] ^:=)
	   ;; This check has to be done BEFORE check for regular functions!
	   (if (re-search-forward mma-function-name-regexp-4 search-end t)
	       (setq res (cons (concat "^" (match-string 1)) beg))
	     ;; Search for regular functions
	     (if (re-search-forward mma-function-name-regexp-2 search-end t)
		 (setq res (cons (buffer-substring beg end) beg))
	       ;; Search for Compiled functions on the same line
	       (if (re-search-forward mma-function-name-regexp-3 search-end t)
		   (setq res (cons (buffer-substring beg end) beg))
		 ;; Search for Compiled function on several lines
		 (forward-line 1)
		 (save-excursion
		   (end-of-line)
		   (setq search-end (point)))
		 (if (re-search-forward mma-function-name-regexp-3 
					search-end t)
		     (setq res (cons (buffer-substring beg end) beg))))))
	   (if res 
		 (push res index-alist))))
	 (imenu-progress-message prev-pos 100)
	 (nreverse index-alist))))

)

)  ; matches (cond ((eq mma-emacs-flavor 'xemacs) ...


;;;;  -- debug support
(defcustom mma-debug-start-1 ": DEBUG "
  "*First part of the string marking the beginning of debug code.

The whole string marking the beginning of debug code is given by

  (*<start-1><marker-name><start-2>*)

where START-1 is the value of `mma-debug-start-1' and START-2 is the value
of `mma-debug-start-2'."
  :type 'string
  :group 'mathematica)

(defcustom mma-debug-start-2 " :"
  "*Second part of the string marking the beginning of debug code.

The whole string marking the beginning of debug code is given by

  \"(*<start-1><marker-name><start-2>*)\"
where START-1 is the value of `mma-debug-start-1' and START-2 is the value
of `mma-debug-start-2'."
  :type 'string
  :group 'mathematica)

(defcustom mma-debug-end ": ENDDEBUG :" 
  "*String marking the end of debug code.

The whole string marking the end of debug code is given by

  \"(*<end>*)\"

where END is the value of `mma-debug-end'."
  :type 'string
  :group 'mathematica)

(defcustom mma-debug-fontify-block t
  "*If non-nil, `mma-debug-on' and `mma-debug-off' fontify buffer after changes."
  :type 'boolean
  :group 'mathematica)

(defun mma-debug-on-off (pattern debugon)
  "Turn debug output on or off in current buffer.
If DEBUGON is non-nil, turns on the debug output matching PATTERN.
If DEBUGON is nil, debug output matching PATTERN is turned off.
If region is active, restrict action to region."
  (let ((marker (make-marker))
	beg end
	err string
        endmarker)
    (save-excursion
      (if (mma-mark-active)
	  (if (< (point) (mark))
	      (setq beg (point)
		    end (mark))
	    (setq beg (mark)
		  end (point)))
	(setq beg (point-min)
	      end (point-max)))
      (goto-char beg)
      (set-marker marker end)
      (while (and (not err)
		  (re-search-forward (concat "(\\*" mma-debug-start-1
					     "\\(.*\\)" mma-debug-start-2
					     "\\*)")
				     (marker-position marker) t))
	(setq string (match-string 1))
	(if (and (eq (string-match pattern string) 0)
		 (eq (match-end 0) (length string)))
	    (progn
	      (if (looking-at "(\\*")
		  (if debugon 
		      (kill-region (point) (+ 2 (point))))
		(if (not debugon)
		    (insert "(*")))
	      (if (re-search-forward (concat "(\\*" mma-debug-end "\\*)")
				     (marker-position marker) t)
		  (progn
		    (goto-char (match-beginning 0))
		    (setq endmarker
			  (save-excursion
			    (backward-char 2)
			    (looking-at "\\*)")))
		    (if debugon
			(if endmarker
			    (kill-region (- (point) 2) (point)))
		      (if (not endmarker)
			  (insert "*)"))))
		(setq err t))))))
    (set-marker marker nil)
    (and mma-debug-fontify-block
	 (boundp 'font-lock-mode)
	 font-lock-mode
	 (mma-debug-fontify))))

(defun mma-debug-on (pattern)
  "Turn on debug code in current buffer.
Any debug code matching the regexp PATTERN is turned on.
If region is active, restrict action to region."
  (interactive "sTurn debug on for: ")
  (mma-debug-on-off pattern t))

(defun mma-debug-off (pattern)
  "Turn off debug code in whole buffer.
Any debug code matching the regexp PATTERN is turned off.
If region is active, restrict action to region."
  (interactive "sTurn debug off for: ")
  (mma-debug-on-off pattern nil))

(defun mma-debug-insert (marker)
  "Insert debug code skeleton.
Inserts a debug code skeleton marked MARKER at point.
See variables `mma-debug-start-1', `mma-debug-start-2', and `mma-debug-end'
how the skeleton is cunstructed.
Uses `indent-for-tab-command' to align the lines of the skeleton."
  (interactive "sMarker: ")
  (insert "(*" mma-debug-start-1)
  (insert marker)
  (insert mma-debug-start-2 "*)\n")
  (indent-for-tab-command)
  (save-excursion
    (insert "\n")
    (indent-for-tab-command)
    (insert "(*" mma-debug-end "*)")))


;;;; - mma-mode
(defun mma-mode ()
  "Mathematica-Mode.
Programming mode for writing Mathematica package files.
It provides a simple indentation machine, font-lock support, function-menu
support for XEmacs, imenu support for Emacs, and some functions for
maintaining debug code.
Turning on mma-mode runs the hook `mma-mode-hook'.
\\{mma-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mma-mode-map)
  (setq mode-name "Mma")
  (setq major-mode 'mma-mode)
  (set-syntax-table mma-mode-syntax-table)
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "(\\*+ *")
  (set (make-local-variable 'comment-column) 48)
  ;; parse-sexp-ignore-comments should be set to nil. Otherwise matching
  ;; paren highlighting does not work properly.
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'indent-line-function) 'mma-indent-line)
  (setq indent-tabs-mode nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'((mma-font-lock-keywords
	   mma-font-lock-keywords-1
	   mma-font-lock-keywords-2)
	  nil nil ((?_ . "w")) beginning-of-defun
	  (font-lock-comment-start-regexp . "^([*]\\|[ \t]([*]")))
  ;; if on Emacs, initialize imenu index creation function
  (cond
   ((eq mma-emacs-flavor 'emacs)
    (set (make-local-variable 'imenu-create-index-function)
	 'mma-imenu-create-index)))
  (run-hooks 'mma-mode-hook))

(provide 'mma)

;;; Local Variables: ***
;;; page-delimiter: "^\\(\\|;;{{{\\)" ***
;;; End: ***
