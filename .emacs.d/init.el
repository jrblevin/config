;;; ~/.emacs: Emacs configuration file.                     -*-emacs-lisp-*-
;;
;; Jason Blevins <jrblevin@sdf.org>
;; Raleigh, May 29, 2004

;;; Directory Structure:
;;
;; ~/.emacs                      init file
;; ~/.emacs.d/                   user directory
;; ~/.emacs.d/backup             single location for backup files
;; ~/.emacs.d/site-lisp          packages

;;; Emacs X Resources:
;;
;; Emacs.menuBar:                off
;; Emacs.verticalScrollBars:     off
;; Emacs.toolBar:                off
;; Emacs.internalBorder:         1
;; Emacs.geometry:               80x35+5+5
;; Emacs.FontBackend:            xft
;; Emacs.font:                   Inconsolata-15

;;; Basic Configuration:

;; Disable the scroll bar, toolbar, tooltips, etc.
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Skip to scratch buffer, with no message
(setq initial-scratch-message nil)

;; Set the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Personal information
(setq user-mail-address "jrblevin@sdf.org")

;; Add directories to the path
(push "/opt/local/bin" exec-path)
(push "/usr/local/bin" exec-path)
(setenv "PATH" (concat "/opt/local/bin:/usr/local/bin" (getenv "PATH")))

;; Package management
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; System-specific configuration
(cond
 ((eq system-type 'darwin)
  ;; Menu bar takes up no additional space in OS X.
  (menu-bar-mode 1)
  ;; Default Latin font
  (set-face-attribute 'default nil :family "Source Code Pro")
  ;; Default font size (point * 10)
  (set-face-attribute 'default nil :height 180)
  ;; Default variable pitch font
  (set-face-attribute 'variable-pitch nil :family "Avenir Next")))

;; Font Selection
(set-frame-font "Source Code Pro-18")
;; (set-frame-font "Inconsolata 18")
;; (set-frame-font "Anonymous Pro 16")

;; Disable transient-mark-mode
(setq transient-mark-mode nil)

;; Highlight current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode 1))

;; Blinking cursor
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 1))

;; Tabs versus Spaces: http://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil)
(setq tab-width 8)

;; Synchronize Emacs kill buffer with X clipboard.
(setq x-select-enable-clipboard t)

;; Store backup files in one place.  Do the same for auto save files.
;; http://www.emacswiki.org/emacs/AutoSave
(defconst emacs-tmp-dir
  (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;; Show matching parentheses.
(show-paren-mode 1)

;; Show the date and time in 24-hour format.
(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)

;; Show the column number in the mode line.
(column-number-mode 1)

;; Use debian-sensible-browser as generic browser
(setq browse-url-generic-program "debian-sensible-browser")

;; Don't print a header
(setq ps-print-header nil)

;; Suppress beeps
(setq visible-bell t)

;; Allow typing y or n instead of typing yes and no in full
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make it hard to accidentally kill Emacs
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "<ns-power-off>"))

;; Start the Emacs server
(server-start)
(setq server-kill-new-buffers t)

;;; Edit with Emacs Chrome extension:

(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

;;; Frame geometry:

;; Set frame geometry according to display resolution.
;;
;; Based on a function by Bryan Oakley on StackOverflow:
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window

(when (display-graphic-p)
  ;; Width: 93 columns for large displays, 80 columns for small ones.
  (if (> (x-display-pixel-width) 1280)
         (add-to-list 'default-frame-alist (cons 'width 93))
         (add-to-list 'default-frame-alist (cons 'width 80)))
  ;; Height: subtract from screen height (for panels, menubars, etc.)
  ;; and divide by the height of a character to get the number of lines.
  (add-to-list 'default-frame-alist
               (cons 'height (/ (- (x-display-pixel-height) 50)
                                (frame-char-height)))))

;;; Margins:

;; Set margins to center content in window.
;; http://stackoverflow.com/questions/24955253/centre-emacs-buffer-within-window

;; (defun my-resize-margins ()
;;  (let ((margin-size (/ (- (frame-width) 100) 2)))
;;    (set-window-margins nil margin-size margin-size)))

;; (add-hook 'window-configuration-change-hook #'my-resize-margins)
;; (my-resize-margins)


;;; Color Themes:

(require 'color-theme)
(eval-after-load "color-theme"
 '(progn
    (color-theme-initialize)
    ;(require 'color-theme-subdued)
    ;(require 'color-theme-gruber-darker)
    ;(require 'color-theme-less)
    ;(require 'color-theme-twilight)
    ;(color-theme-midnight)
    (color-theme-twilight)))

;;; Global keybindings:

(global-set-key [f3] 'my-insert-date-time)
(global-set-key [f4] 'revert-buffer-no-confirm)
(global-set-key [f5] 'my-quick-log)
(global-set-key [f6] 'calendar)
(global-set-key [f7] 'markdown-mode)
(global-set-key [f8] 'deft)
(global-set-key [f9] 'compile)
(global-set-key [f10] 'deft-today)

(global-set-key [?\M-j] 'fill-sentences)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key [\C-\M-down] 'move-line-down)
(global-set-key [\C-\M-up] 'move-line-up)

(global-set-key (kbd "C-h C-r") 'describe-char)
(global-set-key (kbd "C-x C-g") 'deft-find-file)

(global-set-key (kbd "C-c d") 'deft)
(global-set-key (kbd "C-c D") 'deft-today)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c g") 'deft-find-file)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c l") 'my-quick-log)
(global-set-key (kbd "C-c p") 'magit-push)
(global-set-key (kbd "C-c t") 'time-stamp)
(global-set-key (kbd "C-c T") 'titlecase-dwim)

;;; Ispell:

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-tex-skip-alists
      '((("\\\\addcontentsline" ispell-tex-arg-end 2)
         ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
         ("\\\\\\([aA]lph\\|arabic\\)" ispell-tex-arg-end)
         ("\\\\author" ispell-tex-arg-end)
         ("\\\\cite\\(t\\|p\\|year\\|yearpar\\)" ispell-tex-arg-end)
         ("\\\\autoref" ispell-tex-arg-end)
         ("\\\\eqref" ispell-tex-arg-end)
         ("\\\\label" ispell-tex-arg-end)
         ("\\\\bibliographystyle" ispell-tex-arg-end)
         ("\\\\makebox" ispell-tex-arg-end 0)
         ("\\\\e?psfig" ispell-tex-arg-end)
         ("\\\\document\\(class\\|style\\)" .
          "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
        (;; delimited with \begin.
         ("\\(figure\\|table\\)\\*?" ispell-tex-arg-end 0)
         ("list" ispell-tex-arg-end 2)
         ("program" . "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
         ("verbatim\\*?" . "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}"))))

;;; Markdown:

(setq markdown-command "multimarkdown")
(setq markdown-open-command "mark")
(setq markdown-link-space-sub-char "-")
(setq markdown-footnote-location 'end)
(setq markdown-reference-location 'header)
(setq markdown-live-preview-delete-export 'delete-on-destroy)
(setq markdown-css-paths '("/Applications/Marked 2.app/Contents/Resources/Lopash.css"))
(setq org-table-automatic-realign nil); for MultiMarkdown tables

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown formatted text files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/gtd/.*\\.txt\\'" . markdown-mode))

(defun my-markdown-mode-hook ()
  (save-excursion
    (when (re-search-forward "^math:\\s-*itex$" nil t)
      (markdown-enable-math 1))
    ;;(writegood-mode t)
    ;;(turn-on-flyspell)
    ))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(defun my-gfm-mode-hook ()
  (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)

(defun markdown-reload ()
  (interactive)
  (when (eq major-mode 'markdown-mode)
    (fundamental-mode))
  (when (featurep 'markdown-test) (unload-feature 'markdown-test))
  (when (featurep 'markdown-mode) (unload-feature 'markdown-mode))
  (load-library (expand-file-name "~/projects/markdown-mode/markdown-mode.el"))
  (load-library (expand-file-name "~/projects/markdown-mode/tests/markdown-test.el"))
  (markdown-mode))

;;; Deft:

(require 'deft)
(setq deft-directory "~/gtd/")
(setq deft-auto-save-interval 2)
(setq deft-recursive t)
(setq deft-extensions '("txt" "text" "tex" "org"))
(setq deft-use-filter-string-for-filename t)
;;(setq deft-use-filename-as-title t)
(setq deft-file-naming-rules '((nospace . "-")
                               (case-fn . downcase)))
(deft)

(defun deft-today ()
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (filename (concat deft-directory today ".txt"))
         (deft-filter-regexp nil))
    (if (file-exists-p filename)
        (deft-open-file filename t t)
      (deft-new-file-named today)
      (goto-char (point-min))
      (unless (looking-at (concat "^" today))
        (insert today "\n\n<!-- #pending -->\n\n")))))

(defun deft-reload ()
  (interactive)
  (quit-window)
  (load-library "/Users/jblevins/projects/deft/deft.el")
  (deft))

;;; magit

(require 'magit)

;;; git-commit-mode:

(autoload 'git-commit-mode "git-commit-mode"
  "Major mode for editing Git commit messages" t)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . git-commit-mode))
(add-to-list 'auto-mode-alist '("/NOTES_EDITMSG\\'" . git-commit-mode))
(add-to-list 'auto-mode-alist '("/MERGE_MSG\\'" . git-commit-mode))
(add-to-list 'auto-mode-alist '("/TAG_EDITMSG\\'" . git-commit-mode))

;;; titlecase:

(require 'titlecase)

;;; GTD:

;; (require 'taskpaper-mode)

;; (defconst gtd-next-action-regex
;;   "^[x-] \\([^@]*\\) @\\([a-z0-9-]+\\)[ \t]*\\( @\\(due\\|remind\\|reminded\\)([0-9-]+ [0-9:]+)\\)?$"
;;   "Regular expression matching incomplete next actions.")

;; (defun gtd-mark-next-action-complete ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (when (re-search-forward gtd-next-action-regex nil t)
;;       (let ((beg (match-beginning 0))
;;             (project (match-string 2))
;;             (date (format-time-string "@done(%Y-%m-%d)")))
;;         (replace-match (concat "+ \\1\\3 " date) nil nil)
;;         (beginning-of-line)
;;         (kill-whole-line)
;;         (let ((deft-filter-regexp nil))
;;           (deft-open-file (concat deft-directory project ".txt")))
;;         (goto-char (point-min))
;;         (when (re-search-forward "^## Completed" nil t)
;;           (forward-line 2)
;;           (yank)
;;           (exchange-point-and-mark))))))

;; (defun gtd-open-todo ()
;;   (interactive)
;;   (find-file-other-window "~/gtd/todo.taskpaper")
;;   (taskpaper-mode))

;;; todotxt-mode:

;; (require 'todotxt)
;; (setq todotxt-file "~/gtd/todo.txt")
;; (todotxt)

;; (defun todotxt-gtd-complete ()
;;   (interactive)
;;   (setq inhibit-read-only 't)
;;   (gtd-mark-next-action-complete)
;;   (todotxt-prioritize-items)
;;   (setq inhibit-read-only nil)
;;   (save-buffer))

;; (defun todotxt-insert-item (item)
;;   "Prompt for an item to add to the todo list and append it to
;; the file, saving afterwards."
;;   (interactive "sItem to add: ")
;;   (setq inhibit-read-only 't)
;;   (beginning-of-line)
;;   (insert (concat item "\n"))
;;   (todotxt-prioritize-items)
;;   (save-buffer)
;;   (setq inhibit-read-only nil)
;;   (todotxt-jump-to-item item))

;; (defun todotxt-delete-item ()
;;   "Delete the item on the current line."
;;   (interactive)
;;   (setq inhibit-read-only 't)
;;   (beginning-of-line)
;;   (let ((beg (point)))
;;     (forward-line 1)
;;     (delete-region beg (point)))
;;   (todotxt-prioritize-items)
;;   (save-buffer)
;;   (setq inhibit-read-only nil))

;; (defun todotxt-move (n)
;;   "Move the current item up or down by N lines."
;;   (interactive "p")
;;   (setq inhibit-read-only 't)
;;   (setq col (current-column))
;;   (beginning-of-line) (setq start (point))
;;   (end-of-line) (forward-char) (setq end (point))
;;   (let ((line-text (delete-and-extract-region start end)))
;;     (forward-line n)
;;     (insert line-text)
;;     ;; restore point to original column in moved line
;;     (forward-line -1)
;;     (forward-char col))
;;   (save-buffer)
;;   (setq inhibit-read-only nil))

;; (defun todotxt-move-up (n)
;;   "Move the current item up by N lines."
;;   (interactive "p")
;;   (todotxt-move (if (null n) -1 (- n))))

;; (defun todotxt-move-down (n)
;;   "Move the current item down by N lines."
;;   (interactive "p")
;;   (todotxt-move (if (null n) 1 n)))

;; (defun todotxt-sort-by-context ()
;;   "Sort items by context."
;;   (interactive)
;;   (setq inhibit-read-only 't)
;;   (let ((max (save-excursion
;;                (goto-char (point-max))
;;                (skip-syntax-backward "-")
;;                (point))))
;;     (sort-regexp-fields nil "^.*?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} .*\\|@[^ ]+ \\+[^ ]+\\)$" "\\1" (point-min) max)
;;     (save-buffer)
;;     (setq inhibit-read-only nil)))

;; (defun todotxt-deft-open-project ()
;;   (interactive)
;;   (when (re-search-forward " \\+\\([^ ]+\\)$" nil t)
;;       (deft-open-file (concat deft-directory (match-string 1) ".txt") 1)))

;; (define-key todotxt-mode-map (kbd "C") 'todotxt-gtd-complete) ; (C)omplete item
;; (define-key todotxt-mode-map (kbd "I") 'todotxt-insert-item) ; (I)nsert item
;; (define-key todotxt-mode-map (kbd "D") 'todotxt-delete-item) ; (D)elete item
;; (define-key todotxt-mode-map (kbd "N") 'todotxt-move-down) ; Move (N)ext
;; (define-key todotxt-mode-map (kbd "P") 'todotxt-move-up) ; Move (P)revious
;; (define-key todotxt-mode-map (kbd "S") 'todotxt-sort-by-context) ; (S)ort by context
;; (define-key todotxt-mode-map (kbd "o") 'todotxt-deft-open-project) ; (O)pen

;; (defconst gtd-next-action-regex
;;   "^- \\(.*?\\) \\((\\[\\[\\(.+?\\)\\]\\])\\)$"
;;   "Regular expression matching incomplete next actions.")

;; (defun gtd-mark-next-action-complete ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (when (re-search-forward gtd-next-action-regex nil t)
;;       (let ((beg (match-beginning 0))
;;             (project (match-string 3))
;;             (date (format-time-string "(%Y-%m-%d)")))
;;         (message (concat "project: " project))
;;         (replace-match (concat "+ \\1 " date) nil nil)
;;         (beginning-of-line)
;;         (kill-whole-line)
;;         (deft-open-file (concat deft-directory project "." deft-extension))))))

;; (defun gtd-make-next-action ()
;;   (interactive)
;;   (beginning-of-line)
;;   (insert "-  ([[")
;;   (insert (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
;;   (insert "]])\n")
;;   (forward-line -1)
;;   (forward-char 2))

;;; Website:

(defun my-quick-log (slug)
  (interactive "sSlug: ")
  (find-file (concat "~/projects/jblevins.org/htdocs/log/" slug ".text"))
  (skeleton-webpage-header))

;;; Fortran:

(autoload 'f90-mode "f90"
  "Major mode for editing Fortran code in free form." t)
(add-to-list 'auto-mode-alist '("\\.f03\\'" . f90-mode))
(add-hook 'f90-mode-hook 'my-f90-mode-hook)
(add-to-list 'completion-ignored-extensions ".mod")

(defun my-f90-mode-hook ()
  (setq f90-beginning-ampersand nil
	f90-font-lock-keywords f90-font-lock-keywords-3
	comment-column 50)
  ;; Make Backslash non-special (not an escape character).
  ;; With newer versions of f90.el, use `f90-backslash-not-special`.
  (when (equal (char-syntax ?\\ ) ?\\ )
    (modify-syntax-entry ?\\ "."))
  (define-abbrev f90-mode-abbrev-table "f90h" "" 'skeleton-f90-header)
  (abbrev-mode 1)			; turn on abbreviation mode
  (turn-on-font-lock)			; for highlighting
  (auto-fill-mode 0))			; turn off auto-filling

;;; AUCTeX:

(load "auctex.el" nil t t)
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '(".aux" ".nav" ".bbl" ".blg" ".dvi" ".brf" ".snm" ".toc"
                ".fls" ".rel" "_region_." ".fdb_latexmk" ".synctex.gz")))
(setq TeX-PDF-mode t
      TeX-parse-self t
      TeX-auto-save nil
      TeX-source-specials-mode t
      font-latex-match-slide-title-keywords '("foilhead")
      TeX-view-program-list
      (quote
       (("Preview" "/usr/bin/open -a Preview.app %o")
        ;; Skim's displayline is used for forward search (from .tex to .pdf)
        ;; option -b highlights the current line;
        ;; option -g opens Skim in the background;
        ;; option -r asks Skim to revert the file
        ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b")))
      TeX-view-program-selection
      (cond
       ((eq system-type (quote darwin))
        (quote
         ((output-dvi "Skim")
          (output-pdf "Skim")
          (output-html "Skim"))))
       (t
        (quote
         ((output-dvi "xdvi")
          (output-pdf "Evince")
          (output-html "xdg-open"))))))

;;; RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(defun my-remove-Biber ()
  "Remove Biber command"
  (delete-if (lambda (item) (string-equal "Biber" (car item))) TeX-command-list))

(defun my-TeX-mode-hook-fn ()
  "Function added to `TeX-mode-hook'."
  (turn-on-flyspell)
  (my-remove-Biber)
  (LaTeX-math-mode 1))
(add-hook 'TeX-mode-hook 'my-TeX-mode-hook-fn)

;; make latexmk available via C-c C-c
;; Use Command-Shift-click to reverse search in Skim.
;; See http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -g -synctex=1 -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)
  (push
    '("bibtool" "bibtool -x %s.aux > %s.bib" TeX-run-TeX nil t
      :help "Run bibtool on aux file to produce bib file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;;; BibTeX:

(defun my-bibtex-mode-hook-fn ()
  "Function added to `bibtex-mode-hook'."
  (setq fill-column 77)
  (setq bibtex-user-optional-fields
        '(("keywords" "Entry keywords")))
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-autokey-name-year-separator "-")
  (setq bibtex-autokey-year-title-separator "-")
  (setq bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an"))
  (setq bibtex-autokey-titleword-length 30)
  (setq bibtex-autokey-titlewords 1)
  (setq bibtex-contline-indentation 17))

(add-hook 'bibtex-mode-hook 'my-bibtex-mode-hook-fn)

;;; Graphviz

(setq graphviz-dot-indent-width 4)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter Graphviz dot mode." t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

;;; Matlab

(setq matlab-auto-fill nil)
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

;;; Mathematica:

(autoload 'mma-mode "mma" "Mathematica package file mode" t)
;(setq auto-mode-alist (cons '("\\.m\\'" . mma-mode) auto-mode-alist))

;;; Perl:

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 2)
  (setq cperl-extra-newline-before-brace t))

;;; C and C++:

(defun my-c-mode-common-hook ()
  (c-set-style "k&r")
  (setq c-basic-offset 4)
  (setq compilation-window-height 8)
  (c-toggle-auto-hungry-state -1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-to-list 'auto-mode-alist '("\\.leg\\'" . c-mode))

;; Close the compilation window after clean compile.
;; From http://www.bloomington.in.us/~brutt/emacs-c-dev.html.
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "no compilation errors"))))

;;; IDO mode:

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".tex" ".bib" ".sty" ".f90" ".txt" ".text" ".el"))
(setq ido-ignore-extensions t)
(ido-mode t)

;;; CSS:

(setq cssm-indent-function #'cssm-c-style-indenter)

;;; Emacs Speaks Statistics:

(require 'ess-site)

;;; ado-mode:

(defun ado-custom()
  "ado-mode-hook"
  (setq ado-claim-name "Jason Blevins")
  (setq ado-signature-file "~/.emacs.d/.ado-signature")
  (setq ado-site-template-dir "/usr/local/share/emacs/ado-mode/templates/")
  (setq ado-date-format "%Y-%m-%d"))
(add-hook 'ado-mode-hook 'ado-custom)

;;; Timestamps:

;; (require 'time-stamp)
;; (add-hook 'write-file-hooks 'time-stamp)
;; (setq time-stamp-active t)
;; (setq time-stamp-format "%:b %:d, %:y %02H:%02M %Z")
;; (setq time-stamp-start "\\(Time-stamp:[ \t]+\\\\?[\"<]+\\|Last Modified:[ \t]+\\|@modified[ ]+\\|^modified:[ \t]+\\)")
;; (setq time-stamp-end "\\(\n\\|\\\\?[\">]\\)")
;; (setq time-stamp-line-limit 10)

(defun my-insert-year ()
  "Insert the current year."
  (interactive "*")
  (insert (format-time-string "%Y")))

(defun my-insert-date ()
  "Insert the current date."
  (interactive "*")
  (insert (format-time-string "%B %e, %Y")))

(defun my-insert-date-iso ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert (format-time-string "%Y-%m-%d")))

(defun my-insert-date-time ()
  "Insert the current date and time."
  (interactive "*")
  (insert (format-time-string "%B %e, %Y %02H:%02M %Z")))

(defun my-insert-rfc-3339 ()
  "Insert the current date and time formatted according to RFC 3339."
  (interactive "*")
  (insert (format-time-string "%Y-%02m-%02dT%02H:%02M:%02SZ" (current-time) t)))

;;; Miscellaneous:

;; unix-file, dos-file, and mac-file from
;; http://www.emacswiki.org/emacs/EndOfLineTips
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun fill-sentences ()
  "Fill paragraph at point, breaking lines at sentence boundaries."
  (interactive)
  (save-excursion
    (let ((end-marker (make-marker)))
      (set-marker end-marker (progn
                               (forward-paragraph)
                               (skip-syntax-backward "-")
                               (point)))
      (forward-paragraph -1)
      (while (and (< (point) end-marker)
                  (not (eobp)))
        (save-excursion
          (fill-region-as-paragraph (point) end-marker))
        (forward-sentence)
        (unless (>= (point) end-marker)
          (delete-horizontal-space)
          (unless (looking-at "\n") (insert "\n")))))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; From http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun make-row-vector (begin end)
  (interactive "*r")
  (save-excursion
    (let ((a (make-marker))
          (b (make-marker)))
      (set-marker a begin)
      (set-marker b end)
      (goto-char a)
      (while (re-search-forward "\n" b t)
        (replace-match ", " nil nil))
      (goto-char a)
      (while (re-search-forward "[ ]+" b t)
        (replace-match " " nil nil)))))

;; Open files in dired mode using 'open'
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;; Line movement functions by Michael Schuerig.

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

;; Prevent killing the *scratch* buffer
;; http://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


;;; Calendar and Diary:

;; See the following:
;;
;; http://www.dotemacs.de/dotfiles/JasonRumney.emacs.html
;; http://www.zvon.org/other/elisp/Output/index.html
;; http://www.coling.uni-freiburg.de/~schauer/resources/emacs/config/calendar-stuff.el.html
;; http://www.charlescurley.com/emacs.init.html

;; Add ISO format to date formats allowed in diary
;; (require 'diary-lib)
;; (require 'calendar)

;; (setq diary-file (expand-file-name "~/.diary")
;;       calendar-week-start-day 1
;;       mark-diary-entries-in-calendar t
;;       mark-holidays-in-calendar t
;;       view-diary-entries-initially nil
;;       mark-diary-entries-in-calendar t
;;       number-of-diary-entries 7)

;; (add-hook 'diary-display-hook 'fancy-diary-display)
;; (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;; (add-hook 'list-diary-entries-hook 'sort-diary-entries t)

;; (setq diary-date-forms
;;       '((year "-" month "-" day "[^/0-9]")
;;         (month "/" day "[^/0-9]")
;;         (month "/" day "/" year "[^0-9]")
;;         (monthname " *" day "[^,0-9]")
;;         (monthname " *" day ", *" year "[^0-9]")
;;         (dayname "\\W")))
;; (setq calendar-date-display-form
;;       (quote ((format "%04s-%02d-%02d" year (string-to-int month)
;;                       (string-to-int day)))))
;; (setq calendar-time-display-form
;;       (quote (24-hours ":" minutes (if time-zone " (")
;;                        time-zone (if time-zone ")"))))

;;; AMPL:

;; (setq auto-mode-alist
;;       (cons '("\\.mod$" . ampl-mode) auto-mode-alist))
;; (setq auto-mode-alist
;;       (cons '("\\.dat$" . ampl-mode) auto-mode-alist))
;; (setq auto-mode-alist
;;       (cons '("\\.ampl$" . ampl-mode) auto-mode-alist))
;; (setq interpreter-mode-alist
;;       (cons '("ampl" . ampl-mode)
;;             interpreter-mode-alist))

;; (autoload 'ampl-mode "ampl-mode" "AMPL editing mode." t)

;;; SES:

; (autoload 'ses-mode "ses.el" "Spreadsheet mode" t)
; (add-to-list 'auto-mode-alist '("\\.ses$" . ses-mode))

;;; Org mode:

;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;;; Lua mode:

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; post-mode:

;; (require 'post)
;; (defun post-custom()
;;   "post-hook"
;;   (load "mutt-alias")
;;   (setq mutt-alias-file-list '("~/.mutt-aliases"))
;;   (local-set-key "\C-ci" 'mutt-alias-insert)
;;   (setq post-uses-fill-mode nil)
;;   (setq post-underline-pattern nil)
;;   (setq post-emoticon-pattern nil)
;;   (setq post-bold-pattern nil)
;;   (setq post-signature-pattern "\\(--\\|Cheers,\\|Thanks,\\|Best,\\|\\|Sent from my\\)")
;;   (setq post-kill-quoted-sig nil)
;;   (setq post-signature-source-is-file t)
;;   (setq post-fixed-signature-source "~/.signature")
;;   (setq post-email-address user-mail-address)
;;   ;; Remove trailing whitespace (but protect '-- ' in signature)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (while (re-search-forward "\\([>:]\\)\s+$" nil t)
;;       (replace-match (match-string 1) nil nil)))
;;   ;; Rewrite "Last, First" addresses
;;   (save-excursion
;;     (re-search-forward "\n\n")
;;     (let ((end (point)))
;;       ;;(message body)
;;       (beginning-of-buffer)
;;       (while (re-search-forward "\"\\([[:alpha:]]+\\), \\([[:alpha:]]+\\)\"" end t)
;;         (replace-match (concat (match-string 2) " " (match-string 1)) nil nil))))
;;   (post-goto-body))
;; (add-hook 'post-mode-hook '(lambda() (post-custom)))

;;; Mutt:

;; (add-to-list 'auto-mode-alist '("mutt-" . post-mode))

;;; muttrc-mode

(autoload 'muttrc-mode "muttrc-mode.el" "Major mode to edit muttrc files" t)
(add-to-list 'auto-mode-alist '(".muttrc\\'" . muttrc-mode))
(add-to-list 'auto-mode-alist '(".mutt-aliases\\'" . muttrc-mode))

;;; Abbreviations

(defun my-abbrev-expand (expand)
  "Expands macros in the text EXPAND.  Replaces %clipboard with
most recent kill ring contents and leaves the cursor at %|."
  (let (text index len)
    (setq text (replace-in-string expand "%clipboard" (car kill-ring)))
    (setq len (length text))
    (setq index (string-match "%|" text))
    (cond
     (index
      (setq text (replace-in-string text "%|" ""))
      (setq len (- len 2)))
     (t
      (setq index len)))
    (insert text)
    (forward-char (- index len))))

(define-abbrev-table 'global-abbrev-table '(
  ;; date expansions
  ("ddate" "" my-insert-date 0)
  ("iiso" "" my-insert-date-iso 0)

  ;; email addresses
  ("eem1" "jrblevin@sdf.org" nil 0)
  ("eem2" "blevins.141@osu.edu" nil 0)

  ;; signatures
  ("ssig1" "Best,\n\nJason" nil 0)
  ("ssig2" "Best,\n\nProf. Blevins" nil 0)
  ("ssig3" "Best,\n\nJason\n\n-- \nJason R. Blevins\nAssistant Professor of Economics\nThe Ohio State University\nhttp://jblevins.org/\n" nil 0)

  ;; common phrases
  ("afaict" "as far as I can tell" nil 0)
  ("afaik" "as far as I know" nil 0)
  ("btw" "by the way" nil 0)
  ("ito" "it turns out" nil 0)
  ("tyvm" "Thank you very much!" nil 0)

  ;; email shortcuts
  ("ssorry" "I'm very sorry for the delay in getting back to you." nil 0)

  ;; autocorrect
  ("adn" "and" nil 0)
  ("alot" "a lot" nil 0)
  ("ehre" "here" nil 0)
  ("esle" "else" nil 0)
  ("haev" "have" nil 0)
  ("hvae" "have" nil 0)
  ("htp:" "http:" nil 0)
  ("knwo" "know" nil 0)
  ("konw" "know" nil 0)
  ("libary" "library" nil 0)
  ("liek" "like" nil 0)
  ("mkae" "make" nil 0)
  ("recieve" "receive" nil 0)
  ("smoe" "some" nil 0)
  ("soem" "some" nil 0)
  ("taht" "that" nil 0)
  ("teh" "the" nil 0)
  ("tractible" "tractable" nil 0)
  ("wnat" "want" nil 0)
  ("wether" "whether" nil 0)
  ("wtih" "with" nil 0)
  ("yoru" "your" nil 0)
  ("yuor" "your" nil 0)
  ))

;; Don't ask whether to save new abbrevs
(setq save-abbrevs nil)

;; Turn on abbrev mode globally
(setq-default abbrev-mode t)


;;; Skeleton Templates:

(define-skeleton skeleton-webpage-header
  "Insert a metadata header for pages on jblevins.org."
  nil
  "title: " (skeleton-read "Title: ") "\n"
  "description: " (skeleton-read "Description: ") "\n"
  "created: " (my-insert-date-time) "\n"
  "city: Columbus\n"
  "markup: markdown\n"
  "feed: true\n"
  "guid: tag:jblevins.org," (my-insert-year) ":"
  (replace-regexp-in-string
   "/\\(home\\|Users\\)/jblevins/projects/jblevins.org/htdocs\\(.*?\\)\\(main\\)*\\.text" "\\2"
   (buffer-file-name))
  "\n\n")

(define-skeleton skeleton-f90-header
  "Insert a file header for Fortran source code."
  "Description: "
  "! " (buffer-name) " --- " str "\n"
  "!\n"
  "! Copyright (C) " (my-insert-year) " Jason R. Blevins <jrblevin@sdf.org>\n"
  "! All rights reserved.\n"
  "!\n"
  "! Redistribution and use in source and binary forms, with or without\n"
  "! modification, are permitted provided that the following conditions are met:\n"
  "! 1. Redistributions of source code must retain the above copyright\n"
  "!    notice, this list of conditions and the following disclaimer.\n"
  "! 2. Redistributions in binary form must reproduce the above copyright\n"
  "!    notice, this list of conditions and the following disclaimer in the\n"
  "!    documentation  and/or other materials provided with the distribution.\n"
  "! 3. Neither the names of the copyright holders nor the names of any\n"
  "!    contributors may be used to endorse or promote products derived from\n"
  "!    this software without specific prior written permission.\n"
  "!\n"
  "! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"
  "! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"
  "! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"
  "! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE\n"
  "! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"
  "! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"
  "! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"
  "! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"
  "! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"
  "! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"
  "! POSSIBILITY OF SUCH DAMAGE.\n"
  "\n")
