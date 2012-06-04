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

;; Personal information
(setq user-mail-address "jrblevin@sdf.org")

;; Set the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Include MacPorts binaries in the path
(push "/opt/local/bin" exec-path)
(setenv "PATH" (concat  "/opt/local/bin:" (getenv "PATH")))

;; Disable the menu bar, scroll bar, and toolbar.
(if (boundp 'menu-bar-mode)
    (menu-bar-mode 0))
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; Font Selection
(set-default-font "Inconsolata-18")

;; Disable transient-mark-mode
(setq transient-mark-mode nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Blinking cursor
(blink-cursor-mode 1)

;; Tabs versus Spaces: http://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil)
(setq tab-width 8)

;; Synchronize Emacs kill buffer with X clipboard.
(setq x-select-enable-clipboard t)

;; Store backup files in one place.  Do the same for auto save files.
;; http://inamidst.com/config/emacs
(if (file-directory-p "~/.emacs.d/backup")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (message "Directory does not exist: ~/.emacs.d/backup"))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-files/\\1" t)))

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

;; Disable startup screen
(setq inhibit-startup-message t)

;; Make it hard to accidentally kill Emacs
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "<ns-power-off>"))

;; Start the Emacs server
(server-start)
(setq server-kill-new-buffers t)

;;; Edit with Emacs:

(if (and (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (edit-server-start)))

;;; Color Themes:

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (setq color-theme-is-global nil)
     (require 'color-theme-subdued)
     (require 'color-theme-less)
     (require 'color-theme-gruber-darker)
     (require 'color-theme-twilight)))

;; Selects the appropriate color theme for each frame based on whether
;; the client is running in console mode or windowed mode.
(defun my-select-color-theme(frame)
  (select-frame frame)
  (if (window-system frame)
      (color-theme-twilight)
    (color-theme-less)))

;; Hook to run after making a new frame
(add-hook 'after-make-frame-functions 'my-select-color-theme)

;;; Global keybindings:

(global-set-key [f3] 'my-insert-date-time)
(global-set-key [f4] 'revert-buffer-no-confirm)
(global-set-key [f5] 'skeleton-webpage-header)
(global-set-key [f6] 'calendar)
(global-set-key [f7] 'markdown-mode)
(global-set-key [f8] 'deft)
(global-set-key [f9] 'compile)
(global-set-key [f10] 'deft-today)
(global-set-key [f11] 'gtd-make-next-action)
(global-set-key [f12] 'gtd-mark-next-action-complete)

(global-set-key [?\M-j] 'fill-sentence)
(global-set-key [\M-down] 'move-line-down)
(global-set-key [\M-up] 'move-line-up)

;;; Ispell:

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;;; Markdown:

(setq markdown-enable-math t)
(setq markdown-command "peg-markdown")
(setq markdown-link-space-sub-char "-")

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown formatted text files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))

(defun my-markdown-mode-hook ()
  (flyspell-mode 0)                     ; turn off flyspell-mode
  (auto-fill-mode 1))			; turn on auto-fill-mode
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;;; Deft:

(require 'deft)
(setq deft-text-mode 'markdown-mode)
(setq deft-directory "~/gtd/")
(setq deft-auto-save-interval 0.5)

(defun deft-today ()
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (deft-new-file-named today)
    (beginning-of-buffer)
    (unless (looking-at (concat "^" today))
      (insert today "\n\n"))))

;;; GTD:

(defconst gtd-next-action-regex
  "^- \\(.*?\\) \\((\\[\\[\\(.+?\\)\\]\\])\\)$"
  "Regular expression matching incomplete next actions.")

(defun gtd-mark-next-action-complete ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward gtd-next-action-regex nil t)
      (let ((beg (match-beginning 0))
            (project (match-string 3))
            (date (format-time-string "(%Y-%m-%d)")))
        (message (concat "project: " project))
        (replace-match (concat "+ \\1 " date) nil nil)
        (beginning-of-line)
        (kill-whole-line)
        (deft-open-file (concat deft-directory project "." deft-extension))))))

(defun gtd-make-next-action ()
  (interactive)
  (beginning-of-line)
  (insert "-  ([[")
  (insert (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (insert "]])\n")
  (forward-line -1)
  (forward-char 2))

;;; Fortran:

(autoload 'f90-mode "f90"
  "Major mode for editing Fortran code in free form." t)
(setq auto-mode-alist (cons '("\\.f03$" . f90-mode) auto-mode-alist))
(add-hook 'f90-mode-hook 'my-f90-mode-hook)

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
(setq TeX-PDF-mode t
      TeX-parse-self t
      TeX-auto-save nil
      TeX-source-specials-mode t
      font-latex-match-slide-title-keywords '("foilhead")
      TeX-view-program-list
      (quote
       (("open" "open %o")))
      TeX-view-program-selection
      (cond
       ((eq system-type (quote darwin))
        (quote
         ((output-dvi "open")
          (output-pdf "open")
          (output-html "open"))))
       (t
        (quote
         ((output-dvi "xdvi")
          (output-pdf "Evince")
          (output-html "xdg-open"))))))

(defun my-TeX-mode-hook-fn ()
  "Function added to `TeX-mode-hook'."
  (LaTeX-math-mode 1)
  (flyspell-mode))

(add-hook 'TeX-mode-hook 'my-TeX-mode-hook-fn)

;;; BibTeX:

(defun my-bibtex-mode-hook-fn ()
  "Function added to `bibtex-mode-hook'."
  (setq fill-column 77)
  (setq bibtex-user-optional-fields
        '(("tags" "Entry tags")))
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-autokey-name-year-separator "-")
  (setq bibtex-autokey-year-title-separator "-")
  (setq bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an"))
  (setq bibtex-autokey-titleword-length 30)
  (setq bibtex-autokey-titlewords 1)
  (setq bibtex-contline-indentation 17))

(add-hook 'bibtex-mode-hook 'my-bibtex-mode-hook-fn)

;;; GNU Octave:

(autoload 'octave-mode "octave-mode" "Enter Octave mode" t)
;(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

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

(setq auto-mode-alist
      (cons '("\\.leg$" . c-mode) auto-mode-alist))

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

(require 'time-stamp)
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-format "%:b %:d, %:y %02H:%02M %Z")
(setq time-stamp-start "\\(Time-stamp:[ \t]+\\\\?[\"<]+\\|Last Modified:[ \t]+\\|@modified[ ]+\\|^modified:[ \t]+\\)")
(setq time-stamp-end "\\(\n\\|\\\\?[\">]\\)")
(setq time-stamp-line-limit 10)

(defun my-insert-year ()
  "Insert the current year."
  (interactive "*")
  (insert (format-time-string "%Y")))

(defun my-insert-date ()
  "Insert the current date."
  (interactive "*")
  (insert (format-time-string "%B %e, %Y")))

(defun my-insert-date-time ()
  "Insert the current date and time."
  (interactive "*")
  (insert (format-time-string "%B %e, %Y %02H:%02M %Z")))

(defun my-insert-rfc-3339 ()
  "Insert the current date and time formatted according to RFC 3339."
  (interactive "*")
  (insert (format-time-string "%Y-%02m-%02dT%02H:%02M:%02SZ" (current-time) t)))

;;; Miscellaneous:

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun fill-sentence ()
  (interactive)
  (save-excursion
    (unless (bolp)
      (forward-sentence -1))
    (let ((beg (point)))
      (forward-sentence)
      (fill-region-as-paragraph beg (point)))))

;; Line movement functions by Michael Schuerig.

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))

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

;;; post-mode:

;; (defun post-custom()
;;   "post-hook"
;;   (load "mutt-alias")
;;   (setq mutt-alias-file-list '("~/.mutt-aliases"))
;;   (local-set-key "\C-ci" 'mutt-alias-insert)
;;   (setq post-underline-pattern nil)
;;   (setq post-emoticon-pattern nil)
;;   (setq post-bold-pattern nil)
;;   (flyspell-mode 1))
;; (add-hook 'post-mode-hook '(lambda() (post-custom)))

;;; Mutt:

;; (setq auto-mode-alist (cons '("mutt-" . post-mode) auto-mode-alist))

;;; Skeleton Templates:

(define-skeleton skeleton-webpage-header
  "Insert a metadata header for pages on jblevins.org."
  nil
  "title:       " (skeleton-read "Title: ") "\n"
  "description: " (skeleton-read "Description: ") "\n"
  "created:     " (my-insert-date-time) "\n"
  "city:        Columbus\n"
  "markup:      markdown\n"
  "guid:        tag:jblevins.org," (my-insert-year) ":"
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
