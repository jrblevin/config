;;; ~/.emacs.d/init.el: Emacs configuration
;;
;; Jason Blevins <jrblevin@sdf.org>
;; Raleigh, May 29, 2004

;; Directory Structure:
;;
;; ~/.emacs.d/                   user directory
;; ~/.emacs.d/init.el            init file
;; ~/.emacs.d/init-local.el      additional private init file
;; ~/.emacs.d/custom.el          customized variables and faces
;; ~/.emacs.d/site-lisp          manually installed packages
;; ~/.emacs.d/themes             custom themes

(defconst emacs-start-time (current-time))


;;; Basic Configuration:

;; Platform-specific configuration
(defsubst jrb-mac-or-not (mac not)
  "Return MAC if system is a Mac and NOT otherwise."
  (if (eq system-type 'darwin) mac not))

;; Configure GUI elements quickly (scroll bar, tool bar, and menu bar)
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode (jrb-mac-or-not 1 0)))

;; Default fonts (face height is 10 * point size)
(set-face-attribute 'default nil :family "Fira Code" :weight 'light
                    :height (jrb-mac-or-not 180 150))
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
(set-face-attribute 'variable-pitch nil :family "Avenir Next")

;; Set frame geometry according to display resolution.
;; Width: 93 columns for large displays, 80 columns for small ones.
;; Height: subtract from screen height (for panels, menubars, etc.)
;; and divide by the height of a character to get the number of lines.
;; <http://stackoverflow.com/questions/92971/>
(when (display-graphic-p)
  (setq initial-frame-alist
        (list (cons 'top 1) (cons 'left 1)
              (cons 'width (if (> (x-display-pixel-width) 1280) 93 80))
              (cons 'height (/ (- (x-display-pixel-height) 50)
                               (frame-char-height))))))

;; Highlight current line, blink cursor
(if (fboundp 'global-hl-line-mode) (global-hl-line-mode 1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 1))

(setq inhibit-splash-screen     t       ; Disable splash screen
      initial-scratch-message   nil     ; No scratch buffer message
      transient-mark-mode       nil     ; Disable transient-mark-mode
      select-enable-clipboard   t       ; Sync kill ring and clipboard
      scroll-step               1       ; Smooth scrolling
      scroll-margin             3       ; Some context when recentering
      column-number-mode        1       ; Show column number in mode line
      split-height-threshold    nil     ; Window splitting thresholds
      split-width-threshold     140     ; Minimum side-by-side split is 70 char
      visible-bell              t)      ; Suppress beeps

;; Set the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/")

;; Personal information
(setq user-mail-address "jrblevin@sdf.org")

;; Browser
(setq browse-url-generic-program (jrb-mac-or-not nil "debian-sensible-browser"))

;; Tabs versus Spaces: http://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil)
(setq tab-width 8)

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

;; Allow typing y or n instead of typing yes and no in full
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make it hard to accidentally kill Emacs
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "<ns-power-off>"))

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Load libraries
(require 'cl-lib)


;;; Package management

;; Load `package.el'
(eval-when-compile (require 'package))
(setq package-enable-at-startup nil
      package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(eval-when-compile (package-initialize t))

;; Bootstrap `use-package'
(setq package-selected-packages '(use-package))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents))
(package-install-selected-packages)
(require 'use-package)
(setq use-package-verbose t)


;;; Color Themes:

(setq custom-theme-directory "~/.emacs.d/themes")
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (if (member hour (number-sequence 6 17))
      (use-package twilight-theme
        :init (load-theme 'twilight 'no-confirm))
    (use-package color-theme-sanityinc-tomorrow
      :ensure t
      :init (load-theme 'sanityinc-tomorrow-night 'no-confirm))))


;;; Global keybindings:

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)

(global-set-key (kbd "<f3>") 'my-insert-date-time)
(global-set-key (kbd "<f4>") 'revert-buffer-no-confirm)
(global-set-key (kbd "<f5>") 'my-quick-log)
(global-set-key (kbd "<f6>") 'calendar)
(global-set-key (kbd "<f7>") 'markdown-mode)
(global-set-key (kbd "<f8>") 'deft)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f10>") 'jrb-write-mode)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f12>") 'jrb-dual-mode)

(global-set-key [?\M-j] 'fill-sentences)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key [\C-\M-down] 'move-line-down)
(global-set-key [\C-\M-up] 'move-line-up)

(global-set-key (kbd "C-h C-r") 'describe-char)
(global-set-key (kbd "C-x C-g") 'deft-find-file)

(global-set-key (kbd "C-c d") 'deft)
(global-set-key (kbd "C-c D") 'deft-today)
(global-set-key (kbd "C-c M") 'deft-tomorrow)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c l") 'my-quick-log)
(global-set-key (kbd "C-c o") 'send-region-to-omnifocus)
(global-set-key (kbd "C-c f") 'send-region-to-fantastical)


;;; auto-minor-mode-alist

;; From <https://github.com/vermiculus/dotfiles/blob/master/.emacs.d/my-packages/auto-minor-mode.el>

(defvar auto-minor-mode-alist nil
  "Alist of filename patterns and minor mode functions.
See `auto-mode-alist'.  All elements of this alist are checked,
meaning you can enable multiple minor modes for the same
regexp.")

(defun enable-minor-mode-based-on-extension ()
  "Check file name against all pairs in `auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)


;;; Simple package configuration

(defsubst hook-into-modes (func &rest modes)
  "Add FUNC to hook functions given by MODES."
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(use-package ado-mode
  :disabled t
  :defer t
  :load-path "site-lisp/ado-mode/lisp"
  :config
  (setq ado-claim-name "Jason Blevins"
        ado-signature-file "~/.emacs.d/.ado-signature"
        ado-site-template-dir "~/.emacs.d/site-lisp/ado-mode/templates/"
        ado-date-format "%Y-%m-%d"))

(use-package ampl-mode
  :disabled t
  :mode (("\\.ampl\\'" . ampl-mode)
         ("\\.mod\\'" . ampl-mode)
         ("\\.dat\\'" . ampl-mode))
  :interpreter ("ampl" . ampl-mode))

(use-package bibtex
  :defer t
  :config
  (setq bibtex-user-optional-fields '(("keywords" "Entry keywords"))
        bibtex-autokey-names 5
        bibtex-autokey-name-separator "-"
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-year-length 4
        bibtex-autokey-titleword-ignore '("The" "A" "If" "An")
        bibtex-autokey-titleword-length 30
        bibtex-autokey-titlewords 1
        bibtex-contline-indentation 17
        bibtex-align-at-equal-sign t))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config
  (setq company-backends
        '((company-dabbrev-code company-abbrev company-capf)
          (company-files company-keywords)))
  (global-company-mode))

(use-package compile
  :config
  (defun jrb-autoclose-compile-window (buffer string)
    (cond
     ((string-match "finished" string)
      (message "Build seemed successful: closing window.")
      (run-with-timer 2 nil 'delete-window (get-buffer-window buffer t)))
     (t
      (message "Compilation exited abnormally: %s" string))))
  (setq compilation-finish-functions 'jrb-autoclose-compile-window)
  (setq compilation-window-height 15))

(use-package elisp-mode
  :defer t
  :init
  (defun jrb-emacs-lisp-hook ()
    ;; Skip past ^L, headings marked by `;;;`, and subsequent comments
    ;; <http://endlessparentheses.com/improving-page-navigation.html>
    (make-local-variable 'page-delimiter)
    (setq page-delimiter
          (rx bol (or "\f" ";;;")
              (not (any "#")) (* not-newline) "\n"
              (* (* blank) (opt ";" (* not-newline)) "\n"))))
  (add-hook 'emacs-lisp-mode-hook 'jrb-emacs-lisp-hook))

(use-package ess-site
  :load-path "/opt/local/share/emacs/site-lisp/ess/"
  :commands R
  :mode (("\\.a?do\\'" . stata-mode)
         ("\\.[Rr]\\'" . r-mode)
         ("\\.[Rr]out\\'" . r-transcript-mode)
         ("\\.[Ss][Aa][Ss]\\'" . sas-mode))
  :init
  (defun jrb-ess-mode-hook()
    (when (string-equal ess-language "STA")
      (define-key ess-mode-map (kbd "_") nil)))
  (add-hook 'ess-mode-hook 'jrb-ess-mode-hook))

(use-package exec-path-from-shell
  :ensure t
  :if (jrb-mac-or-not t nil)
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :defer 5
  :config
  (setq flycheck-gfortran-warnings (list "all" "extra" "surprising")
        flycheck-gfortran-language-standard "f2008"
        flycheck-gfortran-include-path (list "." ".." "/Users/jblevins/projects/osl/"))
  :init
  (hook-into-modes #'flycheck-mode 'f90-mode-hook 'emacs-lisp-mode-hook))

(use-package flyspell
  :defer 5
  :diminish flyspell-mode
  :init
  (use-package ispell
    :config
    ;; ignore LaTeX commands and environments
    (setq ispell-tex-skip-alists
          (list
           (append (car ispell-tex-skip-alists)
                   '(("\\\\cite" ispell-tex-arg-end)
                     ("\\\\nocite" ispell-tex-arg-end)
                     ("\\\\includegraphics" ispell-tex-arg-end)
                     ("\\\\author" ispell-tex-arg-end)
                     ("\\\\ref" ispell-tex-arg-end)
                     ("\\\\eqref" ispell-tex-arg-end)
                     ("\\\\pageref" ispell-tex-arg-end)
                     ("\\\\label" ispell-tex-arg-end)
                     ("\\\\mathit" ispell-tex-arg-end)
                     ("\\\\mathrm" ispell-tex-arg-end)
                     ("\\\\url" ispell-tex-arg-end)
                     ("\\\\lstinputlisting" ispell-tex-arg-end 2)
                     ("\\\\mint" ispell-tex-arg-end 2)
                     ("\\\\inputminted" ispell-tex-arg-end 2)
                     ("\\\\code" ispell-tex-arg-end 2)
                     ("\\\\example" ispell-tex-arg-end 2)))
           (append (cadr ispell-tex-skip-alists)
                   '(("tabular" ispell-tex-arg-end))
                   (mapcar
                    (lambda (env)
                      (cons env (format "\\\\end[ \t\n]*{[ \t\n]*%s[ \t\n]*}" env)))
                    '("equation\\*" "minted" "listing" "lstlisting" "pre" "interface")))))
    (setq ispell-extra-args '("--sug-mode=ultra")))
  ;; spell-checking in text modes and in comments for programming modes
  (hook-into-modes #'flyspell-prog-mode
                   'prog-mode-hook
                   'nxml-mode-hook)
  (hook-into-modes #'turn-on-flyspell
                   'LaTeX-mode-hook
                   'markdown-mode-hook)
  (hook-into-modes #'flyspell-buffer
                   'LaTeX-mode-hook
                   'markdown-mode-hook))

(use-package f90
  :mode (("\\.[Ff]\\(?:90\\|95\\|03\\|08\\|15\\)\\'" . f90-mode)
         ("\\.inc\\'" . f90-mode))
  :config
  (defun jrb-f90-mode-hook ()
    (setq f90-beginning-ampersand nil
          f90-font-lock-keywords f90-font-lock-keywords-3
          comment-column 50)
    (make-local-variable 'completion-ignored-extensions)
    (add-to-list 'completion-ignored-extensions ".mod")
    ;; Make Backslash non-special (not an escape character).
    ;; With newer versions of f90.el, use `f90-backslash-not-special`.
    (when (equal (char-syntax ?\\ ) ?\\ )
      (modify-syntax-entry ?\\ "."))
    (define-abbrev f90-mode-abbrev-table "`rw" "real(wp)")
    (define-abbrev f90-mode-abbrev-table "f90h" "" 'skeleton-f90-header)
    (abbrev-mode 1)			; turn on abbreviation mode
    (turn-on-font-lock)			; for highlighting
    (auto-fill-mode 0))
  (add-hook 'f90-mode-hook 'jrb-f90-mode-hook))

(use-package git-messenger
  :commands git-messenger:popup-message
  :bind ("C-x g !" . git-messenger:popup-message)
  :init (setq git-messenger:show-detail t))

(use-package hippie-expand
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))

(use-package graphviz-dot-mode
  :ensure t :defer t
  :config (setq graphviz-dot-indent-width 4))

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))

(use-package ido
  :config
  (setq ido-enable-flex-matching t
        ido-file-extensions-order '(".tex" ".bib" ".sty" ".f90" ".txt" ".text" ".el")
        ido-ignore-extensions t
        ido-everywhere t)
  :init
  (ido-mode t))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

(use-package magit
  :bind (("C-x g b" . magit-blame)
         ("C-x g c" . magit-commit)
         ("C-x g d" . magit-diff)
         ("C-x g l" . magit-log-buffer-file)
         ("C-x g p" . magit-push)
         ("C-x g s" . magit-status))
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode))))

(use-package mma
  :commands mma-mode)

(use-package matlab
  :commands (matlab-mode matlab-shell)
  :mode (("\\.m\\'" . matlab-mode))
  :config (setq matlab-auto-fill nil))

(use-package mmm-mode
  :defer t
  :bind (("C-c m" . mmm-parse-buffer))
  :commands (mmm-mode mmm-parse-buffer)
  :config
  (setq mmm-global-mode nil)
  (setq mmm-parse-when-idle nil)
  (setq mmm-submode-decoration-level 0)

  (defun jrb-mmm-markdown-auto-class (lang &optional submode)
    (let ((class (intern (concat "markdown-" lang)))
          (submode (or submode (intern (concat lang "-mode"))))
          (front (concat "^``` ?" lang "[\n\r]+"))
          (back "^```"))
      (mmm-add-classes (list (list class :submode submode :front front :back back)))
      (mmm-add-mode-ext-class 'markdown-mode nil class)))

  (defun jrb-mmm-latex-auto-class (lang &optional submode)
    (let ((class (intern (concat "latex-" lang)))
          (submode (or submode (intern (concat lang "-mode"))))
          (front (concat "^\\\\begin{\\(pre\\|interface\\)}{" lang "}[\n\r]+"))
          (back "^\\\\end{\\(pre\\|interface\\)}"))
      (mmm-add-classes (list (list class :submode submode :front front :back back)))
      (mmm-add-mode-ext-class 'latex-mode nil class)))

  ;; Set up modes for cases where names match
  (mapc 'jrb-mmm-markdown-auto-class
        '("awk" "bibtex" "c" "cpp" "css" "emacs-lisp"
          "html" "latex" "lisp" "makefile"
          "markdown" "python" "r" "ruby" "sql" "stata" "xml" "octave"))

  ;; Mode names that differ from language names
  (jrb-mmm-markdown-auto-class "bib" 'bibtex-mode)
  (jrb-mmm-markdown-auto-class "fortran" 'f90-mode)
  (jrb-mmm-markdown-auto-class "perl" 'cperl-mode)
  (jrb-mmm-markdown-auto-class "shell" 'shell-script-mode)
  (jrb-mmm-latex-auto-class "C" 'c-mode)
  (jrb-mmm-latex-auto-class "Fortran" 'f90-mode))

(use-package muttrc-mode
  :mode (("\\.muttrc\\'" . muttrc-mode)
         ("\\.mutt-aliases\\'" . muttrc-mode)))

(use-package org
  :mode (("\\.org\\'" . org-mode)))

(use-package rainbow-mode
  :defer 10
  :commands rainbow-mode
  :init
  (add-to-list 'auto-minor-mode-alist '("-theme\\.el\\'" . rainbow-mode))
  (add-to-list 'auto-minor-mode-alist '("\\.s?css\\'" . rainbow-mode))
  (setq rainbow-x-colors nil)
  (defun jrb-rainbow-mode-hook ()
    "Disable hl-line-mode when rainbow-mode is active."
    (setq-local global-hl-line-mode nil)
    (hl-line-mode -1))
  (add-hook 'rainbow-mode-hook 'jrb-rainbow-mode-hook))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init (hook-into-modes #'page-break-lines-mode 'emacs-lisp-mode-hook))

(use-package cc-mode
  :mode (("\\.leg\\'" . c-mode))
  :config
  (defun jrb-c-mode-common-hook ()
    (c-set-style "k&r")
    (setq c-basic-offset 4)
    (c-toggle-auto-hungry-state -1))
  (add-hook 'c-mode-common-hook 'jrb-c-mode-common-hook))

(use-package cperl-mode
  :mode (("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode))
  :config
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 2)
  (setq cperl-extra-newline-before-brace nil))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)))

(use-package server
  :config
  (setq server-kill-new-buffers t)
  (unless (server-running-p)
    (server-start)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package sublimity
  :load-path "site-lisp/sublimity"
  :config
  (require 'sublimity-attractive)
  (setq sublimity-attractive-centering-width 100)
  (sublimity-mode 1))

(use-package time-stamp
  :defer t
  :bind ("C-c t" . time-stamp)
  :init
  ;;(add-hook 'write-file-hooks 'time-stamp)
  :config
  (setq time-stamp-active t)
  (setq time-stamp-format "%:b %:d, %:y %02H:%02M %Z")
  (setq time-stamp-start "\\(Time-stamp:[ \t]+\\\\?[\"<]+\\|Last Modified:[ \t]+\\|@modified[ ]+\\|^modified:[ \t]+\\)")
  (setq time-stamp-end "\\(\n\\|\\\\?[\">]\\)")
  (setq time-stamp-line-limit 15))

(use-package titlecase
  :bind (("C-c T" . titlecase-dwim)))


;;; Margins:

;; Set margins to center content in window.
;; http://stackoverflow.com/questions/24955253/centre-emacs-buffer-within-window

;; (defun my-resize-margins ()
;;  (let ((margin-size (/ (- (frame-width) 100) 2)))
;;    (set-window-margins nil margin-size margin-size)))

;; (add-hook 'window-configuration-change-hook #'my-resize-margins)
;; (my-resize-margins)

;; jrb-write-mode
(defvar jrb-write-mode nil)
(defvar jrb-write-mode-width 100)
(define-minor-mode jrb-write-mode
  "Minor mode for distraction-free writing."
  :init-value nil
  :global t
  :variable jrb-write-mode
  :group 'editing-basics
  (cond ((not jrb-write-mode)
         (set-fringe-style nil)
         (visual-line-mode 0)
         (setq-default line-spacing nil)
         (setq line-spacing nil)
         (set-face-attribute 'default nil :height 180)
         (toggle-frame-fullscreen))
        (t
         (toggle-frame-fullscreen)
         (sleep-for 1)
         (visual-line-mode 1)
         (setq-default line-spacing 0.5)
         (setq line-spacing 0.5)
         (set-face-attribute 'default nil :height 240)
         (set-fringe-mode
          (/ (- (frame-pixel-width)
                (* jrb-write-mode-width (frame-char-width)))
             2)))))

;; jrb-dual-mode
(defvar jrb-dual-mode nil)
(defvar jrb-dual-mode-width 90)
(define-minor-mode jrb-dual-mode
  "Dual window mode for writing and previewing."
  :init-value nil
  :global t
  :variable jrb-dual-mode
  :group 'editing-basics
  (cond ((not jrb-dual-mode)
         (delete-other-windows)
         (set-fringe-style nil)
         (visual-line-mode 0)
         (setq-default line-spacing nil)
         (setq line-spacing nil)
         (set-face-attribute 'default nil :height 180)
         (toggle-frame-fullscreen))
        (t
         (toggle-frame-fullscreen)
         (delete-other-windows)
         (sleep-for 1)
         (visual-line-mode 1)
         (setq-default line-spacing 0.5)
         (setq line-spacing 0.5)
         (set-face-attribute 'default nil :height 240)
         (let ((half-width (/ (frame-pixel-width) 2)))
           (set-fringe-mode (/ (- half-width (* jrb-dual-mode-width (frame-char-width))) 2)))
         (split-window-right))))


;;; Markdown:
;; org-table in org-table.el :commands orgtbl-mode

(setq markdown-command "multimarkdown")
(setq markdown-open-command "mark")
(setq markdown-link-space-sub-char "-")
(setq markdown-footnote-location 'end)
(setq markdown-reference-location 'header)
(setq markdown-asymmetric-header t)
(setq markdown-live-preview-delete-export 'delete-on-destroy)
(setq markdown-css-paths '("/Applications/Marked 2.app/Contents/Resources/Lopash.css"))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown formatted text files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/gtd/.*\\.txt\\'" . markdown-mode))

(eval-after-load "markdown"
  '(progn
     (set-face-attribute 'markdown-header-face nil
                         :inherit font-lock-function-name-face :bold t
                         :family "variable-pitch")
     (set-face-attribute 'markdown-header-face-1 nil
                         :inherit markdown-header-face :height 1.8)
     (set-face-attribute 'markdown-header-face-2 nil
                         :inherit markdown-header-face :height 1.4)
     (set-face-attribute 'markdown-header-face-3 nil
                         :inherit markdown-header-face :height 1.2)))

(defun my-markdown-mode-hook ()
  ;; For MultiMarkdown tables
  (make-local-variable 'org-table-automatic-realign)
  (setq org-table-automatic-realign nil)
  ;; Enable math mode based on file metadata
  (save-excursion
    (when (re-search-forward "^math:\\s-*itex$" nil t)
      (markdown-enable-math 1))))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(defun my-gfm-mode-hook ()
  (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)

(defun markdown-reload ()
  "Reload markdown mode from source (for development)."
  (interactive)
  (when (eq major-mode 'markdown-mode)
    (fundamental-mode))
  (when (featurep 'markdown-test) (unload-feature 'markdown-test))
  (when (featurep 'markdown-mode) (unload-feature 'markdown-mode))
  (load-library (expand-file-name "~/projects/markdown-mode/markdown-mode.el"))
  (load-library (expand-file-name "~/projects/markdown-mode/tests/markdown-test.el"))
  ;;(setq debug-on-quit t)
  (markdown-mode))

(defun jrb-fortran-code-block-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n")
    (insert "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (goto-char beg)
    (insert "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {: lang=fortran }\n")))


;;; Deft:

(require 'deft)
(setq deft-directory "~/gtd/")
(setq deft-auto-save-interval 2)
(setq deft-recursive t)
(setq deft-extensions '("txt" "text" "tex" "taskpaper" "org"))
(setq deft-use-filter-string-for-filename nil)
(setq deft-markdown-mode-title-level 1)
(setq deft-use-filename-as-title nil)
(setq deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")
                               (case-fn . downcase)))
(setq deft-strip-summary-regexp
      (concat "\\("
              "[\n\t]" ;; blank
              "\\|^<!-- #pending -->"
              "\\|^<!-- #active -->"
              "\\|^#\\+OPTIONS:.*$" ;; org-mode metadata
              "\\|^#\\+AUTHOR:.*$" ;; org-mode-metadata
              "\\)"))

(defun deft-daily (iso)
  (interactive)
  (let ((filename (concat deft-directory iso ".txt"))
        (deft-filter-regexp nil))
    (if (file-exists-p filename)
        (deft-open-file filename t t)
      (deft-new-file-named iso)
      (goto-char (point-min))
      (unless (looking-at (concat "^" iso))
        (insert "# " iso "\n\n<!-- #pending -->\n\n")))))

(defun deft-today ()
  "Create or open a Deft note for today."
  (interactive)
  (deft-daily (format-time-string "%Y-%m-%d")))

(defun deft-tomorrow ()
  "Create or open a Deft note for tomorrow."
  (interactive)
  (deft-daily (format-time-string "%Y-%m-%d" (tomorrow-time))))

(defun deft-reload ()
  "Reload Deft from source (for development)."
  (interactive)
  (quit-window)
  (load-library "/Users/jblevins/projects/deft/deft.el")
  (deft))


;;; Website:

(defun my-quick-log (slug)
  (interactive "sSlug: ")
  (find-file (concat "~/projects/jblevins.org/htdocs/log/" slug ".text"))
  (skeleton-webpage-header))


;;; AUCTeX:

(load "auctex.el" nil t t)

(setq TeX-parse-self t)
(setq TeX-auto-save nil)
(setq TeX-command-default "latexmk")
(setq font-latex-match-slide-title-keywords '("foilhead"))

;; Exclude temporary files from completion
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '(".aux" ".nav" ".bbl" ".blg" ".dvi" ".brf" ".snm" ".toc"
                ".fls" ".rel" "_region_." ".fdb_latexmk" ".synctex.gz"
                ".minted")))

;; One-time TeX setup
(eval-after-load "tex"
  '(progn
     (use-package company-auctex
       :config
       (company-auctex-init))

     ;; make latexmk available via C-c C-c
     ;; Use Command-Shift-click to reverse search in Skim.
     ;; See http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
     (push
      '("latexmk" "latexmk -g -synctex=1 -pdf %s" TeX-run-TeX nil t
        :help "Run latexmk on file")
      TeX-command-list)

     ;; make bibtool available via C-c C-c
     (push
      '("bibtool" "bibtool -x %s.aux > %s.bib" TeX-run-TeX nil t
        :help "Run bibtool on aux file to produce bib file")
      TeX-command-list)

     ;; Remove Biber command (so that bib completes to BibTeX)
     (cl-delete-if (lambda (x) (string-equal "Biber" (car x))) TeX-command-list)

     ;; Viewers
     (setq TeX-view-program-list
           '(("Preview" "/usr/bin/open -a Preview.app %o")
             ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b")))
     (setq TeX-view-program-selection
           '((output-dvi "Skim") (output-pdf "Skim") (output-html "open")))))


(defun jrb-LaTeX-hook-fn ()
  (setq LaTeX-font-list (append
                         LaTeX-font-list
                         '((?\C-l "\\code{" "}")
                           (?c "\\code[C]{" "}")
                           (?f "\\code[Fortran]{" "}")
                           (?C "\\code[C]|" "|")
                           (?F "\\code[Fortran]|" "|"))))
  ;;(setq reftex-plug-into-AUCTeX t)
  ;;(turn-on-reftex)
  (LaTeX-math-mode 1))

(add-hook 'LaTeX-mode-hook 'jrb-LaTeX-hook-fn)

;; Custom code macro, interface and pre environments
(defvar jrb-LaTeX-env-code-language-history nil
  "History list of languages used in the current buffer in LaTeX code blocks.")

(defun jrb-LaTeX-env-code (environment)
  "Insert code environment with optional language"
  (let ((lang (completing-read
               "(Optional) Language: "
               (list "Fortran" "C" "Perl" "Python" "Emacs-Lisp" "Shell" "Make")
               nil 'confirm "Fortran"
               'jrb-LaTeX-env-code-language-history)))
    (LaTeX-insert-environment environment
                              (unless (zerop (length lang))
                                (concat TeX-grop lang TeX-grcl)))))

(defun jrb-LaTeX-setup-code ()
  (TeX-add-style-hook
   "code"
   (lambda ()
     ;; New symbols
     (TeX-add-symbols
      '("code" TeX-arg-verb))

     ;; New environments
     (LaTeX-add-environments
      '("pre" jrb-LaTeX-env-code)
      '("interface" jrb-LaTeX-env-code))

     ;; Filling
     (make-local-variable 'LaTeX-indent-environment-list)
     (add-to-list 'LaTeX-indent-environment-list
                  '("pre" current-indentation))
     (add-to-list 'LaTeX-indent-environment-list
                  '("interface" current-indentation))
     (make-local-variable 'LaTeX-verbatim-regexp)
     (setq LaTeX-verbatim-regexp
           (concat LaTeX-verbatim-regexp "\\|pre\\|interface"))
     (add-to-list 'LaTeX-verbatim-environments-local "pre")
     (add-to-list 'LaTeX-verbatim-environments-local "interface")
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "code")
     (add-to-list 'LaTeX-verbatim-macros-with-braces-local "code")

     ;; Fontification
     (when (and (fboundp 'font-latex-add-keywords)
                (fboundp 'font-latex-set-syntactic-keywords)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("code" "[{")) 'textual)
       ;; For syntactic fontification, e.g. verbatim constructs.
       (font-latex-set-syntactic-keywords)
       ;; Tell font-lock about the update.
       (setq font-lock-set-defaults nil)
       (font-lock-set-defaults)))
   LaTeX-dialect)
  (TeX-run-style-hooks "code"))


;;; Timestamps:

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

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;; From http://www.emacswiki.org/emacs/UnfillParagraph
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

(defun tomorrow-time ()
 "Provide the time 24 hours from now in the same format as `current-time'.
Modified slightly from <http://www.emacswiki.org/emacs/Journal>."
 (let* ((now-time (current-time))          ; get the time now
        (hi (car now-time))                ; save off the high word
        (lo (car (cdr now-time)))          ; save off the low word
        (msecs (nth 2 now-time)))          ; save off the milliseconds
    (if (> lo 44671)                       ; If low word too big for adding to,
        (setq hi (+ hi 2)                  ; carry 2 to the high word,
              lo (- lo 44672))             ; subtract from the low
      (setq hi (+ hi 1)                    ; else, add 86400 seconds
            lo (+ lo 20864)))              ; (in two parts)
    (list hi lo msecs)))

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

;; "Aligning Text"
;; http://pragmaticemacs.com/emacs/aligning-text/

(defun jrb-align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun jrb-align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\&" 1 1 t))

;; Deleting rather than killing the previous word
;; http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))


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

  ;; keyboard symbols
  ("cctl" "⌃" nil 0)
  ("ccmd" "⌘" nil 0)
  ("oopt" "⌥" nil 0)
  ("sshift" "⇧" nil 0)

  ;; autocorrect
  ("adn" "and" nil 0)
  ("alot" "a lot" nil 0)
  ("comprized" "comprised" nil 0)
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


;;; Inter-App Communication:

;; AppleScript support
(defun applescript-quote-string (argument)
  "Quote a string for passing as a string to AppleScript."
  (if (or (not argument) (string-equal argument ""))
      "\"\""
    ;; Quote using double quotes, but escape any existing quotes or
    ;; backslashes in the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (save-match-data
        (if (or (null (string-match "[^\"\\]" argument))
                (< (match-end 0) (length argument)))
            (while (string-match "[\"\\]" argument start)
              (setq end (match-beginning 0)
                    result (concat result (substring argument start end)
                                   "\\" (substring argument end (1+ end)))
                    start (1+ end))))
        (concat "\"" result (substring argument start) "\"")))))

;; Send region to OmniFocus
(defun send-region-to-omnifocus (beg end)
  "Send the selected region to OmniFocus.
Use the first line of the region as the task name and the second
and subsequent lines as the task note."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (match (string-match "^\\(.*\\)$" region))
         (name (substring region (match-beginning 1) (match-end 1)))
         (note (if (< (match-end 0) (length region))
                   (concat (substring region (+ (match-end 0) 1) nil) "\n\n")
                 "")))
    (do-applescript
     (format "set theDate to current date
              set taskName to %s
              set taskNote to %s
              set taskNote to (taskNote) & \"Added from Emacs on \" & (theDate as string)
              tell front document of application \"OmniFocus\"
                make new inbox task with properties {name:(taskName), note:(taskNote)}
              end tell"
             (applescript-quote-string name)
             (applescript-quote-string note)))))

;; Send region to Fantastical
(defun send-region-to-fantastical (beg end)
  "Send the selected region to Fantastical.
Parse the first line to create the event and use the second
and subsequent lines as the event note."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (match (string-match "^\\(.*\\)$" region))
         (event (substring region (match-beginning 1) (match-end 1)))
         (notes (if (< (match-end 0) (length region))
                   (concat (substring region (+ (match-end 0) 1) nil) "\n\n")
                 "")))
    (do-applescript
     (format "set theDate to current date
              set eventText to %s
              set eventNotes to %s
              set eventNotes to (eventNotes) & \"Added from Emacs on \" & (theDate as string)
              tell application \"Fantastical\"
                parse sentence (eventText) notes (eventNotes)
              end tell"
             (applescript-quote-string event)
             (applescript-quote-string notes)))))



;;; Ligatures

;; Regular expressions should be grouped by length and expressed in
;; order of length.
;; https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(let ((alist
       '(
         ;; ! -> !==, !!!, !!, !=
         (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
         ;; # -> ####, ###, ##, #(, #?, #[, #_, #{
         (35 . ".\\(?:###\\|##\\|[#(?[_{]\\)")
         ;; $ -> $>
         (36 . ".\\(?:>\\)")
         ;; % -> %%%, %%
         (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
         ;; & -> &&&, &&
         (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
         ;; * -> *
         (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
         ;; + -> +++, ++, +>
         (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
         ;; - -> -->, ---, -<<, ->>, -<, ->, -}, -~, --
         (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
         ;; . -> ..., ..<, .., .=, .-
         (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
         ;; / -> /**, ///, /==, /*, //, /=, />
         (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
         ;; 0 -> 0xF
         (48 . ".\\(?:x[a-zA-Z]\\)")
         ;; : -> :::, ::, :=
         (58 . ".\\(?:::\\|[:=]\\)")
         ;; ; -> ;;;, ;;
         (59 . ".\\(?:;;\\|;\\)")
         ;; < -> <!--, <~~, <->, <$>, <*>, <+>, <--, <<<, <<=, <<-, <=<, <==, <=>, <|>,
         ;;      <*, <$, <+, <~, </, <<, <=, <>, <|, <-
         (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
         ;; = -> =/=, =:=, =<<, ===, ==>, =>>, =<, ==, =>, =~
         (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
         ;; > -> >=>, >>=, >>>, >>-, >=, >>, >-
         (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
         ;; ? -> ???, ?:, ?=, ??
         (63 . ".\\(?:\\(\\?\\?\\)\\|[=?]\\)")
         ;; [ -> []
         (91 . ".\\(?:]\\)")
         ;; \ -> \\\, \\
         (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
         ;; ^ -> ^=
         (94 . ".\\(?:=\\)")
         ;; w -> www
         (119 . ".\\(?:ww\\)")
         ;; {, -> {-
         (123 . ".\\(?:-\\)")
         ;; | -> ||=, |||, |=, |>, ||
         (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
         ;; ~ -> ~~>, ~~~, ~~>, ~~=, ~@, ~~, ~-
         (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
         )))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))


;;; Local configuration

(load-file "~/.emacs.d/init-local.el")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Post initialization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;;; init.el ends here
