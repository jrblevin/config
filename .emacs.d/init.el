;;; ~/.emacs.d/init.el: Emacs configuration
;;
;; Jason Blevins <jblevins@xbeta.org>
;; Raleigh, May 29, 2004

;; Directory Structure:
;;
;; ~/.emacs.d/                   user directory
;; ~/.emacs.d/init.el            init file
;; ~/.emacs.d/init-local.el      additional private init file
;; ~/.emacs.d/custom.el          customized variables and faces
;; ~/.emacs.d/site-lisp          manually installed packages
;; ~/.emacs.d/themes             custom themes

;;; Setup:

;; M-x all-the-icons-install-fonts
;; M-x nerd-icons-install-fonts

;;; Code:

;; Set the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")


;;; Platform-specific configuration

(defsubst jrb-mac-or-not (mac not)
  "Return MAC if system is a Mac and NOT otherwise."
  (if (eq system-type 'darwin) mac not))

(defsubst jrb-large-screen-or-not (large not)
  "Return LARGE if system has a large (wide) screen and NOT otherwise."
  (if (and (display-graphic-p) (> (x-display-pixel-width) 1280)) large not))

;; Face height is 10 * point size
(defconst jrb-default-face-height 15)


;;; GUI Elements

(when (display-graphic-p)
  ;; Set fonts first so widths and heights below are correct
  (defconst jrb-default-line-spacing 0.25) ; default is nil
  (setq-default line-spacing jrb-default-line-spacing)
  (setq inhibit-compacting-font-caches t)
  ;; (require 'fira-code-ligatures)
  ;; (set-face-attribute 'default nil :family "Fira Code" :weight 'light
  ;;                     :height (* jrb-default-face-height 10))
  (set-face-attribute 'default nil :family "Operator Mono" :weight 'light
                      :height (* jrb-default-face-height 12))
  (set-face-attribute 'fixed-pitch nil :family "Courier Prime")
  (set-face-attribute 'variable-pitch nil :family "Fira Sans"))

(defun jrb-default-frame-width ()
  "Default width for frames.
Subtract from screen width and divide by the width of a character
to get the number of columns.  Only use half screen width for
large displays."
  (let ((denom (jrb-large-screen-or-not 2 1))
        (fringes (window-fringes)))
    (/ (- (/ (display-pixel-width) denom)
          (+ (car fringes) (cadr fringes)))
       (frame-char-width))))

(defun jrb-default-frame-height ()
  "Default height for frames.
Subtract from screen height (for panels, menubars, etc.) and
divide by the height of a character to get the number of lines.
See <http://stackoverflow.com/questions/92971/>."
  (/ (- (display-pixel-height) 50)
     (frame-char-height)))

(defun jrb-setup-windows ()
  "Open two 80 column window on the right and two evenly-spaced
larger ones to the left.  Open `ansi-term' in the lower-right
window."
  (interactive)
  (delete-other-windows)
  (split-window-right -80)
  (split-window-horizontally)
  (other-window 2)
  (split-window-vertically)
  (other-window 1)
  (ansi-term "/bin/zsh")
  (other-window 1))

;; Set frame geometry according to display resolution.
(setq default-frame-alist
      `((top . 1)
        (left . 1)
        (width . ,(jrb-default-frame-width))
        (height . ,(jrb-default-frame-height))
        (vertical-scroll-bars . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)))

;; Disable scroll bar, tool bar, and menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (if (display-graphic-p)
        (menu-bar-mode (jrb-mac-or-not 1 0))
      (menu-bar-mode -1)))


;;; Fundamental Settings

;; Increase garbage collection threshold
(setq gc-cons-threshold (* 1 1024 1024))

;; Don't show so many messages on startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Personal information
(setq user-full-name "Jason Blevins")
(setq user-mail-address "jblevins@xbeta.org")

;; Load newer version of .el and .elc if both are available
(setq load-prefer-newer t)

;; Highlight current line, blink cursor
(if (fboundp 'global-hl-line-mode) (global-hl-line-mode 1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 1))

(setq inhibit-splash-screen     t       ; Disable splash screen
      initial-scratch-message   nil     ; No scratch buffer message
      transient-mark-mode       nil     ; Disable transient-mark-mode
      select-enable-clipboard   t       ; Sync kill ring and clipboard
      column-number-mode        0       ; Hide column number in mode line
      line-number-mode          0       ; Hide line number in mode line
      x-stretch-cursor          t       ; Make cursor full width of character
      mode-line-position        nil)    ; Hide position (C-x =, M-x what-line)

;; Suppress beeps
(setq visible-bell nil ring-bell-function 'jrb-flash-mode-line)

;; Browser
(setq browse-url-generic-program (jrb-mac-or-not nil "google-chrome"))

;; Tabs versus Spaces: http://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil)
(setq tab-width 8)

;; Store backup files in one place.  Do the same for auto save files.
(defvar jrb-backup-directory (expand-file-name "~/.backups/"))
(unless (file-exists-p jrb-backup-directory)
  (make-directory jrb-backup-directory t))
(setq make-backup-files t
      backup-directory-alist `((".*" . ,jrb-backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      vc-make-backup-files nil
      kept-old-versions 6
      kept-new-versions 9)
(setq auto-save-default t
      auto-save-file-name-transforms `((".*" ,jrb-backup-directory t))
      auto-save-list-file-prefix jrb-backup-directory)

(defun jrb-clean-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'jrb-clean-kill-ring)

;; Ignored files
(setq jrb-ignored-extensions
      '(".aux" ".nav" ".bbl" ".blg" ".dvi" ".brf" ".snm" ".toc"
        ".fls" ".rel" "_region_." ".fdb_latexmk" ".synctex.gz"
        ".ind" ".ilg" ".lol" ".minted"))

;; Show matching parentheses.
(show-paren-mode 1)

;; Show the date and time in 24-hour format.
(display-time-mode 0)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)

;; Allow typing y or n instead of typing yes and no in full
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make it hard to accidentally kill Emacs
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "<ns-power-off>"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'kill-buffer-and-window)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Set undo limits
(setq undo-limit (* 16 1024 1024)
      undo-strong-limit (* 24 1024 1024)
      undo-outer-limit (* 64 1024 1024))

;; Modifier keys: command sends meta, option sends super
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(global-set-key (kbd "M-`") 'ns-next-frame)

;; Don't disable commands
(setq disabled-command-function nil)

;; Save window configuration
(winner-mode 1)


;; Remote connections with tramp
(customize-set-variable 'tramp-encoding-shell "/bin/zsh")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")


;;; Package management

;; Load `package.el'
(eval-when-compile (require 'package))
(setq package-enable-at-startup nil
      package-user-dir (format "%selpa-%d.%d" user-emacs-directory
                               emacs-major-version emacs-minor-version)
      package-menu-async t
      package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("GNU ELPA"     . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("MELPA"        . 10)
        ("MELPA Stable" . 5)
        ("GNU ELPA"     . 0)))
(eval-when-compile (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Support package install from VC
(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))


;;; Color themes:

(setq custom-theme-directory "~/.emacs.d/themes")

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one 'no-confirm 'no-enable)
  ;(load-theme 'doom-tomorrow-night 'no-confirm 'no-enable)
  ;(load-theme 'doom-monokai-ristretto 'no-confirm 'no-enable)
  ;(load-theme 'doom-snazzy 'no-confirm 'no-enable)
  ;(load-theme 'doom-xcode 'no-confirm 'no-enable)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config))

(use-package less-theme
  :init (load-theme 'less 'no-confirm 'no-enable))

(use-package twilight-theme
  :defer t
  :init (load-theme 'twilight 'no-confirm 'no-enable))

(use-package twilight-anti-bright-theme
  :defer t
  :init (load-theme 'twilight-anti-bright 'no-confirm 'no-enable))

(use-package color-theme-sanityinc-tomorrow
  :defer t
  :init
  (load-theme 'sanityinc-tomorrow-day 'no-confirm 'no-enable)
  (load-theme 'sanityinc-tomorrow-night 'no-confirm 'no-enable)
  (load-theme 'sanityinc-tomorrow-bright 'no-confirm 'no-enable)
  (load-theme 'sanityinc-tomorrow-eighties 'no-confirm 'no-enable))

;; (use-package darktooth-theme
;;   :init (load-theme 'darktooth 'no-confirm 'no-enable))

;; (use-package gruvbox-theme
;;   :defer t
;;   :config
;;   (load-theme 'gruvbox-dark-hard 'no-confirm 'no-enable)
;;   (load-theme 'gruvbox-dark-medium 'no-confirm 'no-enable))

;; (use-package modus-themes
;;   :defer t
;;   :ensure t
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil)

;;   ;; Maybe define some palette overrides, such as by using our presets
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-intense)

;;   ;; Load the theme of your choice.
;;   ;(load-theme 'modus-vivendi)
;;   (load-theme 'modus-vivendi-tinted))

(if (display-graphic-p)
    ;; (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    ;;   (cond
    ;;    ((memq hour (number-sequence 7 11))
    ;;     (enable-theme 'sanityinc-tomorrow-day))
    ;;    ((memq hour (number-sequence 12 17))
    ;;     (enable-theme 'twilight))
    ;;    ((memq hour (number-sequence 18 22))
    ;;     (enable-theme 'sanityinc-tomorrow-bright))
    ;;    (t
    ;;     (enable-theme 'sanityinc-tomorrow-night))))
    ;(enable-theme 'doom-tomorrow-night)
    (enable-theme 'doom-one)
    ;(enable-theme 'sanityinc-tomorrow-bright)
    ;(enable-theme 'sanityinc-tomorrow-eighties)
    ;(enable-theme 'twilight-anti-bright)
    ;(enable-theme 'gruvbox-dark-hard)
  (load-theme 'less t))


;;; Global keybindings:

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)

(global-set-key (kbd "<f3>") 'my-insert-date-time)
(global-set-key (kbd "<f4>") 'revert-buffer-no-confirm)
(global-set-key (kbd "<f5>") 'jrb-log-post)
(global-set-key (kbd "<f6>") 'calendar)
(global-set-key (kbd "<f10>") 'jrb-write-mode)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f12>") 'jrb-dual-mode)

(global-set-key [?\M-j] 'fill-sentences)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key [\C-\M-down] 'move-line-down)
(global-set-key [\C-\M-up] 'move-line-up)

(global-set-key (kbd "C-h C-r") 'describe-char)

(global-set-key (kbd "C-c l") 'jrb-log-post)
(global-set-key (kbd "C-c o") 'send-region-to-omnifocus)
(global-set-key (kbd "C-c f") 'send-region-to-fantastical)
(global-set-key (kbd "C-c e") 'jrb-evaluate-template)

(global-set-key (kbd "S-C-o") 'jrb-separate-line)

;; Windows and buffers
(global-set-key (kbd "C-x O") 'swap-windows)
(global-set-key (kbd "C-x M-b") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer) ; Replace buffer-menu with ibuffer

;; Shell
(global-set-key (kbd "s-s") 'shell)
(global-set-key (kbd "s-S") 'ansi-term)

;; Imenu
(global-set-key (kbd "M-i") 'imenu)

;; Region and mark
(global-set-key (kbd "M-`") 'transient-mark-mode)
(global-set-key (kbd "C-c DEL") 'delete-region)


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


;;; General Packages

;; Notes on use-package declarations:
;;
;;     :init          Code to run BEFORE package has been loaded.
;;     :config        Code to run AFTER package has been loaded.

(defsubst hook-into-modes (func &rest modes)
  "Add FUNC to hook functions given by MODES."
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(use-package ace-window
  :ensure t
  :defer t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package all-the-icons
  ;; Run M-x all-the-icons-install-fonts
  :ensure t
  :config
  ;; (add-to-list 'all-the-icons-icon-alist
  ;;              '("\\.f90$" all-the-icons-faicon "bar-chart"
  ;;                :v-adjust -0.9 :face all-the-icons-lpurple))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.f90$" all-the-icons-faicon "calculator"
                 :v-adjust -0.9 :face all-the-icons-lpurple)))

(use-package auto-compile :disabled t
  :config (auto-compile-on-load-mode))

(use-package autorevert
  :diminish global-auto-revert-mode
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode 1))

(use-package compile
  :bind ("<f9>" . compile)
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

(use-package deft
  :bind
  (("<f8>" . deft)
   ("C-c d" . deft)
   ("C-c Y" . deft-yesterday)
   ("C-c D" . deft-today)
   ("C-c M" . deft-tomorrow)
   ("C-c F" . jrb-git-autocommit-and-push)
   ("C-x C-g" . deft-find-file))

  :commands
  (deft deft-open-file deft-new-file-named)

  :init
  (defun deft-daily (iso)
    (interactive)
    (let* ((slug (concat "Daily/" iso))
           (filename (file-truename (concat deft-directory "/" slug ".md")))
           (deft-filter-regexp nil))
      (if (file-exists-p filename)
          ;; If it exists, open it with Deft
          (deft-open-file filename t t)
        ;; Insert the template and replace dates
        (find-file-noselect filename)
        (with-current-buffer (get-file-buffer filename)
          (insert-file-contents "~/git/gtd/Templates/daily.md")
          (goto-char (point-min))
          (when (search-forward "{{date:MMMM D, YYYY}}" nil t)
            (replace-match (format-time-string "%B %e, %Y")))
          (when (or (search-forward "{{yesterday-iso}}" nil t)
                    (search-forward "<% tp.date.now(\"YYYY-MM-DD\", -1) %>" nil t))
            (replace-match (format-time-string "%Y-%m-%d" (yesterday-time))))
          (when (or (search-forward "{{tomorrow-iso}}" nil t)
                    (search-forward "<% tp.date.now(\"YYYY-MM-DD\", 1) %>" nil t))
            (replace-match (format-time-string "%Y-%m-%d" (tomorrow-time))))
          (goto-char (point-min)))
        ;; Open in deft
        (deft-cache-update-file filename)
        (deft-refresh-filter)
        (deft-open-file filename))))

  (defun deft-today ()
    "Create or open a Deft note for today."
    (interactive)
    (deft-daily (format-time-string "%Y-%m-%d")))

  (defun deft-yesterday ()
    "Create or open a Deft note for yesterday."
    (interactive)
    (deft-daily (format-time-string "%Y-%m-%d" (yesterday-time))))

  (defun deft-tomorrow ()
    "Create or open a Deft note for tomorrow."
    (interactive)
    (deft-daily (format-time-string "%Y-%m-%d" (tomorrow-time))))

  (defun deft-previous ()
    "Find previous daily file"
    (interactive)
    (let* ((default-directory (concat deft-directory "/Daily/"))
           (pattern "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].md")
           (files (file-expand-wildcards pattern))
           ;; Today
           (today (format-time-string "%Y-%m-%d.md"))
           ;; Remove the current day
           (files (delete today files))
           ;; Most recent daily file
           (prev (car (reverse files))))
      (deft-open-file (concat default-directory prev) t nil)))

  (defun deft-plan ()
    "Open today's daily note and the most recent previous note."
    (interactive)
    (deft)
    (deft-today)
    (delete-other-windows)
    (split-window-right)
    (deft-previous))

  (defun jrb-git-autocommit-and-push ()
    "Automatically commit all changes and push to the remote repository."
    (interactive)
    ;; Stage all changes
    (magit-stage-modified t)
    ;; Commit with a standard message
    (let ((message (format-time-string "gtd commit (Emacs): %Y-%m-%d %02H:%02M:%02S %Z")))
      (magit-commit-create (list "--all" "-m" message)))
    ;; Push to remote
    (magit-push-current-to-pushremote nil))

  :init
  (setq deft-directory "~/git/gtd/")
  (setq deft-time-format " %y-%m-%d")

  :config
  (defun jrb-deft-mode-hook ()
    (olivetti-mode)
    (deft-refresh-browser))
  (add-hook 'deft-mode-hook 'jrb-deft-mode-hook)

  (defun deft-reload ()
    "Reload Deft from source (for development)."
    (interactive)
    (quit-window)
    (load-library "/Users/jblevins/projects/deft/deft.el")
    (deft))

  (defun deft-sidebar ()
    "Open Deft in a small window on the side."
    (interactive)
    (delete-other-windows)
    (split-window-horizontally 30)
    (deft)
    (switch-to-buffer-other-window
     (or (buffer-name (car deft-auto-save-buffers))
         "*scratch*"))
    (other-window 1))

  (setq deft-auto-save-interval 2
        deft-recursive t
        deft-extensions '("md" "txt" "text" "tex" "taskpaper" "org")
        deft-default-extension "md"
        deft-archive-directory "Archive"
        deft-use-filter-string-for-filename nil
        deft-markdown-mode-title-level 1
        deft-use-filename-as-title t
        deft-generation-rules nil
        deft-file-limit 200
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))
        deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ; blank
                "\\|^<!--[ ]*#[[:alnum:]][ ]*-->" ; hashtags
                "\\|^#\\+[[:upper:]_]+:.*$" ; org-mode metadata
                "\\)")))

(use-package devdocs
  :bind (("C-h D" . devdocs-lookup)))

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-state-icon nil))

(use-package dumb-jump
  :ensure t
  :init
  ;; Use ag due to bug with git-grep on macOS: https://github.com/jacktasia/dumb-jump/issues/428
  ;; brew install the_silver_searcher
  (setq dumb-jump-force-searcher 'ag)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-selector 'completing-read))

(use-package ebib
  :commands ebib
  :config
  (setq ebib-file-search-dirs '("~/references/articles" "~/references/books")
        ebib-search-dirs '("~/git/gtd")
        ebib-preload-bib-files '("research.bib")
        ebib-index-display-fields '("title")
        ebib-file-associations '(("pdf" . "pdfexpert"))
        ebib-keywords-file (expand-file-name "ebib-keywords.txt"
                                             user-emacs-directory)
        ebib-save-keywords-on-exit 'always
        ebib-keywords-use-only-file t))

(use-package exec-path-from-shell
  :defer t
  :if (jrb-mac-or-not t nil)
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :defer 1
  :config (setq er--show-expansion-message t)
  :bind ("C-c =" . er/expand-region))

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :config
  (setq flycheck-gfortran-warnings (list "all" "extra" "surprising")
        flycheck-gfortran-language-standard "f2008"
        flycheck-gfortran-include-path (list "." ".." "/Users/jblevins/work/osl/fortran"))
  (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space)
  :init
  (hook-into-modes #'flycheck-mode 'f90-mode-hook 'emacs-lisp-mode-hook))

(use-package flyspell
  :defer t
  :after ispell
  :diminish flyspell-mode)

(use-package flyspell-lazy
  :after flyspell
  :commands (flyspell-lazy-mode)
  :defines (flyspell-lazy-idle-seconds
            flyspell-lazy-window-idle-seconds)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package font-lock-profiler
  :defer t
  :commands (font-lock-profiler-buffer
             font-lock-profiler-region
             font-lock-profiler-start)
  :load-path "site-lisp/font-lock-profiler/")

(use-package font-lock-studio
  :defer t
  :commands (font-lock-studio)
  :load-path "site-lisp/font-lock-studio/")

(use-package gscholar-bibtex
  :commands gscholar-bibtex
  :init
  (setq gscholar-bibtex-database-file "~/git/gtd/research.bib")
  :config
  (dolist (source '("ACM Digital Library" "IEEE Xplore" "DBLP"))
    (gscholar-bibtex-source-on-off :off source))
  (setq gscholar-bibtex-default-source "Google Scholar"))

(use-package google-this
  :commands google-this
  :bind ("C-c s" . google-this))

(use-package git-messenger
  :commands git-messenger:popup-message
  :bind ("C-x !" . git-messenger:popup-message)
  :init (setq git-messenger:show-detail t))

(use-package guide-key
  :disabled t
  :defer 5
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode 1))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h ." . helpful-at-point)))

(use-package highlight-refontification
  :commands  (highlight-refontification-mode)
  :load-path "site-lisp/highlight-refontification")

(use-package html-mode
  :mode (("\\.leaf\\'" . html-mode)))

(use-package imenu-list
  :ensure t
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

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
  (setq ispell-program-name "aspell"
        ispell-really-aspell t
        ispell-extra-args '("--sug-mode=ultra")))

(use-package magit
  :ensure t
  :bind (("C-x g s" . magit-status)
         ("C-x g l" . magit-log-all)
         ("C-x g F" . magit-pull-from-upstream)
         ("C-x g U" . magit-push-current-to-upstream))
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode))))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-s-c C-s-c" . mc/edit-lines))

(use-package neotree
  :ensure t
  :after all-the-icons
  :bind (("C-c n" . neotree-toggle))
  :init
  (setq neo-theme 'nerd
        neo-autorefresh t
        neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 30
        neo-cwd-line-style 'text
        neo-vc-integration '(face char))
  :config
  (setq neo-hidden-regexp-list
        (append neo-hidden-regexp-list
                (mapcar (lambda (ext) (concat (regexp-quote ext) "$"))
                        jrb-ignored-extensions))))

(use-package olivetti
  :ensure t
  :diminish (olivetti-mode visual-line-mode)
  :init
  (setq olivetti-body-width (jrb-large-screen-or-not 90 80)
        olivetti-hide-mode-line nil)
  :bind (("C-c w" . olivetti-mode)))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init (hook-into-modes #'page-break-lines-mode
                         'emacs-lisp-mode-hook
                         'python-mode-hook))

(use-package persistent-scratch
  :diminish
  :hook ((after-init . persistent-scratch-autosave-mode))
  :init (setq persistent-scratch-save-file
              (concat user-emacs-directory ".scratch-" (system-name)))
  :config (persistent-scratch-setup-default))

(use-package powerline
  :disabled t
  :ensure t
  :if (display-graphic-p)
  :config
  (setq powerline-display-hud nil
        powerline-display-buffer-size nil
        powerline-display-mule-info t
        powerline-gui-use-vcs-glyph nil
        powerline-height 24
        powerline-default-separator 'slant)
  :init (powerline-default-theme))

(use-package profiler
  :defer t
  :bind (("C-c p s" . profiler-start)
         ("C-c p r" . profiler-report)
         ("C-c p q" . profiler-stop))
  :config
  (setq profiler-report-cpu-line-format
        '((100 left)
          (24 right ((19 right)
                     (5 right))))))

(use-package project
  :defer t
  :config
  (setq project-list-file "~/.emacs.d.private/projects")

  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41955#26
  (defvar jrb-project-root-markers
    '(".dir-locals.el" ".obsidian" "README.md")
    "Files or directories that indicate the root of a project.")

  (defun jrb-project-find-local (dir)
    "Determine if DIR is a project root directory.
DIR must one of `project-root-markers' to be considered a project."
    (let ((root
           (locate-dominating-file
            dir
            (lambda (d)
              (let ((default-directory d))
                (seq-some #'file-expand-wildcards
                          jrb-project-root-markers))))))
      (when root
        (cons 'transient root))))

  ;; This is a fallback to `project-try-vc', and it is slow, so use
  ;; depth = 10 to give this hook a lower priority.
  (add-hook 'project-find-functions #'jrb-project-find-local 10))

;; (use-package counsel-projectile
;;   :defer t)

(use-package rainbow-mode
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

;; Save history
(use-package savehist
  :init
  (setq jrb-history-directory (expand-file-name "history/" user-emacs-directory))
  (unless (file-exists-p jrb-history-directory)
    (make-directory jrb-history-directory t))
  (setq savehist-file (expand-file-name "history" jrb-history-directory))
  (savehist-mode 1)
  :config
  (setq history-length 100
        history-delete-duplicates t
        savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(use-package server
  :config
  (setq server-kill-new-buffers t)
  (unless (server-running-p)
    (server-start)))

(use-package smartparens
  :defer t
  :ensure t
  :config
  ;;(setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package sublimity
  :load-path "site-lisp/sublimity"
  :config
  (require 'sublimity-attractive)
  (require 'sublimity-map)
  (setq sublimity-attractive-centering-width 120)
  (sublimity-attractive-hide-vertical-border)
  (sublimity-attractive-hide-fringes)
  (defun jrb-setup-sublimity-map ()
    "Set face and line spacing for sublimity-mode minimap."
    (sublimity-map-set-delay 0)
    (setq-local line-spacing nil)
    (setq buffer-face-mode-face '(:family "Nitti WM2"))
    (buffer-face-mode))
  (add-hook 'sublimity-map-setup-hook #'jrb-setup-sublimity-map))

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

(use-package which-key
  :defer 5
  :diminish which-key-mode
  :config
  (which-key-mode))


;;; Completion: Vertico, Consult, Marginalia, Orderless, Embark

(use-package all-the-icons-completion
  :disabled t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("C-s" . consult-line)                    ;; orig. isearch
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package consult-dir
  :ensure t
  :bind (("M-g d"   . consult-dir)
         :map minibuffer-local-completion-map
         ("M-s f" . consult-dir-jump-file)
         ("M-g d" . consult-dir)))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  ;; Exclude temporary files from completion
  (setq completion-ignored-extensions
        (append completion-ignored-extensions jrb-ignored-extensions))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  :demand t
  :bind (("M-/" . completion-at-point)
         :map corfu-map
         ("C-n"      . corfu-next)
         ("C-p"      . corfu-previous)
         ("<escape>" . corfu-quit)
         ("<return>" . corfu-insert)
         ("M-d"      . corfu-info-documentation)
         ("M-l"      . corfu-info-location)
         ("M-."      . corfu-move-to-minibuffer))
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  ;; Only use `corfu' when calling `completion-at-point' or
  ;; `indent-for-tab-command'
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Other
  (corfu-echo-documentation nil)        ; Already use corfu-popupinfo
  :preface
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :config
  (global-corfu-mode)

  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main
  ;; minibuffer completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ([remap corfu-show-documentation] . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-delay 0.5)
  ;(corfu-popupinfo-max-width nil)
  ;(corfu-popupinfo-max-height nil)
  ;; Also here to be extra-safe that this is set when `corfu-popupinfo' is
  ;; loaded. I do not want documentation shown in both the echo area and in
  ;; the `corfu-popupinfo' popup.
  (corfu-echo-documentation nil))

(use-package cape
  :ensure t
  :demand t
  :bind (("C-c . p" . completion-at-point)
         ("C-c . t" . complete-tag)
         ("C-c . d" . cape-dabbrev)
         ("C-c . h" . cape-history)
         ("C-c . f" . cape-file)
         ("C-c . k" . cape-keyword)
         ("C-c . s" . cape-symbol)
         ("C-c . a" . cape-abbrev)
         ("C-c . l" . cape-line)
         ("C-c . w" . cape-dict)
         ("C-c . \\" . cape-tex)
         ("C-c . _" . cape-tex)
         ("C-c . ^" . cape-tex)
         ("C-c . &" . cape-sgml)
         ("C-c . r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :init
  (vertico-mode))

(use-package embark
  :disabled t
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :disabled t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;(use-package codeium
;;  :init
;;  (slot/vc-install :fetcher "github" :repo "Exafunction/codeium.el")
;;  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;  :config
;;  (setq use-dialog-box nil) ;; do not use popup boxes
;;  (setq codeium-mode-line-enable
;;        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;  ;; You can overwrite all the codeium configs!
;;  ;; for example, we recommend limiting the string sent to codeium for better performance
;;  (defun my-codeium/document/text ()
;;    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;  ;; if you change the text, you should also change the cursor_offset
;;  ;; warning: this is measured by UTF-8 encoded bytes
;;  (defun my-codeium/document/cursor_offset ()
;;    (codeium-utf8-byte-length
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;  (setq codeium/document/text 'my-codeium/document/text)
;;  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))


;;; Completion: Ivy/Swiper/Counsel

(use-package abbrev
  :disabled t
  :defer t
  :diminish abbrev-mode
  :config
  ;; Turn on abbrev mode globally
  (setq-default abbrev-mode t))

(use-package company
  :disabled t
  :defer 1
  :ensure t
  :diminish company-mode
  :config
  (setq company-backends
        '((company-dabbrev-code company-abbrev company-capf)
          (company-files company-keywords)))
  (global-company-mode))

(use-package hippie-expand
  :disabled t
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

(use-package ido
  :disabled t
  :init
  (ido-mode t)
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  :config
  (use-package ido-ubiquitous
    :ensure t
    :defer 1
    :config (ido-ubiquitous-mode 1))
  (setq ido-enable-flex-matching t
        ido-file-extensions-order '(".tex" ".bib" ".sty" ".f90" ".txt" ".text" ".el")
        read-file-name-function 'ido-read-file-name
        ido-ignore-extensions t
        ido-everywhere t))

(use-package ivy
  :disabled t
  :defer t
  :diminish ivy-mode
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char))
  :config
  ;; Exclude temporary files from completion
  (setq completion-ignored-extensions
        (append completion-ignored-extensions jrb-ignored-extensions))
  (setq counsel-find-file-ignore-regexp
        (regexp-opt completion-ignored-extensions))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-initial-inputs-alist nil
        ;ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        magit-completing-read-function 'completing-read)
  (ivy-mode 1))

(use-package smex
  :disabled t
  :ensure t
  :after ivy
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :init (setq smex-save-file (concat jrb-history-directory "smex"))
  :config (smex-initialize))


;;; Languages

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
  :mode (("\\.ampl\\'" . ampl-mode)
         ("\\.mod\\'" . ampl-mode)
         ("\\.dat\\'" . ampl-mode))
  :interpreter ("ampl" . ampl-mode))

(use-package bibtex
  :defer t
  :init
  (defun bibtex-open-file ()
    "Search for and open PDF file corresponding to BibTeX entry at point."
    (interactive)
    (save-excursion
      (bibtex-beginning-of-entry)
      (let ((key (cdr (assoc-string "=key=" (bibtex-parse-entry))))
            (dirs '("/Users/jblevins/references/articles"
                    "/Users/jblevins/references/books"))
            (filename nil))
        (while dirs
          (setq filename (concat (car dirs) "/" key ".pdf"))
          (if (not (file-exists-p filename))
              (setq dirs (cdr dirs))
            (shell-command (format "open \"%s\"" filename))
            (setq dirs nil))))))
  :config
  (bind-key "C-c C-v" 'bibtex-open-file bibtex-mode-map)
  (bind-key "C-M-a" 'beginning-of-defun bibtex-mode-map)
  (bind-key "C-M-e" 'end-of-defun bibtex-mode-map)
  (setq bibtex-user-optional-fields '(("keywords" "Entry keywords"))
        bibtex-autokey-names 5
        bibtex-autokey-name-separator "-"
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-year-length 4
        bibtex-autokey-titleword-ignore '("The" "A" "If" "An")
        bibtex-autokey-titleword-length 30
        bibtex-autokey-titlewords 1
        bibtex-align-at-equal-sign t))

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

(use-package f90
  :ensure t
  :defer 1
  :config
  (setq f90-indent-level 4
        f90-do-indent 4
        f90-if-indent 4
        f90-type-indent 4
        f90-program-indent 4
        f90-associate-indent 4
        f90-critical-indent 4
        f90-continuation-indent 4
        f90-tab-indent 4))

(use-package f90
  :mode (("\\.[Ff]\\(?:90\\|95\\|03\\|08\\|15\\)\\'" . f90-mode)
         ("\\.inc\\'" . f90-mode))
  :config
  (defun jrb-f90-mode-hook ()
    (setq f90-beginning-ampersand nil
          f90-font-lock-keywords f90-font-lock-keywords-3
          comment-column 50)
    ;; Font lock for selector operator and OpenMP directives
    (font-lock-add-keywords
     'f90-mode
     '(("%" . font-lock-keyword-face)
       ("^[\t ]*!\\($OMP[\t ].*\\)$" 1 font-lock-builtin-face prepend)
       ("^[#@$]:\\(?:\\sw\\|\\s_\\)+" (0 font-lock-preprocessor-face t))))
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

(use-package gnuplot
  :ensure t
  :mode (("\\.gnuplot\\'" . gnuplot-mode)))

(use-package graphviz-dot-mode
  :ensure t :defer t
  :config (setq graphviz-dot-indent-width 4))

(use-package latex
  :ensure auctex
  :mode (("\\.tex\\'" . latex-mode))
  :commands (latex-mode LaTeX-mode)
  :config
  (setq font-latex-match-slide-title-keywords '("foilhead" "fhead"))

  (defun jrb-LaTeX-hook-fn ()
    (setq TeX-command-default "latexmk")
    (setq LaTeX-font-list (append
                           LaTeX-font-list
                           '((?c "\\ccode{" "}")
                             (?f "\\fcode{" "}")
                             (?s "\\scode{" "}")
                             (?C "\\ccode|" "|")
                             (?F "\\fcode|" "|")
                             (?S "\\scode|" "|"))))
    (setq LaTeX-math-list
          (quote (("C-(" (lambda ()(interactive)(LaTeX-my-leftright "(" ")")) "" nil)
                  ("C-{" (lambda ()(interactive)(LaTeX-my-leftright "\\{" "\\}")) "" nil))))

    ;; (setq LaTeX-math-list
    ;;       '((?8 "infty" "Misc Symbol" 8734)
    ;;         (?I "int" "Var Symbol" 8747)
    ;;         ("C-T" "text" "" nil)
    ;;         ("C-(" (lambda ()(interactive)(LaTeX-my-leftright "(" ")")) "" nil)
    ;;         ("C-{" (lambda ()(interactive)(LaTeX-my-leftright "\\{" "\\}")) "" nil)))
    ;; (setq reftex-plug-into-AUCTeX t)
    ;; (turn-on-reftex)
    (LaTeX-math-mode 1)
    (setq TeX-PDF-mode t)
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    (jrb-LaTeX-setup-code))
  (add-hook 'LaTeX-mode-hook 'jrb-LaTeX-hook-fn))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

(use-package markdown-mode
  :bind (("<f7>" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("/projects/markdown-mode.*\\.txt\\'" . markdown-mode)
         ("/git/gtd/.*\\.txt\\'" . markdown-mode))
  :init
  (setq markdown-header-scaling t
        markdown-hide-urls t
        markdown-marginalize-headers t
        markdown-marginalize-headers-margin-width 4
        markdown-wiki-link-search-type '(project sub-directories)
        markdown-fontify-code-blocks-natively t)
  :config
  (use-package org-table
    :commands orgtbl-mode)

  (setq markdown-command "multimarkdown --snippet"
        markdown-open-command "mark"
        markdown-enable-wiki-links t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-link-space-sub-char " "
        markdown-unordered-list-item-prefix "*   "
        markdown-footnote-location 'end
        markdown-reference-location 'header
        markdown-asymmetric-header t
        markdown-nested-imenu-heading-index t
        markdown-live-preview-delete-export 'delete-on-destroy
        markdown-max-image-size '(640 . 480)
        markdown-hr-strings
        '("------------------------------------------------------------------------------"
          "*** *** ***"
          "--- --- ---")
        markdown-css-paths '("/Applications/Marked 2.app/Contents/Resources/Lopash.css"))

  (defun jrb-markdown-mode-hook ()
    ;; Olivetti
    ;(turn-on-olivetti-mode)
    ;; For MultiMarkdown tables
    (make-local-variable 'org-table-automatic-realign)
    (setq org-table-automatic-realign nil)
    ;; Automatically keep imenu index up to date
    (setq imenu-auto-rescan t)
    ;; Enable math mode based on file metadata
    (save-excursion
      (when (re-search-forward "^math:\\s-*itex$" nil t)
        (markdown-enable-math 1))))
  (add-hook 'markdown-mode-hook 'jrb-markdown-mode-hook)
  (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)

  (defun jrb-gfm-mode-hook ()
    (visual-line-mode 1))
  (add-hook 'gfm-mode-hook 'jrb-gfm-mode-hook)

  (defun markdown-reload ()
    "Reload markdown mode from source (for development)."
    (interactive)
    (when (eq major-mode 'markdown-mode)
      (fundamental-mode))
    (let ((features '(markdown-test markdown-mode markdown)))
      (dolist (feature features)
        (when (featurep feature) (unload-feature feature 'force))))
    (let ((files '("~/work/markdown-mode/markdown.el"
                   "~/work/markdown-mode/markdown-mode.el")))
      (dolist (file files)
        (when (file-exists-p file)
          (load-library file))))
    (markdown-mode)))

(defun jrb-convert-reference-to-inline-link ()
  "Convert reference link at point to an inline link."
  (interactive)
  (when (thing-at-point-looking-at markdown-regex-link-reference)
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (text (match-string 3))
          (url (markdown-link-url)))
      (delete-region beg end)
      (markdown-insert-inline-link text url))))

(use-package mma
  :commands mma-mode)

(use-package matlab
  :commands (matlab-mode matlab-shell)
  :mode (("\\.m\\'" . matlab-mode))
  :config
  (setq-local comment-insert-comment-function 'matlab-comment)
  (add-hook 'matlab-mode-hook (lambda () (local-set-key "\M-;" nil)))
  (setq matlab-auto-fill nil))

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
  :mode (("\\.org\\'" . org-mode))
  :init
  (setq org-hide-emphasis-markers t))

(use-package post
  :mode (("mutt-" . post-mode))
  ;:bind (("C-c i" . mutt-alias-insert))
  :config
  (setq post-signature-pattern "\\(--\\|Cheers,\\|Thanks,\\|Best,\\|Best regards,\\|\\|Sent from my\\)"
        post-uses-fill-mode nil
        post-underline-pattern nil
        post-emoticon-pattern nil
        post-bold-pattern nil
        post-kill-quoted-sig nil
        post-signature-source-is-file t
        post-fixed-signature-source "~/.signature"
        post-email-address user-mail-address)

  (use-package mutt-alias
    :commands (mutt-alias-insert)
    :init (setq mutt-alias-file-list '("~/.mutt-aliases")))

  (defun jrb-post-mode-hook()
    ;; Remove trailing whitespace (but protect '-- ' in signature)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "\\([>:]\\)\s+$" nil t)
        (replace-match (match-string 1) nil nil)))
    ;; Rewrite "Last, First" addresses
    (save-excursion
      (re-search-forward "\n\n")
      (let ((end (point)))
        ;;(message body)
        (beginning-of-buffer)
        (while (re-search-forward "\"\\([[:alpha:]]+\\), \\([[:alpha:]]+\\)\"" end t)
          (replace-match (concat (match-string 2) " " (match-string 1)) nil nil))))
    (post-goto-body))
  (add-hook 'post-mode-hook 'jrb-post-mode-hook))

(use-package python
  :defer t
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)))

(use-package swift
  :mode (("\\.swift\\'" . swift-mode)))

(use-package taskpaper-mode
  :commands (taskpaper-mode)
  :mode (("\\.taskpaper\\'" . taskpaper-mode))
  :config
  (defun jrb-taskpaper-mode-hook ()
    (setq-local indent-tabs-mode t)
    (setq-local tab-width 4)
    (setq-local indent-line-function 'insert-tab))
  (add-hook 'taskpaper-mode-hook #'jrb-taskpaper-mode-hook))

(use-package tex
  :ensure auctex
  :commands (tex-mode)
  :config
  (setq TeX-parse-self t
        TeX-auto-save nil)

  ;; Update available TeX commands
  (setq TeX-command-list
        (append
         '(("latexmk" "latexmk -g -synctex=1 -pdf %s"
            TeX-run-TeX nil (latex-mode)
            :help "Run latexmk on file")
           ("XeLaTeX" "%`xelatex%(mode)%' %t"
            TeX-run-TeX nil t
            :help "Run xelatex on file")
           ("bibtool" "bibtool -x %s.aux > %s.bib"
            TeX-run-command nil t
            :help "Run bibtool on aux file to produce bib file"))
         TeX-command-list))

  ;; Viewers
  (setq TeX-view-program-list
        '(("Preview" "/usr/bin/open -a Preview.app %o")
          ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b")))
  (setq TeX-view-program-selection
        '((output-dvi "Skim") (output-pdf "Skim") (output-html "open")))

  (use-package company-auctex
    :config (company-auctex-init)))

(use-package tex-site
  :defer t
  :ensure auctex)


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
(define-minor-mode jrb-write-mode
  "Minor mode for distraction-free writing."
  :init-value nil
  :global t
  :variable jrb-write-mode
  :group 'editing-basics
  (cond ((not jrb-write-mode)
         ;; Restore line spacing and text scaling
         (setq-default line-spacing jrb-default-line-spacing)
         (when (> text-scale-mode-amount 0)
           (text-scale-set 0))
         ;; Turn full screen off
         (when (frame-parameter nil 'fullscreen)
           (toggle-frame-fullscreen)))
        (t
         ;; Turn full screen on
         (unless (frame-parameter nil 'fullscreen)
           (toggle-frame-fullscreen))
         (sleep-for 1)
         ;; Increase text scaling and line spacing
         (text-scale-set 0.5)
         (setq-default line-spacing 0.5))))

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


;;; Website:

(defun jrb-log-post (slug)
  (interactive "sSlug: ")
  (find-file (concat "~/work/jblevins.org/htdocs/log/" slug ".text"))
  (skeleton-webpage-header))


;;; Custom Environments in AUCTeX:

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
      '("ccode" TeX-arg-verb)
      '("fcode" TeX-arg-verb)
      '("scode" TeX-arg-verb))

     ;; New environments
     (LaTeX-add-environments
      '("pre" jrb-LaTeX-env-code)
      '("interface" jrb-LaTeX-env-code))

     ;; Abbrevs
     (define-abbrev latex-mode-abbrev-table "xfr" "" 'skeleton-beamer-frame)
     (define-abbrev latex-mode-abbrev-table "xfl" "" 'skeleton-beamer-listing-frame)
     (define-abbrev latex-mode-abbrev-table "xfo" "" 'skeleton-beamer-output-listing-frame)

     ;; Filling
     (make-local-variable 'LaTeX-indent-environment-list)
     (add-to-list 'LaTeX-indent-environment-list
                  '("pre" current-indentation))
     (add-to-list 'LaTeX-indent-environment-list
                  '("interface" current-indentation))
     ;; (make-local-variable 'LaTeX-verbatim-regexp)
     ;; (setq LaTeX-verbatim-regexp
     ;;      (concat (or LaTeX-verbatim-regexp nil) "\\|pre\\|interface"))
     (add-to-list 'LaTeX-verbatim-environments-local "pre")
     (add-to-list 'LaTeX-verbatim-environments-local "interface")
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "code")
     (add-to-list 'LaTeX-verbatim-macros-with-braces-local "code")
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "ccode")
     (add-to-list 'LaTeX-verbatim-macros-with-braces-local "ccode")
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "fcode")
     (add-to-list 'LaTeX-verbatim-macros-with-braces-local "fcode")
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "scode")
     (add-to-list 'LaTeX-verbatim-macros-with-braces-local "scode")

     ;; Fontification
     (when (and (fboundp 'font-latex-add-keywords)
                (fboundp 'font-latex-set-syntactic-keywords)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("code" "{{")) 'textual)
       (font-latex-add-keywords '(("ccode" "{")) 'textual)
       (font-latex-add-keywords '(("fcode" "{")) 'textual)
       (font-latex-add-keywords '(("scode" "{")) 'textual)
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

;; Based on endless/fill-or-unfill by Artur Malabarba at
;; <http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html>.
(defun fill-paragraph-dwim ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (if (eq last-command 'fill-paragraph-dwim)
      (progn
        (message "Unfilling...")
        (setq this-command nil)
        (unfill-paragraph))
    (message "Filling...")
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'fill-paragraph-dwim)

(defun fill-paragraph-and-double-space ()
  "Fill paragraph at point, ensuring double spaces at sentence boundaries."
  (interactive)
  (save-excursion
    (let ((end-marker (make-marker))
          (sentence-end-double-space nil))  ; Recognize single space as sentence end
      (set-marker end-marker (progn
                               (forward-paragraph)
                               (skip-syntax-backward "-")
                               (point)))
      (forward-paragraph -1)
      (while (and (< (point) end-marker)
                  (not (eobp)))
        (forward-sentence)
        (forward-char) ; Move past end punctuation
        (unless (>= (point) end-marker)
          ;; Check and add an additional space if there's only one space
          (when (and (equal (char-before) ?\s)
                     (not (equal (char-after) ?\s)))
            (insert " "))
          ;; Skip any extra whitespace or newline
          (skip-syntax-forward "-")))))
  (fill-paragraph))

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
 "Provide the time 24 hours from now in the same format as `current-time'."
 (time-add (current-time) (seconds-to-time 86400)))

(defun yesterday-time ()
 "Provide the time 24 hours before now in the same format as `current-time'."
 (time-subtract (current-time) (seconds-to-time 86400)))

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

(defun jrb-fortran-code-block-region (beg end)
  "Create a tilde-fenced Fortran code block using region from BEG to END.
Formats code blcoks for use on the Fortran Wiki."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n")
    (insert "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (goto-char beg)
    (insert "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {: lang=fortran }\n")))

;; Deleting rather than killing the previous word
;; http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun jrb-flash-mode-line ()
  "Alternative `ring-bell-function' that flashes the mode line.
Avoids visual bell issues in Emacs 24.5 on OS X."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(defun jrb-clean-minibuffer ()
  "Remove unnecessary items from minibuffer before taking screenshots."
  (interactive)
  (abbrev-mode 0)
  (display-time-mode 0))

(defun jrb-separate-line ()
  "Add blank lines before and after current line."
  (interactive "*")
  (save-excursion
    (beginning-of-line)
    (insert "\n")
    (end-of-line)
    (insert "\n")))

(defun swap-windows ()
  "If you have two windows, it swaps them.
From <https://sites.google.com/site/steveyegge2/my-dot-emacs-file>."
  (interactive)
  (cond
   ((not (= (count-windows) 2))
    (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME.
From <https://sites.google.com/site/steveyegge2/my-dot-emacs-file>."
 (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
       (progn (rename-file filename new-name 1) (rename-buffer new-name))))))

(defun jrb-report-properties-in-region (begin end)
  "Report positions, characters, and text properties from BEGIN to END."
  (interactive "r")
  (unless (region-active-p)
    (setq begin (point-min) end (point-max)))
  (let* ((report (format "Properties in region (%d, %d) are as follows:\n" begin end))
         (bufname "*Properties*")
         (buf (get-buffer-create bufname)))
    (dolist (loc (number-sequence begin (1- end)))
      (setq report
            (concat report (format "%4d %c: %s\n" loc
                                   (char-after loc) (text-properties-at loc)))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert report)
      (goto-char (point-min)))
    (view-buffer-other-window buf)))


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
  ("hhem" "jblevins@xbeta.org" nil 0)
  ("wwem" "blevins.141@osu.edu" nil 0)

  ;; common shortcuts
  ("xmd" "markdown" nil 0)
  ("xmm" "markdown-mode" nil 0)
  ("xMd" "Markdown" nil 0)

  ;; typography
  (";dot" "…" nil 0)
  (";en" "–" nil 0)
  (";em" "—" nil 0)

  ;; signatures
  ("ssig1" "Best,\n\nJason" nil 0)
  ("ssig2" "Best,\n\nProf. Blevins" nil 0)
  ("ssig3" "Best,\n\nJason\n\n-- \nJason R. Blevins\nAssociate Professor of Economics\nThe Ohio State University\nhttp://jblevins.org/\n" nil 0)

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
  ("intractible" "intractable" nil 0)
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

  ;; Fill text
  ("llipsum" "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." nil 0)
  ))

;; Don't ask whether to save new abbrevs
(setq save-abbrevs nil)


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
  "link: DELETE IF NOT USED\n"
  "guid: tag:jblevins.org," (my-insert-year) ":"
  (replace-regexp-in-string
   "/\\(home\\|Users\\)/jblevins/work/jblevins.org/htdocs\\(.*?\\)\\(main\\)*\\.text" "\\2"
   (buffer-file-name))
  "\n\n")

(define-skeleton skeleton-f90-header
  "Insert a file header for Fortran source code."
  "Description: "
  "! " (buffer-name) " --- " str "\n"
  "!\n"
  "! Copyright (C) " (my-insert-year) " Jason R. Blevins <jblevins@xbeta.org>\n"
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

(define-skeleton skeleton-beamer-frame
  "Insert an empty Beamer frame."
  "Title: "
  "\\begin{frame}{" str "}\n"
  "  \\begin{itemize}\n"
  "  \\item " _ "\n"
  "  \\end{itemize}\n"
  "\\end{frame}\n"
  "\n")

(define-skeleton skeleton-beamer-listing-frame
  "Insert an empty code listing frame."
  "Title: "
  "\\begin{frame}[fragile]{" str "}\n"
  "\\begin{lstlisting}\n"
  _ "\n"
  "\\end{lstlisting}\n"
  "\\end{frame}\n"
  "\n")

(define-skeleton skeleton-beamer-output-listing-frame
  "Insert an empty code listing frame."
  "Title: "
  "\\begin{frame}[fragile]{" str "}\n"
  "\\begin{lstlisting}[style=output]\n"
  _ "\n"
  "\\end{lstlisting}\n"
  "\\end{frame}\n"
  "\n")


;;; TaskPaper Templates:

(defconst jrb-template-dir "~/gtd/Templates/")
(defconst jrb-template-buffer-name "*Template*")

(defun jrb-find-template ()
  "Find TEMPLATE interactively using the minibuffer."
  (completing-read
   "Template: "
   (let* ((dir (expand-file-name jrb-template-dir))
          (regexp (concat dir "\\([a-z0-9-]+\\)\.taskpaper"))
          (files (directory-files dir t "." t))
          (templates (mapcar (lambda (f) (replace-regexp-in-string regexp "\\1" f)) files)))
     templates)))

(defun jrb-evaluate-template (template buffer)
  (interactive (list (jrb-find-template)
                     (generate-new-buffer-name jrb-template-buffer-name)))
  (switch-to-buffer buffer)
  (taskpaper-mode)
  (insert-file-contents (concat jrb-template-dir template ".taskpaper"))
  (beginning-of-buffer)
  (let ((vars (list)))
    (while (re-search-forward "«[a-zA-Z0-9 ]+»" nil t)
      (add-to-list 'vars (match-string 0)))
    (dolist (var (reverse vars))
      (let ((value (read-from-minibuffer (concat var ": "))))
        (beginning-of-buffer)
        (while (re-search-forward var nil t)
          (replace-match value)))))
  (kill-ring-save (point-min) (point-max)))


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


;;; Smart Punctuation:

(defcustom smart-to-ascii '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'")
                            ;; en-dash
                            ("\x2013" . "-")
                            ;; em-dash
                            ("\x2014" . "--"))
  ""
  :type '(repeat (cons (string :tag "Smart Character  ")
                       (string :tag "ASCII Replacement"))))

(defun replace-smart-to-ascii (beg end)
  (interactive "r")
  (format-replace-strings smart-to-ascii
                          nil beg end))


;;; Local configuration

(load-file "~/.emacs.d.private/init-local.el")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
