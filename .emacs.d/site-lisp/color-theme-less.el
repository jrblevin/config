;; color-theme-less.el --- A minimalistic color theme for Emacs
;; Revision 2
;;
;; Copyright (C) 2008-2010 Jason R. Blevins <jrblevin@sdf.org>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(require 'color-theme)

(defun color-theme-less ()
  "A minimalistic color scheme by Jason Blevins, created 2008-10-07.
Inspired by color-theme-late-night and the Less is More theme for vim."
  (interactive)
  (let ((color-theme-is-cumulative t))
    (color-theme-dark-erc)
    (color-theme-dark-gnus)
    (color-theme-dark-info)
    (color-theme-install
     '(color-theme-less
       ((background-color . "#000")
	(background-mode . dark)
	(background-toolbar-color . "#000")
	(border-color . "#000")
	(bottom-toolbar-shadow-color . "#000")
	(cursor-color	. "yellow")
	(foreground-color . "gray70")
	(top-toolbar-shadow-color . "#111"))

       ;; Standard font lock faces
       (default ((t (nil))))
       (bold ((t (:bold t))))
       (italic ((t (:bold t))))
       (underline ((t (:bold t))))
       (variable-pitch ((t (nil))))
       (font-lock-comment-face ((t (:bold t :foreground "dim gray"))))
       (font-lock-comment-delimiter-face ((t (:bold t :foreground "dim gray"))))
       (font-lock-function-name-face ((t (:foreground "gray90"))))
       (font-lock-variable-name-face ((t (:foreground "gray90"))))
       (font-lock-constant-face ((t (:foreground "gray90"))))
       (font-lock-doc-string-face ((t (:foreground "gray90"))))
       (font-lock-doc-face ((t (:foreground "gray90"))))
       (font-lock-preprocessor-face ((t (:foreground "gray90"))))
       (font-lock-reference-face ((t (:foreground "gray90"))))
       (font-lock-string-face ((t (:foreground "gray90"))))
       (font-lock-type-face ((t (:bold t :foreground "white"))))
       (font-lock-builtin-face ((t (:bold t :foreground "white"))))
       (font-lock-keyword-face ((t (:bold t :foreground "white"))))
       (font-lock-warning-face ((t (:bold t :foreground "red"))))

       ;; Emacs Interface
       (fringe ((t (:background "#111" :foreground "#444"))))
       (header-line ((t (:background "#333" :foreground "#000"))))
       (menu ((t (:background "#111" :foreground "#444"))))
       (minibuffer-prompt ((t (:foreground "white"))))
       (modeline ((t (:background "gray20" :foreground "white"))))
       (mode-line-inactive ((t (:background "gray20" :foreground "light gray"))))
       (modeline-buffer-id ((t (:background "gray20" :foreground "light gray"))))
       (modeline-mousable ((t (:background "black" :foreground "light gray"))))
       (modeline-mousable-minor-mode ((t (:background "black" :foreground "white"))))
       (region ((t (:background "light gray" :foreground "black"))))
       (secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))
       (tool-bar ((t (:background "#111" :foreground "#777"))))
       (tooltip ((t (:background "#333" :foreground "#777"))))
       (widget-button-face ((t (:bold t :foreground "#888"))))
       (widget-field-face ((t (:bold t :foreground "#999"))))

       ;; Search
       (isearch ((t (:foreground "black" :background "red"))))
       (isearch-lazy-highlight-face ((t (:foreground "red"))))

       ;; Parenthesis matching
       (show-paren-match-face ((t (:foreground "black" :background "light gray"))))
       (show-paren-mismatch-face ((t (:foreground "black" :background "red"))))

       ;; Line highlighting
       (highlight ((t (:background "#1f1f1f" :foreground nil))))
       (highlight-current-line-face ((t (:background "#1f1f1f" :foreground nil))))
       (hl-line ((t (:background "#1f1f1f" :foreground nil))))

       ;; Buttons
       (button ((t (:bold t))))
       (custom-button-face ((t (:bold t :foreground "#999"))))

       ;; Calendar
       (holiday-face ((t (:background "#000" :foreground "#777"))))
       ))))

(provide 'color-theme-less)
