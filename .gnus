;;; ~/.gnus: Gnus configuration file.                        -*-emacs-lisp-*-
;;
;; Jason Blevins <jblevins@xbeta.org>
;; Created: Durham, August 28, 2006
;; Last Modified: September 9, 2009 12:38 EDT

;; Profile
(setq user-full-name "Jason Blevins")
(setq user-mail-address "jblevins@xbeta.org")

(setq message-alternative-emails
      (regexp-opt (cons user-mail-address
                        '("jblevins@xbeta.org"
                          "jrblevin@sdf.org"
                          "jrblevin@sdf.lonestar.org"
                          "jrb11@duke.edu"
                          "jrblevin@gmail.com"
                          "jason.r.blevins@duke.edu"))))

(setq gnus-posting-styles
      '((".*" ;; default
         (name "Jason Blevins")
         (address "blevins.141@osu.edu")
         (signature-file "~/.signature"))
        ;; For mail sent to Xbeta
        ((header "to" "jblevins@xbeta.org")
         (address "jblevins@xbeta.org"))
        ;; For mail sent to SDF
        ((header "to" "jrblevin@sdf.lonestar.org")
         (address "jblevins@xbeta.org"))
        ((header "to" "jrblevin@sdf.org")
         (address "jblevins@xbeta.org"))
        ;; Usenet news
        ((message-news-p)
         (address "jblevins@xbeta.org"))))


;; News
(setq gnus-select-method '(nntp "news.sunsite.dk"))

;; Email
; (setq gnus-secondary-select-methods
;        '((nnmaildir "" (directory "~/Mail")
;                     (create-directory "~/Mail/")
;                     (directory-files nnheader-directory-files-safe)
;                     (get-new-mail t))))
; 
; (setq mail-sources `((maildir)))
; (setq mail-source-directory "~/Mail/")
;
; (add-hook 'message-mode-hook
; 	  (lambda ()
; 	    (setq fill-column 72)
; 	    (turn-on-auto-fill)))

;; Use browse-url-generic to open URLs.
(setq gnus-button-url 'browse-url-generic)

;; Citation function (On April 1, 2008 foobar wrote...).
(setq message-citation-line-function 'my-message-insert-citation-line)

;; Quoting function
(setq message-cite-function 'message-cite-original-without-signature)

;; Display html emails via emacs-w3m
;(setq mm-text-html-renderer 'w3m)

;; Prefer plain text over HTML or RTF.
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Integration with bbdb and dired
;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; Appearance
(setq gnus-summary-line-format "%U%R%d %(%{%-20,20n%}%) %B%s% \n")
(setq gnus-treat-display-smileys nil)
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-vertical "│")
  (setq gnus-sum-thread-tree-single-leaf "╰─► "))

;; Quoting messages when replying
(defun my-message-insert-citation-line ()
  "Insert a citation line with time and date."
  (when message-reply-headers
    (let*
        ((addr (mail-extract-address-components
                (mail-header-from message-reply-headers)))
         (name (car addr))
         (email (cadr addr))
         (date (mail-header-date message-reply-headers)))
      (insert "On ")
      (insert (format-time-string "%b %e, %Y, at %l:%M %p, " (gnus-date-get-time date)))
      (if name
          (insert name)
        (insert email))
      (insert " wrote:\n"))))
