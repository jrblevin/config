;;; fira-code-ligatures.el: Support ligatures in Fira Code font -*-emacs-lisp-*-

;; Based on https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs

;; Regular expressions should be grouped by length and expressed in
;; order of length.

(defconst fira-code-char-regexp-alist
  '(
    ;; ! -> !==, !!!, !!, !=
    (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
    ;; # -> ####, ###, #_(, ##, #(, #?, #[, #_, #{
    (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
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
    ;; ~ -> ~~>, ~~~, ~>, ~=, ~@, ~~, ~-
    (126 . ".\\(?:~[>~]\\|[>=@~-]\\)")))

(if (fboundp 'mac-auto-operator-composition-mode)
    ;; Enable automatic ligatures in Mitsuharu Yamamoto's Mac port of Emacs
    ;; <http://www.math.s.chiba-u.ac.jp/~mituharu/emacs-mac.git>
    (mac-auto-operator-composition-mode)
  ;; Enable ligatures in GNU Emacs
  (dolist (char-regexp fira-code-char-regexp-alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Use Fira Code as default font
(set-face-attribute 'default nil :family "Fira Code")

;; Use Fira Sans when available
(when (x-list-fonts "Fira Sans")
  (set-face-attribute 'variable-pitch nil :family "Fira Sans"))

;; Disable ligatures for strings (e.g., in regular expressions)
(when (x-list-fonts "Fira Mono")
  (set-face-attribute 'font-lock-string-face nil :family "Fira Mono"))

(provide 'fira-code-ligatures)
