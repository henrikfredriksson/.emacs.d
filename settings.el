(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(abbrev-file-name "~/.emacs.d/abbrevs")
 '(ad-redefinition-action (quote accept))
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(alert-default-style (quote fringe))
 '(align-c++-modes (quote (csharp-mode c++-mode c-mode java-mode groovy-mode)))
 '(align-to-tab-stop nil)
 '(allout-command-prefix ".")
 '(ansi-color-names-vector
   ["black" "red" "green" "brown" "blue" "magenta" "blue" "white"])
 '(appt-display-interval 30)
 '(appt-message-warning-time 60)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-default nil)
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:.*" "/tmp" t))))
 '(auto-save-interval 64)
 '(auto-save-list-file-prefix
   "~/.emacs.d/data-other-other-other-other/auto-save-list/.saves-")
 '(auto-save-timeout 2)
 '(avy-case-fold-search nil)
 '(avy-keys (quote (97 111 101 117 105 100 104 116 110 115)))
 '(backward-delete-char-untabify-method (quote untabify))
 '(bbdb-default-country "")
 '(bbdb-file "~/Documents/tasks/bbdb")
 '(bbdb-message-caching-enabled nil)
 '(bbdb-no-duplicates t)
 '(bbdb-offer-save (quote savenoprompt))
 '(bbdb-silent-running t)
 '(bbdb-use-pop-up nil)
 '(bbdb-vcard-import-translation-table
   (quote
    (("CELL\\|CAR" . "Mobile")
     ("WORK" . "Work")
     ("HOME" . "Home")
     ("^$" . "Work"))))
 '(bbdb/mail-auto-create-p nil)
 '(bc-bookmark-file "~/.emacs.d/data-other-other-other-other/breadcrumb")
 '(bind-key-segregation-regexp "\\`\\(\\(C-[chx.] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bmkp-bmenu-commands-file
   "~/.emacs.d/data-other-other-other-other/bmk-bmenu-commands.el")
 '(bmkp-bmenu-state-file
   "~/.emacs.d/data-other-other-other-other/bmk-bmenu-state.el")
 '(bmkp-crosshairs-flag nil)
 '(bmkp-last-as-first-bookmark-file "~/Documents/tasks/bookmarks")
 '(bookmark-default-file "~/Documents/tasks/bookmarks")
 '(byte-compile-verbose nil)
 '(c-default-style
   (quote
    ((java-mode . "gnu")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(calendar-daylight-time-zone-name "PDT")
 '(calendar-latitude 38.547795)
 '(calendar-longitude -121.524102)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "PST")
 '(calendar-time-zone -480)
 '(canlock-password "8d2ee9a7e4658c4ff6d863f91a3dd5340b3918ec")
 '(cc-other-file-alist
   (quote
    (("\\.hs\\'"
      (".hs-boot"))
     ("\\.cc\\'"
      (".hh" ".h"))
     ("\\.hh\\'"
      (".cc" ".C"))
     ("\\.c\\'"
      (".h"))
     ("\\.h\\'"
      (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
     ("\\.C\\'"
      (".H" ".hh" ".h"))
     ("\\.H\\'"
      (".C" ".CC"))
     ("\\.CC\\'"
      (".HH" ".H" ".hh" ".h"))
     ("\\.HH\\'"
      (".CC"))
     ("\\.c\\+\\+\\'"
      (".h++" ".hh" ".h"))
     ("\\.h\\+\\+\\'"
      (".c++"))
     ("\\.cpp\\'"
      (".hpp" ".hh" ".h"))
     ("\\.hpp\\'"
      (".cpp"))
     ("\\.cxx\\'"
      (".hxx" ".hh" ".h"))
     ("\\.hxx\\'"
      (".cxx")))))
 '(cfw:read-date-command
   (lambda nil
     (interactive)
     (let
         ((xs
           (decode-time
            (org-time-string-to-time
             (org-read-date)))))
       (list
        (nth 4 xs)
        (nth 3 xs)
        (nth 5 xs)))))
 '(clean-buffer-list-kill-never-buffer-names
   (quote
    ("*scratch*" "*Messages*" "*server*" "*Group*" "*Org Agenda*" "todo.txt" "&bitlbee")))
 '(clean-buffer-list-kill-never-regexps
   (quote
    ("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")))
 '(clean-buffer-list-kill-regexps (quote (".*")))
 '(column-number-mode t)
 '(company-coq-disabled-features
   (quote
    (hello unicode-math-backend refman-tactic-abbrevs-backend refman-vernac-abbrevs-backend dynamic-symbols-backend)))
 '(company-coq-prettify-symbols-alist
   (quote
    (("|-" . 8866)
     ("True" . 8868)
     ("False" . 8869)
     ("->" . 8594)
     ("-->" . 10230)
     ("<-" . 8592)
     ("<--" . 10229)
     ("<->" . 8596)
     ("<-->" . 10231)
     ("==>" . 10233)
     ("<==" . 10232)
     ("++>" . 10239)
     ("<++" . 11059)
     ("fun" . 955)
     ("forall" . 8704)
     ("exists" . 8707)
     ("/\\" . 8743)
     ("\\/" . 8744)
     ("~" . 172)
     ("+-" . 177)
     ("<=" . 8804)
     (">=" . 8805)
     ("<>" . 8800)
     ("*" . 215)
     ("++" . 10746)
     ("nat" . 120029)
     ("Z" . 8484)
     ("N" . 8469)
     ("Q" . 8474)
     ("Real" . 8477)
     ("bool" . 120121)
     ("Prop" . 120031))))
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-frontend)))
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error))
 '(compilation-search-path
   (quote
    (nil "~/src/gitlib" "~/src/gitlib/gitlib" "~/src/gitlib/gitlib-libgit2" "~/src/gitlib/gitlib-s3" "~/src/gitlib/gitlib-test" "~/src/gitlib/git-monitor" "~/src/c2hsc")))
 '(compilation-skip-threshold 2)
 '(compilation-window-height 100)
 '(coq-compile-before-require t)
 '(coq-holes-minor-mode nil)
 '(coq-maths-menu-enable t)
 '(coq-one-command-per-line nil)
 '(coq-prefer-top-of-conclusion t)
 '(coq-prog-args (quote ("-emacs" "-dont-load-proofs")))
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "~/.emacs.d/settings.el")
 '(custom-raised-buttons nil)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(default-frame-alist (quote ((cursor-color . "#b247ee"))))
 '(default-input-method "latin-1-prefix")
 '(default-major-mode (quote text-mode) t)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(diary-file "~/Documents/tasks/diary")
 '(diff-mode-hook
   (quote
    (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(directory-free-space-args "-kh")
 '(dired-clean-up-buffers-too nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lah")
 '(dired-no-confirm
   (quote
    (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files
   "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\)$\\|^\\.\\.$")
 '(dired-omit-mode nil t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(diredful-init-file "~/.emacs.d/data-other-other-other-other/diredful-conf.el")
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(display-time-use-mail-icon t)
 '(doc-view-resolution 300)
 '(ebib-autogenerate-keys t)
 '(ediff-combination-pattern
   (quote
    ("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))
 '(ediff-diff-options "-w")
 '(ediff-highlight-all-diffs nil)
 '(ediff-show-clashes-only t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edit-server-new-frame nil)
 '(el-get-auto-update-cached-recipes nil)
 '(el-get-dir "~/.emacs.d/site-lisp/")
 '(el-get-generate-autoloads nil)
 '(electric-indent-mode nil)
 '(emacs-lisp-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function))))
     eldoc-mode
     (lambda nil
       (local-set-key
        [(meta 46)]
        (quote find-function))
       (local-set-key
        [(control 109)]
        (quote newline-and-indent))))))
 '(enable-recursive-minibuffers t)
 '(eshell-directory-name "~/.emacs.d/eshell/")
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args "-h")
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (abbreviate-file-name
       (eshell/pwd))
      (if
          (=
           (user-uid)
           0)
          " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands
   (quote
    ("vi" "top" "screen" "less" "lynx" "rlogin" "telnet")))
 '(eudc-inline-expansion-format (quote ("%s <%s>" name email)))
 '(eval-expr-print-function (quote pp))
 '(eww-lnum-actions-link-alist
   (quote
    ("----  Link   ----"
     (102 eww-lnum-visit "Visit")
     (101
      (lambda
        (info)
        (eww-lnum-visit info nil t))
      "Edit URL and visit")
     (70
      (lambda
        (info)
        (eww-lnum-visit info t))
      "Visit in new buffer")
     (69
      (lambda
        (info)
        (eww-lnum-visit info t t))
      "Edit URL and visit in new buffer")
     (98
      (lambda
        (info)
        (eww-lnum-visit info :background))
      "Open in background")
     (66
      (lambda
        (info)
        (eww-lnum-visit info :background t))
      "Edit URL and open in background")
     (100
      (lambda
        (info)
        (save-excursion
          (goto-char
           (cadr info))
          (eww-download)))
      "Download")
     (119
      (lambda
        (info)
        (let
            ((url
              (car info)))
          (kill-new url)
          (message url)))
      "Copy")
     (38
      (lambda
        (info)
        (eww-browse-with-external-browser
         (car info)))
      "Open in external browser")
     (68
      (lambda
        (info)
        (shell-command
         (concat "aria2c -d ~/Downloads -x5 '"
                 (car info)
                 "' &")
         "*Aria*"))
      "Download with Aria"))))
 '(eww-search-prefix "https://startpage.com/do/m/mobilesearch?query=")
 '(fill-column 70)
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")))
 '(find-ls-subdir-switches "-alh")
 '(flx-ido-use-faces nil)
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-standard-error-navigation nil)
 '(flymake-compilation-prevents-syntax-check nil)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(font-lock-verbose nil)
 '(frame-title-format
   (quote
    (:eval
     (concat
      (if buffer-file-name default-directory "%b")
      "    "
      (number-to-string
       (cdr
        (assq
         (quote width)
         (frame-parameters))))
      "x"
      (number-to-string
       (cdr
        (assq
         (quote height)
         (frame-parameters))))))) t)
 '(garbage-collection-messages t)
 '(gc-cons-threshold 32000000)
 '(gdb-find-source-frame t)
 '(gdb-same-frame nil)
 '(ggtags-enable-navigation-keys nil)
 '(ggtags-oversize-limit 1048576)
 '(ggtags-use-sqlite3 t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)) t)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-undo-tree-mode t)
 '(grep-find-command (quote ("ag --noheading --column --ignore branches " . 43)))
 '(haskell-indent-spaces 4)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 4)
 '(haskell-indentation-where-post-offset 4)
 '(haskell-indentation-where-pre-offset 4)
 '(helm-adaptive-history-file
   "~/.emacs.d/data-other-other-other-other/helm-adaptive-history")
 '(helm-buffers-fuzzy-matching t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (ffap)
     (tmm-menubar)
     (find-file)
     (magit-status . ido)
     (dired-do-copy . ido)
     (dired-do-rename . ido)
     (dired-create-directory . ido)
     (mml-attach-file . ido))))
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-grep-default-recurse-command "rg --no-heading --color=always -j4 -n%cH -e %p %f")
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(hi2-ifte-offset 4)
 '(hi2-layout-offset 4)
 '(hi2-left-offset 4)
 '(hi2-show-indentations nil)
 '(hippie-expand-try-functions-list
   (quote
    (yas-hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(holiday-bahai-holidays nil)
 '(hoogle-binary-path "hoogle")
 '(hpaste-announce (quote always))
 '(hpaste-blank-title nil)
 '(hpaste-channel "#haskell")
 '(hpaste-default-lang "haskell")
 '(hpaste-default-nick "hfn")
 '(hpaste-lang (quote always))
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Haskell"
       (or
        (mode . haskell-mode)
        (mode . haskell-cabal-mode)
        (mode . literate-haskell-mode)))
      ("Coq"
       (or
        (mode . coq-mode)
        (name . "^\\*\\(coq\\(-.*\\)?\\|goals\\|response\\)\\*")
        (name . "_CoqProject")))
      ("Nix"
       (mode . nix-mode))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")
        (name . "^\\*\\(sent\\|unsent\\|fetch\\)")
        (name . "^ \\*\\(nnimap\\|nntp\\|nnmail\\|gnus\\|server\\|mm\\*\\)")
        (name . "\\(Original Article\\|canonical address\\|extract address\\)")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (name . "^diary$")
        (mode . org-mode)))
      ("Helm"
       (or
        (mode . helm-mode)
        (name . "\\<helm\\>")))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "^\\*magit")
        (name . "git-monitor")))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibuffer-use-other-window t)
 '(ido-auto-merge-work-directories-length 0)
 '(ido-cannot-complete-command (quote ido-exit-minibuffer))
 '(ido-decorations
   (quote
    ("{" "}" "," ",..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'")))
 '(ido-save-directory-list-file "~/.emacs.d/data-other-other-other-other/ido.last")
 '(ido-use-virtual-buffers t)
 '(ido-use-virtual-buffers-automatically t)
 '(idris-interpreter-flags (quote ("-p" "effects")))
 '(image-dired-dir "~/.emacs.d/data-other-other-other-other/image-dired/")
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "hfn")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(gnus\\|nn\\|message\\|mail\\|mm-\\|smtp\\|send-mail\\|check-mail\\|spam\\|sc-\\)" "~/.emacs.d/gnus-settings.el" nil nil)
     ("\\`\\(org-\\|deft-\\|cfw:\\)" "~/.emacs.d/org-settings.el" nil nil))))
 '(irfc-directory "~/Archives/Admin/RFC/")
 '(ispell-extra-args (quote ("--sug-mode=fast" "--keyboard=dvorak")))
 '(ivy-dynamic-exhibit-delay-ms 200)
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil t)
 '(ivy-magic-tilde nil)
 '(ivy-re-builders-alist (quote ((t . ivy--regex-ignore-order))) t)
 '(ivy-sort-matches-functions-alist (quote ((t))))
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(jist-enable-default-authorized t)
 '(jist-gist-directory "/Users/johnw/src/notes/gists")
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(ledger-file "/Volumes/Files/Accounts/ledger.dat")
 '(ledger-post-use-ido t)
 '(line-number-mode t)
 '(line-spacing nil)
 '(load-prefer-newer t)
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-auto-revert-mode nil)
 '(magit-completing-read-function (quote helm--completing-read-default))
 '(magit-diff-options nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-process-popup-time 15)
 '(magit-push-always-verify nil)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(magit-use-overlays nil)
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(moccur-following-mode-toggle nil)
 '(modelinepos-column-limit 80)
 '(mudel-mode-hook (quote (mudel-add-scroll-to-bottom)))
 '(mudel-output-filter-functions (quote (ansi-color-process-output)))
 '(multi-term-program "/usr/bin/screen")
 '(multi-term-program-switches "-DR")
 '(multi-term-scroll-show-maximum-output t)
 '(my-gnus-thread-sort-functions
   (quote
    (gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-total-score)))
 '(next-line-add-newlines nil)
 '(nix-buffer-directory-name "~/.emacs.d/data-other-other-other-other/nix-buffer")
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote super))
 '(ns-right-control-modifier (quote hyper))
 '(nsm-settings-file
   "/Users/hfn/.emacs.d/data-other-other-other-other/network-security.data")
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-M-RET-may-split-line (quote ((headline) (default . t))))
 '(org-adapt-indentation nil)
 '(org-agenda-auto-exclude-function (quote org-my-auto-exclude-function))
 '(org-agenda-cmp-user-defined (quote org-compare-todo-age))
 '(org-agenda-custom-commands
   (quote
    (("e" "Emacs Tasks" tags "TODO<>\"PROJECT\"&LEVEL<>1"
      ((org-agenda-overriding-header "Emacs Tasks")
       (org-agenda-files
        (quote
         ("~/Documents/tasks/emacs.txt")))))
     ("h" "Current Hotlist" tags "HOT&TODO=\"PROJECT\""
      ((org-agenda-overriding-header "Current Hotlist")))
     ("H" "Non-Hot Projects" tags "-HOT&TODO=\"PROJECT\""
      ((org-agenda-overriding-header "Non-Hot Projects")))
     ("A" "Priority #A tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notregexp)
          "\\=.*\\[#A\\]")))))
     ("b" "Priority #A and #B tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A and #B tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\=.*\\[#C\\]")))))
     ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
      ((org-agenda-overriding-header "Uncategorized items")))
     ("W" "Waiting/delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
      ((org-agenda-overriding-header "Waiting/delegated tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))))
     ("D" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-overriding-header "Deadlined tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notdeadline))))
       (org-agenda-sorting-strategy
        (quote
         (category-up)))))
     ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE\\|PROJECT}&STYLE<>\"habit\""
      ((org-agenda-overriding-header "Scheduled tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notscheduled))))
       (org-agenda-sorting-strategy
        (quote
         (category-up)))))
     ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-overriding-header "Unscheduled tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline)
          (quote timestamp)
          (quote regexp)
          "\\* \\(DEFERRED\\|SOMEDAY\\)")))
       (org-agenda-sorting-strategy
        (quote
         (user-defined-down)))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
     ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
      ((org-agenda-overriding-header "Deferred tasks:")
       (org-agenda-sorting-strategy
        (quote
         (user-defined-down)))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
     ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
      ((org-agenda-overriding-header "Someday tasks:")
       (org-agenda-sorting-strategy
        (quote
         (user-defined-down)))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
     ("w" "Unscheduled work-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-overriding-header "Unscheduled work-related tasks")
       (org-agenda-files
        (quote
         ("~/Documents/tasks/BAE.txt")))
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline)
          (quote timestamp))))))
     ("c" "Appointment Calendar" agenda ""
      ((org-agenda-overriding-header "Appointment Calendar")
       (org-agenda-sorting-strategy
        (quote
         (time-up)))
       (org-agenda-span 14)
       (org-agenda-ndays 14)
       (org-agenda-regexp-filter-preset
        (quote
         ("+APPT")))))
     ("O" "All TODOs" tags "TODO<>\"\""
      ((org-agenda-overriding-header "All TODOs")
       (org-agenda-files
        (quote
         ("~/Documents/tasks/todo.txt" "~/Documents/tasks/BAE.txt" "~/Documents/tasks/Bahai.txt" "~/Documents/tasks/OSS.txt" "~/Documents/tasks/emacs.txt" "~/Documents/tasks/habits.txt" "~/Documents/tasks/index.txt" "~/Documents/tasks/notes.txt" "~/Documents/tasks/archive/BAE.txt" "~/Documents/tasks/archive/Bahai.txt" "~/Documents/tasks/archive/BoostPro.txt" "~/Documents/tasks/archive/CEG.txt" "~/Documents/tasks/archive/Embarcadero.txt" "~/Documents/tasks/archive/FPComplete.txt" "~/Documents/tasks/archive/IL-05.txt" "~/Documents/tasks/archive/TI.txt" "~/Documents/tasks/archive/archive-2007.txt" "~/Documents/tasks/archive/archive-2008.txt" "~/Documents/tasks/archive/archive-2009.txt" "~/Documents/tasks/archive/archive-2010.txt" "~/Documents/tasks/archive/archive-2011.txt" "~/Documents/tasks/archive/archive-2012.txt" "~/Documents/tasks/archive/archive-2013.txt" "~/Documents/tasks/archive/archive-2014.txt" "~/Documents/tasks/archive/archive-2015.txt" "~/Documents/tasks/archive/archive-2016.txt" "~/Documents/tasks/archive/archive.txt" "~/Documents/tasks/archive/emacs.txt"))))))))
 '(org-agenda-deadline-leaders (quote ("!D!: " "D%02d: ")))
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-files
   (quote
    ("~/Documents/tasks/BAE.txt" "~/Documents/tasks/todo.txt" "~/Documents/tasks/habits.txt" "~/Documents/tasks/Bahai.txt" "~/Documents/tasks/emacs.txt" "~/Documents/tasks/OSS.txt")))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c%5(org-todo-age) ")
     (tags . "  %-11c"))))
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-text "")
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up todo-state-up priority-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode t)
 '(org-agenda-tags-column -100)
 '(org-agenda-text-search-extra-files (quote (agenda-archives "~/Documents/tasks/notes.txt")))
 '(org-agenda-use-time-grid nil)
 '(org-archive-location "TODO-archive::")
 '(org-archive-save-context-info (quote (time category itags)))
 '(org-attach-method (quote mv))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (ditaa . t)
     (haskell . t)
     (python . t)
     (calc . t))))
 '(org-beamer-frame-default-options "fragile")
 '(org-capture-templates
   (quote
    (("a" "Add Task" entry
      (file+headline "~/Documents/tasks/todo.txt" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("n" "Note" entry
      (file "~/Documents/tasks/notes.txt")
      "* NOTE %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("c" "Calendar" entry
      (file+headline "~/Documents/tasks/todo.txt" "Inbox")
      "* APPT %?
SCHEDULED: %t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("t" "Add Task" entry
      (file+headline "~/Documents/tasks/todo.txt" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t))))
 '(org-clock-clocked-in-display nil)
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-mode-line-total (quote current))
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-persist t)
 '(org-clock-persist-file "~/.emacs.d/data/org-clock-save.el")
 '(org-clock-resolve-expert t)
 '(org-completion-use-ido t)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-crypt-disable-auto-save nil)
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Documents/tasks/todo.txt")
 '(org-directory "~/Documents/tasks/")
 '(org-ditaa-jar-path "~/bin/DitaaEps/DitaaEps.jar")
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "OUT")))
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-export-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("linalg" "\\documentclass{article}
\\usepackage{linalgjh}
[DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))
 '(org-export-use-babel nil)
 '(org-extend-today-until 8)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-fontify-done-headline t)
 '(org-footnote-section nil)
 '(org-habit-preceding-days 42)
 '(org-habit-today-glyph 45)
 '(org-hide-leading-stars t)
 '(org-id-locations-file "~/.emacs.d/data/org-id-locations")
 '(org-image-actual-width (quote (800)))
 '(org-insert-heading-respect-content t)
 '(org-irc-link-to-logs t t)
 '(org-latex-default-packages-alist
(quote
 (("T1" "fontenc" t)
  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "longtable" nil)
  ("" "float" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "marvosym" t)
  ("" "wasysym" t)
  ("" "amssymb" t)
  ("" "hyperref" nil)
  "\\tolerance=1000")))
 '(org-mobile-agendas (quote ("Z")))
 '(org-mobile-directory "~/Dropbox/Apps/MobileOrg")
 '(org-mobile-files (quote ("~/Documents/tasks/todo.txt")))
 '(org-mobile-files-exclude-regexp "\\(TODO\\(-.*\\)?\\)\\'")
 '(org-mobile-inbox-for-pull "~/Documents/tasks/from-mobile.org")
 '(org-modules (quote (org-gnus org-habit org-info org-depend)))
 '(org-priority-faces
(quote
 ((65 :foreground "ForestGreen" :weight bold)
  (66 . "DarkGreen")
  (67 :foreground "dark gray" :slant italic))))
 '(org-refile-targets
(quote
 (("~/Documents/tasks/todo.txt" :level . 1)
  ("~/Documents/tasks/Bahai.txt" :level . 1)
  ("~/Documents/tasks/emacs.txt" :level . 1)
  ("~/Documents/tasks/OSS.txt" :level . 1)
  ("~/Documents/tasks/BAE.txt" :level . 1)
  (org-agenda-files :todo . "PROJECT"))))
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-smart-capture-use-lastname t)
 '(org-src-fontify-natively t)
 '(org-stuck-projects (quote ("TODO=\"PROJECT\"" nil nil "SCHEDULED:")))
 '(org-tags-column -97)
 '(org-time-clocksum-use-fractional t)
 '(org-todo-keyword-faces
(quote
 (("TODO" :foreground "medium blue" :weight bold)
  ("APPT" :foreground "medium blue" :weight bold)
  ("NOTE" :foreground "brown" :weight bold)
  ("STARTED" :foreground "dark orange" :weight bold)
  ("WAITING" :foreground "red" :weight bold)
  ("DELEGATED" :foreground "dark violet" :weight bold)
  ("DEFERRED" :foreground "dark blue" :weight bold)
  ("SOMEDAY" :foreground "dark blue" :weight bold)
  ("PROJECT" :foreground "#088e8e" :weight bold))))
 '(org-todo-repeat-to-state "TODO")
 '(org-use-property-inheritance (quote ("AREA")))
 '(org-use-speed-commands t)
 '(org-use-tag-inheritance nil)
 '(org-velocity-always-use-bucket t)
 '(org-velocity-bucket "~/Documents/tasks/notes.txt")
 '(org-velocity-capture-templates
(quote
 (("v" "Velocity" entry
   (file "~/Documents/tasks/notes.txt")
   "* NOTE %:search
%i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \\\"uuidgen\\\"):CREATED:  %U
:END:" :prepend t))))
 '(org-velocity-exit-on-match t)
 '(org-velocity-force-new t)
 '(org-velocity-search-method (quote regexp))
 '(org-x-backends (quote (ox-org ox-redmine)))
 '(org-x-redmine-title-prefix-function (quote org-x-redmine-title-prefix))
 '(org-x-redmine-title-prefix-match-function (quote org-x-redmine-title-prefix-match))
 '(pabbrev-idle-timer-verbose nil)
 '(package-archives
(quote
 (("gnu" . "https://elpa.gnu.org/packages/")
  ("MELPA" . "https://melpa.org/packages/")
  ("Marmalade" . "https://marmalade-repo.org/packages/"))))
 '(page-break-lines-modes
(quote
 (emacs-lisp-mode compilation-mode outline-mode prog-mode haskell-mode)))
 '(parens-require-spaces t)
 '(pcomplete-compare-entries-function (quote file-newer-than-file-p))
 '(persistent-scratch-file-name
"~/.emacs.d/data-other-other-other-other/persistent-scratch")
 '(pp^L-^L-string
"                                                                              ")
 '(projectile-cache-file "~/.emacs.d/data-other-other-other-other/projectile.cache")
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")))
 '(projectile-known-projects-file
"~/.emacs.d/data-other-other-other-other/projectile-bookmarks.eld")
 '(projectile-switch-project-action (quote helm-projectile))
 '(proof-auto-action-when-deactivating-scripting (quote retract))
 '(proof-autosend-enable nil)
 '(proof-electric-terminator-enable t)
 '(proof-shell-fiddle-frames nil)
 '(proof-splash-enable nil)
 '(proof-sticky-errors t)
 '(ps-font-size (quote (8 . 10)))
 '(ps-footer-font-size (quote (12 . 14)))
 '(ps-header-font-size (quote (12 . 14)))
 '(ps-header-title-font-size (quote (14 . 16)))
 '(ps-line-number-font-size 10)
 '(ps-print-color-p nil)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 2)
 '(rdebug-many-windows nil)
 '(read-buffer-function (quote ido-read-buffer))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude
(quote
 ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data-other-other-other-other/recentf")
 '(redisplay-dont-pause t t)
 '(reftex-trust-label-prefix t)
 '(regex-tool-backend (quote perl))
 '(rng-schema-locating-files
(quote
 ("schemas.xml" "~/src/schemas.xml" "~/.nix-profile/share/emacs/24.4/etc/schema/schemas.xml")))
 '(runner-init-file "~/.emacs.d/data-other-other-other-other/runner-conf.el")
 '(safe-local-eval-forms
(quote
 ((add-hook
   (quote write-file-hooks)
   (quote time-stamp))
  (add-hook
   (quote write-file-functions)
   (quote time-stamp))
  (add-hook
   (quote before-save-hook)
   (quote time-stamp)
   nil t)
  (add-hook
   (quote before-save-hook)
   (quote delete-trailing-whitespace)
   nil t)
  (progn
    (let
        ((coq-root-directory
          (when buffer-file-name
            (locate-dominating-file buffer-file-name ".dir-locals.el")))
         (coq-project-find-file
          (and
           (boundp
            (quote coq-project-find-file))
           coq-project-find-file)))
      (set
       (make-local-variable
        (quote tags-file-name))
       (concat coq-root-directory "TAGS"))
      (setq camldebug-command-name
            (concat coq-root-directory "dev/ocamldebug-coq"))
      (unless coq-project-find-file
        (set
         (make-local-variable
          (quote compile-command))
         (concat "make -C " coq-root-directory))
        (set
         (make-local-variable
          (quote compilation-search-path))
         (cons coq-root-directory nil)))
      (when coq-project-find-file
        (setq default-directory coq-root-directory)))))))
 '(safe-local-variable-values
(quote
 ((ispell-dictionary . "svenska")
  (TeX-master . t)
  (nix-package-name . "pkgs.haskellPackages_ghc782.newartisans")
  (eval require
        (quote edg))
  (eval ignore-errors
        (require
         (quote edg)))
  (after-save-hook git-commit-changes)
  (shm-lambda-indent-style . leftmost-parent)
  (haskell-indent-spaces . 4)
  (haskell-indent-spaces . 2)
  (haskell-indentation-ifte-offset . 2)
  (haskell-indentation-layout-offset . 2)
  (haskell-indentation-left-offset . 2)
  (haskell-indentation-starter-offset . 2)
  (haskell-indentation-where-post-offset . 2)
  (haskell-indentation-where-pre-offset . 2)
  (coq-prog-args "-emacs" "-no-native-compiler" "-R" "." "Hask")
  (coq-prog-args "-emacs" "-R" "." "Hask"))))
 '(same-window-buffer-names
(quote
 ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-kill-file-name
"~/.emacs.d/data-other-other-other-other/kill-ring-saved.el")
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(semanticdb-default-save-directory "~/.emacs.d/data-other-other-other-other/semanticdb")
 '(sentence-end-double-space nil)
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include
(quote
 ((kill-ring 10 nil)
  (session-file-alist 200 t)
  (file-name-history 200 nil)
  search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-name-disable-regexp "\\(\\`/tmp\\|COMMIT_EDITMSG\\)")
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data-other-other-other-other/session")
 '(shm-auto-insert-bangs nil)
 '(shm-indent-spaces 4)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data-other-other-other-other/slime-history.eld")
 '(slime-startup-animation nil)
 '(smart-compile-alist
(quote
 (((lambda
     (buf)
     (let
         ((case-fold-search t))
       (and
        (string-match "/ansi/"
                      (buffer-file-name buf))
        (string-match "/opencl"
                      (shell-command-to-string "git symbolic-ref HEAD")))))
   . "cd ~/Contracts/TI/src/c60_iaansi ; ~/Contracts/TI/bin/build.sh c60")
  ((lambda
     (buf)
     (let
         ((case-fold-search t))
       (and
        (string-match "/\\(src/ansi\\|src/.*?ansi\\)/"
                      (buffer-file-name buf))
        (string-match "/merge_4_2"
                      (shell-command-to-string "git symbolic-ref HEAD")))))
   . "cd ~/Contracts/TI/src/msp_iaansi ; ~/Contracts/TI/bin/build.sh msp")
  ((lambda
     (buf)
     (string-match "/\\(\\(src\\|Projects\\)/ledger\\)/"
                   (buffer-file-name buf)))
   . "cd ~/src/ledger ; ~/src/ledger/tools/build.sh debug")
  ((lambda
     (buf)
     (string-match "/emacs/"
                   (buffer-file-name buf)))
   . "emacs-build release macport opt make")
  (emacs-lisp-mode emacs-lisp-byte-compile-and-load)
  ((lambda
     (buf)
     (string-match "/\\(cree\\|EDG/Projects/edg\\)/"
                   (buffer-file-name buf)))
   . "cd ~/Products/cree/edg && (ninja && ctest -j$(ncpu)) & (cd ~/src/cree; mktags src ext/llvm) & wait"))))
 '(sp-highlight-pair-overlay nil)
 '(sql-sqlite-program "sqlite3")
 '(sr-attributes-display-mask (quote (nil nil t nil nil nil)))
 '(sr-autoload-extensions nil)
 '(sr-kill-unused-buffers nil)
 '(sr-listing-switches "--time-style=locale --group-directories-first -alDhgG")
 '(sr-loop-use-popups nil)
 '(sr-popviewer-style (quote single-frame))
 '(sr-show-file-attributes nil)
 '(sr-show-hidden-files t)
 '(sr-use-commander-keys nil)
 '(sr-windows-default-ratio 80)
 '(ssl-certificate-verification-policy 1)
 '(svn-status-hide-unmodified t)
 '(switch-to-buffer-preserve-window-point t)
 '(tab-width 2)
 '(tags-apropos-verbose t)
 '(tags-case-fold-search nil)
 '(tail-max-size 25)
 '(tail-volatile nil)
 '(temp-buffer-resize-mode t nil (help))
 '(term-bind-key-alist
(quote
 (("C-c C-c" . term-interrupt-subjob)
  ("C-b" . my-term-send-raw-at-prompt)
  ("C-f" . my-term-send-raw-at-prompt)
  ("C-a" . my-term-send-raw-at-prompt)
  ("C-e" . my-term-send-raw-at-prompt)
  ("C-p" . previous-line)
  ("C-n" . next-line)
  ("C-s" . isearch-forward)
  ("C-r" . isearch-backward)
  ("C-m" . term-send-raw)
  ("M-f" . term-send-forward-word)
  ("M-b" . term-send-backward-word)
  ("M->" . my-term-end-of-buffer)
  ("M-o" . term-send-backspace)
  ("M-p" . term-send-up)
  ("M-n" . term-send-down)
  ("M-d" . term-send-forward-kill-word)
  ("M-DEL" . term-send-backward-kill-word)
  ("M-r" . term-send-reverse-search-history)
  ("M-," . term-send-input)
  ("M-." . comint-dynamic-complete)
  ("C-y" . term-paste))))
 '(term-buffer-maximum-size 0)
 '(term-scroll-show-maximum-output t)
 '(text-mode-hook
(quote
 (turn-on-auto-fill
  (lambda nil
    (ignore-errors
      (diminish
       (quote auto-fill-function)))))))
 '(tls-checktrust t)
 '(tls-program
(quote
 ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CApath /etc/postfix/certs -cert ~/Messages/me.pem")))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.backups")
 '(tramp-default-method-alist
(quote
 (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "sudo"))))
 '(tramp-persistency-file-name "~/.emacs.d/data-other-other-other-other/tramp")
 '(tramp-use-ssh-controlmaster-options nil)
 '(trash-directory "~/.Trash")
 '(undo-limit 800000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.backups"))))
 '(undo-tree-mode-lighter "")
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-cache-directory "~/.emacs.d/data-other-other-other-other/url/cache")
 '(url-configuration-directory "~/.emacs.d/data-other-other-other-other/url/")
 '(url-irc-function (quote url-irc-erc))
 '(user-full-name "Henrik Fredriksson")
 '(user-initials "hfn")
 '(user-mail-address "henrikfredriksson2@gmail.com")
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-default-display-inline-images t)
 '(w3m-fill-column 80)
 '(w3m-use-cookies t)
 '(warning-minimum-log-level :error)
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(wg-mode-line-on nil)
 '(wg-morph-on nil)
 '(wg-prefix-key "")
 '(wg-query-for-save-on-emacs-exit nil)
 '(wg-query-for-save-on-workgroups-mode-exit nil)
 '(whitespace-auto-cleanup t t)
 '(whitespace-line-column 110)
 '(whitespace-rescan-timer-time nil t)
 '(whitespace-silent t t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(workgroups-mode nil)
 '(x-stretch-cursor t)
 '(yas-prompt-functions
(quote
 (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)
 '(zencoding-indentation 2)
 '(zencoding-preview-default nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))))
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(org-agenda-done ((t (:foreground "ForestGreen"))))
 '(org-done ((t (:foreground "ForestGreen" :weight bold))))
 '(org-habit-alert-face ((((background light)) (:background "#f5f946"))))
 '(org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
 '(org-habit-clear-face ((((background light)) (:background "#8270f9"))))
 '(org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
 '(org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
 '(org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
 '(org-habit-ready-face ((((background light)) (:background "#4df946"))))
 '(org-habit-ready-future-face ((((background light)) (:background "#acfca9"))))
 '(org-headline-done ((t nil)))
 '(org-scheduled ((((class color) (min-colors 88) (background light)) nil)))
 '(org-upcoming-deadline ((((class color) (min-colors 88) (background light)) (:foreground "Brown")))))
