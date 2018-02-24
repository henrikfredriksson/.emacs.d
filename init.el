;; (require 'profiler)
;; (profiler-start 'cpu+mem)

(defconst emacs-start-time (current-time))

(setq message-log-max 16384)

;;; Functions

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (defsubst add-load-path (path)
    (add-to-list 'load-path (emacs-path path)))

  (defsubst lookup-password (host user port)
    (require 'auth-source)
    (funcall (plist-get (car (auth-source-search :host host :user user
                                                 :type 'netrc :port port))
                        :secret)))

  (defun get-jobhours-string ()
    (with-current-buffer (get-buffer-create "*scratch*")
      (let ((str (shell-command-to-string "jobhours")))
        (require 'ansi-color)
        (ansi-color-apply (substring str 0 (1- (length str))))))))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

;; (add-hook 'focus-out-hook 'save-all)

;;; Environment

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'seq)

  (defconst emacs-environment (getenv "NIX_MYENV_NAME"))

  (mapc #'add-load-path
        (append (directory-files (emacs-path "site-lisp") t
                                 "site-[A-Z0-9a-z-]+\\'")
                '("site-lisp" "lisp/use-package" "lisp" "")))

  (defun nix-read-environment (name)
    (with-temp-buffer
      (insert-file-contents-literally
       (with-temp-buffer
         (insert-file-contents-literally
          (executable-find (concat "load-env-" name)))
         (and (re-search-forward "^source \\(.+\\)$" nil t)
              (match-string 1))))
      (and (or (re-search-forward "^  nativeBuildInputs=\"\\(.+?\\)\"" nil t)
               (re-search-forward "^  buildInputs=\"\\(.+?\\)\"" nil t))
           (split-string (match-string 1)))))

  (when (executable-find "nix-env")
    (mapc #'(lambda (path)
              (let ((share (expand-file-name "share/emacs/site-lisp" path)))
                (if (file-directory-p share)
                    (add-to-list 'load-path share))))
          (nix-read-environment emacs-environment)))

  (require 'use-package)
  ;; (setq use-package-verbose 'debug)
  (setq use-package-verbose t)
  (setq use-package-expand-minimally nil)
  (setq use-package-compute-statistics nil))

;;(require 'bind-key)
(require 'diminish nil t)

;;; Utility macros and functions

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun get-jobhours-string ()                                 
  (with-current-buffer (get-buffer "*scratch*")               
    (let ((str (shell-command-to-string "jobhours")))          
      (require 'ansi-color)
      (ansi-color-apply (substring str 0 (1- (length str)))))))

;;; Load customization settings

(defvar running-alternate-emacs nil)
(defvar running-development-emacs nil)

(defvar user-data-directory (emacs-path "data"))

(if (string= "emacs26" emacs-environment)
    (load (expand-file-name "settings" user-emacs-directory))
  (let ((settings
         (with-temp-buffer
           (insert-file-contents
            (expand-file-name "settings.el" user-emacs-directory))
           (goto-char (point-min))
           (read (current-buffer))))
        (suffix (cond ;; ((string= "emacs25alt" emacs-environment) "alt")
		 ((string= "emacsHEAD" emacs-environment) "alt")
		 (t "other"))))
    (setq running-development-emacs (string= suffix "dev")
          running-alternate-emacs (string= suffix "alt")
          user-data-directory
          (replace-regexp-in-string "/data" (format "/data-%s" suffix)
                                    user-data-directory))
    (dolist (setting settings)
      (let ((value (and (listp setting)
                        (nth 1 (nth 1 setting)))))
        (if (and (stringp value)
                 (string-match "/\\.emacs\\.d/data" value))
            (setcar (nthcdr 1 (nth 1 setting))
                    (replace-regexp-in-string
                     "/\\.emacs\\.d/data"
                     (format "/.emacs.d/data-%s" suffix)
                     value)))))
    (eval settings)))

(setq Info-directory-list
      (mapcar
       'expand-file-name
       (list
        "~/.emacs.d/info"
        "~/Library/Info"
        (if (executable-find "nix-env")
            (expand-file-name
             "share/info" (car (nix-read-environment emacs-environment)))
          "~/share/info")
        "~/.nix-profile/share/info")))

;;; Configure libraries

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lib" user-emacs-directory)))

(use-package alert         :defer t  :load-path "lisp/alert")
(use-package anaphora      :demand t :load-path "lib/anaphora")
(use-package apiwrap       :defer t  :load-path "lib/apiwrap")
(use-package asoc          :defer t  :load-path "lib/asoc")
(use-package async         :defer t  :load-path "lisp/emacs-async")
(use-package button-lock   :defer t  :load-path "lib/button-lock")
(use-package crux          :demand t :load-path "lib/crux")
(use-package ctable        :defer t  :load-path "lib/emacs-ctable")
(use-package dash          :defer t  :load-path "lib/dash-el")
(use-package deferred      :defer t  :load-path "lib/emacs-deferred")
(use-package difflib       :defer t  :load-path "lib/difflib")
(use-package diminish      :demand t :load-path "lib/diminish")
(use-package el-mock       :defer t  :load-path "lib")
(use-package elisp-refs    :defer t  :load-path "lib/elisp-refs")
(use-package emojify       :defer t  :load-path "lib/emacs-emojify")
(use-package epc           :defer t  :load-path "lib/emacs-epc")
(use-package epl           :defer t  :load-path "lib/epl")
(use-package esxml         :defer t  :load-path "lib/esxml")
(use-package f             :defer t  :load-path "lib/f-el")
(use-package fn            :defer t  :load-path "lib/fn-el")
(use-package fringe-helper :defer t  :load-path "lib/fringe-helper-el")
(use-package fuzzy         :defer t  :load-path "lib/fuzzy-el")
(use-package gh            :defer t  :load-path "lib/gh-el")
(use-package ghub          :defer t  :load-path "lib/ghub")
(use-package ghub+         :defer t  :load-path "lib/ghub-plus")
(use-package ht            :defer t  :load-path "lib/ht-el")
(use-package kv            :defer t  :load-path "lib/kv")
(use-package list-utils    :defer t  :load-path "lib/list-utils")
(use-package logito        :defer t  :load-path "lib/logito")
(use-package loop          :defer t  :load-path "lib/loop")
(use-package m-buffer      :defer t  :load-path "lib/m-buffer")
(use-package makey         :defer t  :load-path "lib/makey")
(use-package marshal       :defer t  :load-path "lib/marshal-el")
(use-package names         :defer t  :load-path "lib/names")
(use-package noflet        :defer t  :load-path "lib/noflet")
(use-package oauth2        :defer t  :load-path "lib/oauth2")
(use-package ov            :defer t  :load-path "lib/ov-el")
(use-package parent-mode   :defer t  :load-path "lib/parent-mode")
(use-package parsebib      :defer t  :load-path "lib/parsebib")
(use-package parsec        :defer t  :load-path "lib/parsec")
(use-package pcache        :defer t  :load-path "lib/pcache")
(use-package peval         :defer t  :load-path "lib/peval")
(use-package pfuture       :defer t  :load-path "lib/pfuture")
(use-package pkg-info      :defer t  :load-path "lib/pkg-info")
(use-package popup         :defer t  :load-path "lib/popup-el")
(use-package popup-pos-tip :defer t  :load-path "lib")
(use-package popwin        :defer t  :load-path "site-lisp/popwin")
(use-package pos-tip       :defer t  :load-path "lib")
(use-package pythonic      :defer t  :load-path "site-lisp/pythonic")
(use-package request       :defer t  :load-path "lib/emacs-request")
(use-package rich-minority :defer t  :load-path "lib/rich-minority")
(use-package s             :defer t  :load-path "lib/s-el")
(use-package spinner       :defer t  :load-path "lib/spinner")
(use-package tablist       :defer t  :load-path "lib/tablist")
(use-package uuidgen       :defer t  :load-path "lib/uuidgen-el")
(use-package web           :defer t  :load-path "lib/emacs-web")
(use-package web-server    :defer t  :load-path "lib/emacs-web-server")
(use-package websocket     :defer t  :load-path "lib/emacs-websocket")
(use-package with-editor   :defer t  :load-path "lib/with-editor")
(use-package xml-rpc       :defer t  :load-path "lib")
(use-package zoutline      :defer t  :load-path "lib/zoutline")


;;; Keybindings

(eval-when-compile
  (setplist 'flet (use-package-plist-delete (symbol-plist 'flet)
                                            'byte-obsolete-info)))

;;; Keymaps

(define-key input-decode-map [?\C-m] [C-m])

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("<C-m>" . my-ctrl-m-map)

          ("C-h e" . my-ctrl-h-e-map)

          ("C-c e" . my-ctrl-c-e-map)
          ("C-c m" . my-ctrl-c-m-map)
          ("C-c y" . my-ctrl-c-y-map)

          ("C-."   . my-ctrl-dot-map)
          ("C-. =" . my-ctrl-dot-equals-map)
          ("C-. f" . my-ctrl-dot-f-map)
          ("C-. g" . my-ctrl-dot-g-map)
          ("C-. h" . my-ctrl-dot-h-map)
          ("C-. m" . my-ctrl-dot-m-map)
          ("C-. r" . my-ctrl-dot-r-map))))

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

;;; Packages


(use-package ggtags
  :disabled t
  :load-path "site-lisp/ggtags"
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package cc-mode
  :disabled t
  :load-path "override/cc-mode"
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :preface
  (defun my-paste-as-check ()
    (interactive)
    (save-excursion
      (insert "/*\n")
      (let ((beg (point)) end)
        (yank)
        (setq end (point-marker))
        (goto-char beg)
        (while (

                (point) end)
          (forward-char 2)
          (insert "CHECK: ")
          (forward-line 1)))
      (insert "*/\n")))

  (defvar printf-index 0)

  (defun insert-counting-printf (arg)
    (interactive "P")
    (if arg
        (setq printf-index 0))
    (if t
        (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                        (setq printf-index (1+ printf-index))))
      (insert (format "printf(\"step %d..\\n\");\n"
                      (setq printf-index (1+ printf-index)))))
    (forward-line -1)
    (indent-according-to-mode)
    (forward-line))

  (defun my-c-mode-common-hook ()
    ;; (ggtags-mode 1)
    (eldoc-mode 1)
    (hs-minor-mode 1)
    (hide-ifdef-mode 1)
    ;; (whitespace-mode 1)
    (which-function-mode 1)
    (company-mode 1)
    (bug-reference-prog-mode 1)

    (diminish 'hs-minor-mode)
    (diminish 'hide-ifdef-mode)

    ;; (bind-key "C-c p" 'insert-counting-printf c-mode-base-map)

    ;;(doxymacs-mode 1)
    ;;(doxymacs-font-lock)

    (bind-key "<return>" #'newline-and-indent c-mode-base-map)

    (unbind-key "M-j" c-mode-base-map)
    (bind-key "C-c C-i" #'c-includes-current-file c-mode-base-map)
    (bind-key "C-c C-y" #'my-paste-as-check c-mode-base-map)

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indicate-empty-lines t)
    (setq fill-column 72)

    (bind-key "M-q" #'c-fill-paragraph c-mode-base-map)

    (let ((bufname (buffer-file-name)))
      (when bufname
        (cond
         ((string-match "/ledger/" bufname)
          (c-set-style "ledger"))
         ((string-match "/edg/" bufname)
          (c-set-style "edg"))
         (t
          (c-set-style "clang")))))

    (font-lock-add-keywords 'c++-mode '(("\\\\(assert\\|DEBUG\\)("
                                         1 font-lock-warning-face t))))

  :config
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

  (setq c-syntactic-indentation nil)

  (bind-key "#" #'self-insert-command c-mode-base-map)
  (bind-key "{" #'self-insert-command c-mode-base-map)
  (bind-key "}" #'self-insert-command c-mode-base-map)
  (bind-key "/" #'self-insert-command c-mode-base-map)
  (bind-key "*" #'self-insert-command c-mode-base-map)
  (bind-key ";" #'self-insert-command c-mode-base-map)
  (bind-key "," #'self-insert-command c-mode-base-map)
  (bind-key ":" #'self-insert-command c-mode-base-map)
  (bind-key "(" #'self-insert-command c-mode-base-map)
  (bind-key ")" #'self-insert-command c-mode-base-map)
  (bind-key "

" #'self-insert-command c++-mode-map)
  (bind-key ">" #'self-insert-command c++-mode-map)

  (add-to-list 'c-style-alist
               '("edg"
                 (indent-tabs-mode           . nil)
                 (c-basic-offset             . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro      . 5)
                     (substatement-open      . 0)
                     (substatement-label     . 0)
                     (label                  . 0)
                     (case-label             . +)
                     (statement-case-open    . 0)
                     (statement-cont         . +)
                     (arglist-intro          . +)
                     (arglist-close          . +)
                     (inline-open            . 0)
                     (brace-list-open        . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook      . c-gnu-impose-minimum)
                 (c-block-comment-prefix     . "")))

  (add-to-list 'c-style-alist
               '("ledger"
                 (indent-tabs-mode           . nil)
                 (c-basic-offset             . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro      . 5)
                     (substatement-open      . 0)
                     (substatement-label     . 0)
                     (label                  . 0)
                     (case-label             . 0)
                     (statement-case-open    . 0)
                     (statement-cont         . +)
                     (arglist-intro          . +)
                     (arglist-close          . +)
                     (inline-open            . 0)
                     (brace-list-open        . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook      . c-gnu-impose-minimum)
                 (c-block-comment-prefix     . "")))

  (add-to-list 'c-style-alist
               '("clang"
                 (indent-tabs-mode           . nil)
                 (c-basic-offset             . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro      . 5)
                     (substatement-open      . 0)
                     (substatement-label     . 0)
                     (label                  . 0)
                     (case-label             . 0)
                     (statement-case-open    . 0)
                     (statement-cont         . +)
                     (arglist-intro          . +)
                     (arglist-close          . +)
                     (inline-open            . 0)
                     (brace-list-open        . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook      . c-gnu-impose-minimum)
                 (c-block-comment-prefix     . ""))))

;; ;;; PACKAGE CONFIGURATIONS

(use-package abbrev
  :diminish
  :hook
  ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook 'indent-according-to-mode)
       (add-hook 'expand-jump-hook 'indent-according-to-mode)))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package ace-window
  :load-path "site-lisp/ace-window"
  :bind ("<C-return>" . ace-window))

(use-package ag
  :disabled t
  :commands ag-project
  :load-path "site-lisp/ag")

(use-package agda2-mode
  :disabled t
  :mode "\\.agda\\'"
  :load-path "site-lisp/agda/src/data/emacs-mode"
  :defines agda2-mode-map
  :preface
  (defun agda2-insert-helper-function (&optional prefix)
    (interactive "P")
    (let ((func-def (with-current-buffer "*Agda information*"
                      (buffer-string))))
      (save-excursion
        (forward-paragraph)
        (let ((name (car (split-string func-def " "))))
          (insert "  where\n    " func-def "    " name " x = ?\n")))))

  :init
  (use-package agda-input)
  :config
  (bind-key "C-c C-i" #'agda2-insert-helper-function agda2-mode-map)

  (defun char-mapping (key char)
    (bind-key key `(lambda () (interactive) (insert ,char))))

  (char-mapping "A-G" "Γ")
  (char-mapping "A-l" "λ x → ")
  (char-mapping "A-:" " ∷ ")
  (char-mapping "A-r" " → ")
  (char-mapping "A-~" " ≅ ")
  (char-mapping "A-=" " ≡ "))

(use-package aggressive-indent
  :load-path "site-lisp/aggressive-indent-mode"
  :defer
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package alert 
  :load-path "lisp/alert"
  :commands alert)

(use-package align 
  :bind (("M-["   . align-code)
         ("C-c [" . align-regexp)
         ("C-c ]" . align-whitespace))
  :commands align
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark))))

  (defun align-whitespace (start end)
    "Align columns by whitespace"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)\\s-" 1 0 t)))

(use-package ascii
  :disabled t
  :bind ("C-c e A" . ascii-toggle)
  :commands ascii-on
  :functions ascii-off
  :preface
  (defun ascii-toggle ()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))

(use-package auto-yasnippet
  :load-path "site-lisp/auto-yasnippet"
  :bind (("C-. w" . aya-create)
         ("C-. y" . aya-expand)
         ("C-. o" . aya-open-line)))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))

(use-package avy
  :load-path "site-lisp/avy"
  :defer 10
  :bind ("M-h" . avy-goto-char)
  :config
  (avy-setup-default))

(use-package backup-each-save
  :commands backup-each-save
  :preface
  (defun show-backups ()
    (interactive)
    (require 'find-dired)
    (let* ((file (make-backup-file-name (buffer-file-name)))
           (dir (file-name-directory file))
           (args (concat "-iname '" (file-name-nondirectory file)
                         ".~*~'"))
           (dired-buffers dired-buffers)
           (find-ls-option '("-print0 | xargs -0 ls -lta" . "-lta")))
      ;; Check that it's really a directory.
      (or (file-directory-p dir)
          (error "Backup directory does not exist: %s" dir))
      (with-current-buffer (get-buffer-create "*Backups*")
        (let ((find (get-buffer-process (current-buffer))))
          (when find
            (if (or (not (eq (process-status find) 'run))
                    (yes-or-no-p "A `find' process is running; kill it? "))
                (condition-case nil
                    (progn
                      (interrupt-process find)
                      (sit-for 1)
                      (delete-process find))
                  (error nil))
              (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))

        (widen)
        (kill-all-local-variables)
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq default-directory dir
              args (concat
                    find-program " . "
                    (if (string= args "")
                        ""
                      (concat
                       (shell-quote-argument "(")
                       " " args " "
                       (shell-quote-argument ")")
                       " "))
                    (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                      (car find-ls-option))
                        (format "%s %s %s"
                                (match-string 1 (car find-ls-option))
                                (shell-quote-argument "{}")
                                find-exec-terminator)
                      (car find-ls-option))))
        ;; Start the find process.
        (message "Looking for backup files...")
        (shell-command (concat args "&") (current-buffer))
        ;; The next statement will bomb in classic dired (no optional arg
        ;; allowed)
        (dired-mode dir (cdr find-ls-option))
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map "\C-c\C-k" 'kill-find)
          (use-local-map map))
        (make-local-variable 'dired-sort-inhibit)
        (setq dired-sort-inhibit t)
        (set (make-local-variable 'revert-buffer-function)
             `(lambda (ignore-auto noconfirm)
                (find-dired ,dir ,find-args)))
        ;; Set subdir-alist so that Tree Dired will work:
        (if (fboundp 'dired-simple-subdir-alist)
            ;; will work even with nested dired format (dired-nstd.el,v 1.15
            ;; and later)
            (dired-simple-subdir-alist)
          ;; else we have an ancient tree dired (or classic dired, where
          ;; this does no harm)
          (set (make-local-variable 'dired-subdir-alist)
               (list (cons default-directory (point-min-marker)))))
        (set (make-local-variable 'dired-subdir-switches)
             find-ls-subdir-switches)
        (setq buffer-read-only nil)
        ;; Subdir headlerline must come first because the first marker in
        ;; subdir-alist points there.
        (insert "  " dir ":\n")
        ;; Make second line a ``find'' line in analogy to the ``total'' or
        ;; ``wildcard'' line.
        (insert "  " args "\n")
        (setq buffer-read-only t)
        (let ((proc (get-buffer-process (current-buffer))))
          (set-process-filter proc (function find-dired-filter))
          (set-process-sentinel proc (function find-dired-sentinel))
          ;; Initialize the process marker; it is used by the filter.
          (move-marker (process-mark proc) 1 (current-buffer)))
        (setq mode-line-process '(":%s")))))

  (bind-key "C-x ~" #'show-backups)

  :init
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (file-truename file)))

  (add-hook 'after-save-hook 'backup-each-save)

  :config
  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  ;; (setq backup-each-save-filter-function 'backup-each-save-filter)

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  (setq backup-enable-predicate 'my-dont-backup-files-p))

(use-package bug-reference-github
  :disabled t
  :load-path "site-lisp/bug-reference-github"
  :defer 10
  :config
  (bug-reference-github-set-url-format))

(use-package bytecomp-simplify
  :disabled t
  :defer 15)

(use-package c-includes
  :disabled t
  :commands c-includes)

(use-package cl-info
  :disabled t)

(use-package cmake-mode
  :disabled t
  :mode (("CMakeLists.txt" . cmake-mode)
         ("\\.cmake\\'"    . cmake-mode)))

(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all)))

(use-package company
  :load-path "site-lisp/company-mode"
  :diminish company-mode
  :commands company-mode
  ;;:bind ("<tab>" . company-complete-common-or-cycle)
  :config
  (setq company-dabbrev-downcase 0)
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))

  (eval-after-load "yasnippet"
    '(progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends)))))

(use-package compile
  :disabled t
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
           (catch 'found
             (dolist(buf (buffer-list))
               (if (string-match "\\*compilation\\*" (buffer-name buf))
                   (throw 'found buf))))))
      (if compile-buf
          (switch-to-buffer-other-window compile-buf)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :config
  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output))


(use-package crosshairs
  :disabled t
  :bind ("M-o c" . crosshairs-mode)
  :config
  (use-package col-highlight
    :config
    (use-package vline)))

(use-package css-mode
  :disabled t
  :mode "\\.css\\'")

(use-package csv-mode
  :disabled t
  :mode "\\.use\\'")

(use-package cursor-chg
  :defer 5
  ;; :commands change-cursor-mode
  :config
  (setq curchg-default-cursor-color 'Black)
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1))

(use-package cus-edit
  :disabled t
  :load-path "lisp/initsplit"
  :defer 5
  :config
  (use-package initsplit))

(use-package dash-at-point
  :load-path "site-lisp/dash-at-point"
  :bind ("C-c D" . dash-at-point)
  :config
  (add-to-list 'dash-at-point-mode-alist '(haskell-mode . "haskell")))

(use-package debbugs-gnu
  :disabled t
  :load-path "elpa/packages/debbugs"
  :commands (debbugs-gnu debbugs-gnu-search))

(use-package dedicated
  :disabled t
  :bind ("C-. D" . dedicated-mode))

(use-package diff-mode
  :commands diff-mode
  :config
  (use-package diff-mode-))

(use-package diffview
  :disabled t
  :commands (diffview-current diffview-region diffview-message))

(use-package dired
  :bind ("C-c J" . dired-double-jump)
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  :config
  (bind-key "l" #'dired-up-directory dired-mode-map)
  (bind-key "<tab>" #'my-dired-switch-window dired-mode-map)
  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

  (use-package dired-x)
  (use-package dired+
    :config
    (unbind-key "M-s f" dired-mode-map))

  (use-package dired-ranger
    :bind (:map dired-mode-map
                ("W" . dired-ranger-copy)
                ("X" . dired-ranger-move)
                ("Y" . dired-ranger-paste)))

  (use-package dired-toggle
    :load-path "site-lisp/site-dired/dired-toggle"
    :bind ("C-. d" . dired-toggle)
    :preface
    (defun my-dired-toggle-mode-hook ()
      (interactive)
      (visual-line-mode 1)
      (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
      (setq-local word-wrap nil))
    :config
    (add-hook 'dired-toggle-mode-hook #'my-dired-toggle-mode-hook))

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig)))))

(use-package docker
  :disabled t
  :defer 15
  :diminish docker-mode
  :load-path "site-lisp/docker-el/"
  :preface
  :config
  (docker-global-mode)
  (use-package docker-images)
  (use-package docker-containers)
  (use-package docker-volumes)
  (use-package docker-networks)
  (use-package docker-machine))

(use-package dockerfile-mode
  :disabled t
  :mode (".*Dockerfile.*" . dockerfile-mode)
  :load-path "site-lisp/dockerfile-mode/")

(use-package dot-gnus
  :disabled t
  :load-path ("override/gnus/lisp" "override/gnus/contrib")
  :bind (("M-G"   . switch-to-gnus)
         ("C-x m" . compose-mail))
  :init
  (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
        gnus-home-directory "~/Messages/Gnus/"))

(use-package dot-org
  :load-path ("override/org-mode/contrib/lisp"
              "override/org-mode/lisp")
  :mode (("\\.org\\'" . org-mode)
         ("\\.txt\\'" . org-mode))
  :commands my-org-startup
  :bind (("M-C"   . jump-to-org-agenda)
         ("M-m"   . org-smart-capture)
         ("M-M"   . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read))
  :defer 10
  :config
  (setq org-babel-python-command    "/anaconda3/bin/python3.6")
  ;; (when (and nil
  ;;            (not running-alternate-emacs)
  ;;            (not running-development-emacs))
  ;;   (run-with-idle-timer 300 t 'jump-to-org-agenda)
  ;;   (my-org-startup))
  )

(use-package doxymacs
  :disabled t
  :load-path "site-lisp/doxymacs/lisp/")

(use-package edebug
  :defer t
  :preface
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp
    (concat "(\\s-*"
            "\\(defun\\|defmacro\\)\\s-+"
            "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-excursion
        (search-backward-regexp modi/fns-regexp)
        (setq fn (match-string 1))
        (mark-sexp)
        (narrow-to-region (point) (mark))
        (if (member fn modi/fns-in-edebug)
            ;; If the function is already being edebugged, uninstrument it
            (progn
              (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
              (eval-region (point) (mark))
              (setq-default eval-expression-print-length 12)
              (setq-default eval-expression-print-level  4)
              (message "Edebug disabled: %s" fn))
          ;; If the function is not being edebugged, instrument it
          (progn
            (add-to-list 'modi/fns-in-edebug fn)
            (setq-default eval-expression-print-length nil)
            (setq-default eval-expression-print-level  nil)
            (edebug-defun)
            (message "Edebug: %s" fn)))
        (widen)))))

(use-package ediff
  :disabled t
  :config
  (use-package ediff-keep)
  (bind-keys :prefix-map ctl-period-equals-map
             :prefix "C-. ="
             ("b" . ediff-buffers)
             ("B" . ediff-buffers3)
             ("c" . compare-windows)
             ("=" . ediff-files)
             ("f" . ediff-files)
             ("F" . ediff-files3)
             ("r" . ediff-revision)
             ("p" . ediff-patch-file)
             ("P" . ediff-patch-buffer)
             ("l" . ediff-regions-linewise)
             ("w" . ediff-regions-wordwise)))

(use-package edit-env
  :disabled t
  :commands edit-env)

(use-package edit-var
  :bind ("C-c e v" . edit-variable))

(use-package eshell
  :disabled t
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (defun ss (server)
      (interactive "sServer: ")
      (call-process "spawn" nil nil nil "ss" server))

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)

  (use-package esh-toggle
    :bind ("C-x C-z" . eshell-toggle)))


(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package etags
  :disabled t
  :bind ("M-T" . tags-search))

(use-package eval-expr
  :load-path "site-lisp/eval-expr"
  :bind ("M-:" . eval-expr)
  :preface 
  (defun my-elisp-indent-or-complete (&optional arg)
    (interactive "p")
    (call-interactively 'lisp-indent-line)
    (unless (or (looking-back "^\\s-*")
                (bolp)
                (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
      (call-interactively 'lisp-complete-symbol)))
  :config
  (setq eval-expr-print-function 'pp
        eval-expr-print-level 20
        eval-expr-print-length 100)

  (defun eval-expr-minibuffer-setup ()
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)
    (local-set-key (kbd "<tab>") #'my-elisp-indent-or-complete)))

(use-package eww
  :disabled t
  :bind ("A-M-g" . eww))

(use-package expand-region
  :load-path "site-lisp/expand-region-el"
  :commands er/expand-region
  :bind (("M-+" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package fancy-narrow
  :disabled t
  :load-path "site-lisp/fancy-narrow"
  :diminish fancy-narrow-mode
  ;;:commands (fancy-narrow-to-region fancy-widen)
  :config (fancy-narrow-mode))

(use-package flycheck
  :load-path "site-lisp/flycheck"
  :commands flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)
           ("C-c i w" . ispell-word)))
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package gist
  :disabled t
  :load-path "site-lisp/gist"
  :bind ("C-c G" . my-gist-region-or-buffer)
  :preface
  (defun my-gist-region-or-buffer ()
    (interactive)
    (deactivate-mark)
    (with-temp-buffer
      (if buffer-file-name
          (call-process "gist" nil t nil "-f" buffer-file-name "-P")
        (call-process "gist" nil t nil "-P"))
      (kill-ring-save (point-min) (1- (point-max)))
      (message (buffer-substring (point-min) (1- (point-max)))))))

(use-package git-link
  :disabled t
  :bind ("C-. G" . git-link)
  :commands (git-link git-link-commit git-link-homepage)
  :load-path "site-lisp/git-link")

(use-package git-timemachine
  :disabled t
  :load-path "site-lisp/git-timemachine"
  :commands git-timemachine)

(use-package graphviz-dot-mode
  :disabled t
  :mode "\\.dot\\'")

(use-package grep
  :disabled t
  :bind (("M-s d" . find-grep-dired)
         ("M-s n" . find-name-dired)
         ("M-s f" . find-grep)
         ("M-s G" . grep))
  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package grep-ed)))

  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("rg --no-heading --color=always -j4 -nH -e " . 43)
   ;; '("find . -name '*.v' -type f -print0 | xargs -P4 -0 egrep -nH " . 61)
   ))

(use-package gud
  :disabled t
  :commands gud-gdb
  :bind ("C-. g" . show-debugger)
  :init
  (defun show-debugger ()
    (interactive)
    (let ((gud-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*gud-" (buffer-name buf))
                   (throw 'found buf))))))
      (if gud-buf
          (switch-to-buffer-other-window gud-buf)
        (call-interactively 'gud-gdb))))
  :config
  (progn
    (bind-key "<f9>" #'gud-cont)
    (bind-key "<f10>" #'gud-next)
    (bind-key "<f11>" #'gud-step)
    (bind-key "S-<f11>" #'gud-finish)))


(use-package haskell-mode-autoloads
  :load-path "site-lisp/haskell-mode"
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :init
  (setenv "PATH" (concat "~/Library/Haskell/bin:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "~/Library/Haskell/bin")

  :preface
  (defvar interactive-haskell-mode-map)
  (defun snippet (name)
    (interactive "sName: ")
    (find-file (expand-file-name (concat name ".hs") "~/src/notes"))
    (haskell-mode)
    (goto-char (point-min))
    (when (eobp)
      (insert "hdr")
      (yas-expand)))

  (defvar hoogle-server-process nil)
  (defun my-haskell-hoogle (query &optional arg)
    "Do a Hoogle search for QUERY."
    (interactive
     (let ((def (haskell-ident-at-point)))
       (if (and def (symbolp def)) (setq def (symbol-name def)))
       (list (read-string (if def
                              (format "Hoogle query (default %s): " def)
                            "Hoogle query: ")
                          nil nil def)
             current-prefix-arg)))
    (unless (and hoogle-server-process
                 (process-live-p hoogle-server-process))
      (message "Starting local Hoogle server on port 8687...")
      (with-current-buffer (get-buffer-create " *hoogle-web*")
        (cd temporary-file-directory)
        (setq hoogle-server-process
              (start-process "hoogle-web" (current-buffer) "hoogle"
                             "server" "--local" "--port=8687")))
      (message "Starting local Hoogle server on port 8687...done"))
    (browse-url
     (format "http://127.0.0.1:8687/?hoogle=%s"
             (replace-regexp-in-string
              " " "+" (replace-regexp-in-string "\\+" "%2B" query)))))

  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)

      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)))

  :config
  (require 'haskell-mode)
  (require 'haskell-font-lock)

  (bind-key "C-c C-u" (lambda () (interactive) (insert "undefined")) haskell-mode-map)

  (unbind-key "M-s" haskell-mode-map)
  (unbind-key "M-t" haskell-mode-map)

  (bind-key "C-c C-h" #'my-haskell-hoogle haskell-mode-map)

  (defun my-haskell-mode-hook ()
    (haskell-indentation-mode)
    (interactive-haskell-mode)
    (unbind-key "C-c c" interactive-haskell-mode-map)
    (flycheck-mode)
    (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
    (prettify-symbols-mode)
    (bug-reference-prog-mode 1)
    (whitespace-mode 1))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

  (use-package flycheck-haskell
    :load-path "site-lisp/flycheck-haskell"
    :config
    (flycheck-haskell-setup)
    (bind-key "M-n" #'flycheck-next-error haskell-mode-map)
    (bind-key "M-p" #'flycheck-previous-error haskell-mode-map))

  (use-package haskell-edit
    :load-path "lisp/haskell-config"
    :config (bind-key "C-c M-q" #'haskell-edit-reformat haskell-mode-map))

  (eval-after-load 'align
    '(nconc
      align-rules-list
      (mapcar (lambda (x) `(,(car x)
                       (regexp . ,(cdr x))
                       (modes quote (haskell-mode literate-haskell-mode))))
              '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
                (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+"))))))

(use-package helm
  :load-path "site-lisp/helm"
  :defer t
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("C-i"   . helm-execute-persistent-action)
              ("C-z"   . helm-select-action)
              ("A-v"   . helm-previous-page))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-dash
  :disabled t
  :after helm
  :load-path "site-lisp/helm-dash"
  :commands helm-dash)

(use-package helm-descbinds
  :load-path "site-lisp/helm-descbinds"
  :after (helm)
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-describe-modes
  :disabled t
  :load-path "site-lisp/helm-describe-modes"
  :after helm
  :bind ("C-h m" . helm-describe-modes))

(use-package helm-hoogle
  :load-path "lisp/helm-hoogle"
  :after helm
  :commands helm-hoogle
  ;;:init (bind-key "A-M-h" #'helm-hoogle haskell-mode-map)
  :config
  (add-hook
   'helm-c-hoogle-transform-hook
   #'(lambda ()
       (goto-char (point-min))
       (while (re-search-forward "file:///nix/store" nil t)
         (replace-match "http://127.0.0.1:8687/file//nix/store" t t)))))

(use-package helm-navi
  :disabled t
  :load-path "site-lisp/helm-navi"
  :after (helm navi)
  :commands helm-navi)

(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

(use-package hilit-chg
  :bind ("M-o C" . highlight-changes-mode))

(use-package hippie-exp
  :bind (("M-/" . dabbrev-expand)
         ("M-?" . hippie-expand))
  :preface
  (autoload 'yas-expand "yasnippet" nil t)

  (defun my-yas-hippie-try-expand (first-time)
    (if (not first-time)
        (let ((yas-fallback-behavior 'return-nil))
          (yas-expand))
      (undo 1)
      nil))

  (defun my-hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible completions generated by `hippie-expand'.
   The optional argument can be generated with `make-hippie-expand-function'."
    (let ((this-command 'my-hippie-expand-completions)
          (last-command last-command)
          (buffer-modified (buffer-modified-p))
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (flet ((ding))        ; avoid the (ding) when hippie-expand exhausts its
                                        ; options.
        (while (progn
                 (funcall hippie-expand-function nil)
                 (setq last-command 'my-hippie-expand-completions)
                 (not (equal he-num -1)))))
      ;; Evaluating the completions modifies the buffer, however we will finish
      ;; up in the same state that we began.
      (set-buffer-modified-p buffer-modified)
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table))))

  (defmacro my-ido-hippie-expand-with (hippie-expand-function)
    "Generate an interactively-callable function that offers ido-based
  completion using the specified hippie-expand function."
    `(call-interactively
      (lambda (&optional selection)
        (interactive
         (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
           (if options
               (list
                ;; (ido-completing-read "Completions: " options)
                (completing-read "Completions: " options)
                ))))
        (if selection
            (he-substitute-string selection t)
          (message "No expansion found")))))

  (defun my-ido-hippie-expand ()
    "Offer ido-based completion for the word at point."
    (interactive)
    (my-ido-hippie-expand-with 'hippie-expand))

  (defun my-try-expand-company (old)
    (require 'company)
    (unless company-candidates
      (company-auto-begin))
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     company-candidates))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-tag-beg ()
    (save-excursion
      (backward-word 1)
      (point)))

  (defun tags-complete-tag (string predicate what)
    (save-excursion
      ;; If we need to ask for the tag table, allow that.
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))))

  (defun try-expand-tag (old)
    (when tags-table-list
      (unless old
        (he-init-string (he-tag-beg) (point))
        (setq he-expand-list
              (sort (all-completions he-search-string 'tags-complete-tag)
                    'string-lessp)))
      (while (and he-expand-list
                  (he-string-member (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
          (progn
            (when old (he-reset-string))
            ())
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)))

  (defun my-dabbrev-substring-search (pattern &optional reverse limit)
    (let ((result ())
          (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
                         (concat (regexp-quote pattern) "\\sw+"))
                        ((eq (char-syntax (aref pattern 0)) ?_)
                         (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
                        (t
                         (concat (regexp-quote pattern)
                                 "\\(\\sw\\|\\s_\\)+")))))
      (while (and (not result)
                  (if reverse
                      (re-search-backward regpat limit t)
                    (re-search-forward regpat limit t)))
        (setq result (buffer-substring-no-properties
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (skip-syntax-backward "w_")
                        (point))
                      (match-end 0)))
        (if (he-string-member result he-tried-table t)
            (setq result nil)))     ; ignore if bad prefix or already in table
      result))

  (defun try-my-dabbrev-substring (old)
    (let ((old-fun (symbol-function 'he-dabbrev-search)))
      (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
      (unwind-protect
          (try-expand-dabbrev old)
        (fset 'he-dabbrev-search old-fun))))

  (defun try-expand-flexible-abbrev (old)
    "Try to complete word using flexible matching.
  Flexible matching works by taking the search string and then
  interspersing it with a regexp for any character. So, if you try
  to do a flexible match for `foo' it will match the word
  `findOtherOtter' but also `fixTheBoringOrange' and
  `ifthisisboringstopreadingnow'.
  The argument OLD has to be nil the first call of this function, and t
  for subsequent calls (for further possible completions of the same
  string).  It returns t if a new completion is found, nil otherwise."
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     (he-flexible-abbrev-collect he-search-string)))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-flexible-abbrev-collect (str)
    "Find and collect all words that flex-matches STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (let ((collection nil)
          (regexp (he-flexible-abbrev-create-regexp str)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t)
          ;; Is there a better or quicker way than using `thing-at-point'
          ;; here?
          (setq collection (cons (thing-at-point 'word) collection))))
      collection))

  (defun he-flexible-abbrev-create-regexp (str)
    "Generate regexp for flexible matching of STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
            "\\w*" "\\b"))

  (defun my-try-expand-dabbrev-visible (old)
    (save-excursion (try-expand-dabbrev-visible old)))

  :config
  (setq hippie-expand-try-functions-list
        '(my-yas-hippie-try-expand
          my-try-expand-company
          try-my-dabbrev-substring
          my-try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-tag
          try-expand-flexible-abbrev
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-line-all-buffers
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (bind-key "M-i" #'my-ido-hippie-expand)

  (defadvice he-substitute-string (after he-paredit-fix)
    "remove extra paren when expanding line in paredit"
    (if (and paredit-mode (equal (substring str -1) ")"))
        (progn (backward-delete-char 1) (forward-char)))))

(use-package hl-line
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  (use-package hl-line+))

(use-package hydra
  :load-path "site-lisp/hydra"
  :defer 10
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package ido
  :disabled t
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window))
  :preface
  (eval-when-compile
    (defvar ido-require-match)
    (defvar ido-cur-item)
    (defvar ido-show-confirm-message)
    (defvar ido-selected)
    (defvar ido-final-text))

  (defun ido-smart-select-text ()
    "Select the current completed item.  Do NOT descend into directories."
    (interactive)
    (when (and (or (not ido-require-match)
                   (if (memq ido-require-match
                             '(confirm confirm-after-completion))
                       (if (or (eq ido-cur-item 'dir)
                               (eq last-command this-command))
                           t
                         (setq ido-show-confirm-message t)
                         nil))
                   (ido-existing-item-p))
               (not ido-incomplete-regexp))
      (when ido-current-directory
        (setq ido-exit 'takeprompt)
        (unless (and ido-text (= 0 (length ido-text)))
          (let ((match (ido-name (car ido-matches))))
            (throw 'ido
                   (setq ido-selected
                         (if match
                             (replace-regexp-in-string "/\\'" "" match)
                           ido-text)
                         ido-text ido-selected
                         ido-final-text ido-text)))))
      (exit-minibuffer)))

  :config
  (ido-mode 'buffer)

  (use-package ido-hacks
    :demand t
    :load-path "site-lisp/ido-hacks"
    :bind ("M-x" . my-ido-hacks-execute-extended-command)
    :config
    (ido-hacks-mode 1)

    (defvar ido-hacks-completing-read (symbol-function 'completing-read))
    (fset 'completing-read ido-hacks-orgin-completing-read-function)
    (defun my-ido-hacks-execute-extended-command (&optional arg)
      (interactive "P")
      (flet ((completing-read
              (prompt collection &optional predicate require-match
                      initial-input hist def inherit-input-method)
              (funcall ido-hacks-completing-read
                       prompt collection predicate require-match
                       initial-input hist def inherit-input-method)))
        (ido-hacks-execute-extended-command arg))))

  (use-package flx-ido
    :load-path "site-lisp/flx"
    :config
    (flx-ido-mode 1))

  (add-hook 'ido-minibuffer-setup-hook
            #'(lambda ()
                (bind-key "<return>" #'ido-smart-select-text
                          ido-file-completion-map))))

(use-package iedit
  :load-path "site-lisp/iedit"
  :bind (("C-;" . iedit-mode)))

(use-package ielm
  :disabled t
  :bind ("C-c :" . ielm)
  :config
  (defun my-ielm-return ()
    (interactive)
    (let ((end-of-sexp (save-excursion
                         (goto-char (point-max))
                         (skip-chars-backward " \t\n\r")
                         (point))))
      (if (>= (point) end-of-sexp)
          (progn
            (goto-char (point-max))
            (skip-chars-backward " \t\n\r")
            (delete-region (point) (point-max))
            (call-interactively #'ielm-return))
        (call-interactively #'paredit-newline))))

  (add-hook 'ielm-mode-hook
            (function
             (lambda ()
               (bind-key "<return>" #'my-ielm-return ielm-map)))
            t))

(use-package iflipb
  :disabled t
  :load-path "site-lisp/iflipb"
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :preface
  (defvar my-iflipb-auto-off-timeout-sec 2)
  (defvar my-iflipb-auto-off-timer-canceler-internal nil)
  (defvar my-iflipb-ing-internal nil)

  (defun my-iflipb-auto-off ()
    (message nil)
    (setq my-iflipb-auto-off-timer-canceler-internal nil
          my-iflipb-ing-internal nil))

  (defun my-iflipb-next-buffer (arg)
    (interactive "P")
    (iflipb-next-buffer arg)
    (if my-iflipb-auto-off-timer-canceler-internal
        (cancel-timer my-iflipb-auto-off-timer-canceler-internal))
    (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
    (setq my-iflipb-ing-internal t))

  (defun my-iflipb-previous-buffer ()
    (interactive)
    (iflipb-previous-buffer)
    (if my-iflipb-auto-off-timer-canceler-internal
        (cancel-timer my-iflipb-auto-off-timer-canceler-internal))
    (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
    (setq my-iflipb-ing-internal t))

  :config
  (setq iflipb-always-ignore-buffers
        "\\`\\( \\|diary\\|ipa\\|\\.newsrc-dribble\\'\\)"
        iflipb-wrap-around t)

  (defun iflipb-first-iflipb-buffer-switch-command ()
    (not (and (or (eq last-command 'my-iflipb-next-buffer)
                  (eq last-command 'my-iflipb-previous-buffer))
              my-iflipb-ing-internal))))

(use-package image-file
  :disabled t
  :defer
  :config
  (auto-image-file-mode 1))

(use-package indent
  :commands indent-according-to-mode)

(use-package indent-shift
  :bind (("C-c <" . indent-shift-left)
         ("C-c >" . indent-shift-right))
  :load-path "site-lisp/indent-shift")

(use-package indirect
  :disabled t
  :bind ("C-c C" . indirect-region))

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)
  :config
  (defadvice Info-exit (after remove-info-window activate)
    "When info mode is quit, remove the window."
    (if (> (length (window-list)) 1)
        (delete-window))))

(use-package info+
  ;; (shell-command "rm -f site-lisp/info+.el*")
  :disabled t)

(use-package info-look
  :commands info-lookup-add-help)

(use-package interaction-log
  ;; (shell-command "rm -fr site-lisp/interaction-log")
  ;; (shell-command "git remote rm ext/interaction-log")
  :disabled t
  :load-path "site-lisp/interaction-log")

(use-package isearch
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-forward))

  :config
  (bind-key "C-c" #'isearch-toggle-case-fold isearch-mode-map)
  (bind-key "C-t" #'isearch-toggle-regexp isearch-mode-map)
  (bind-key "C-^" #'isearch-edit-string isearch-mode-map)
  (bind-key "C-i" #'isearch-complete isearch-mode-map))

(use-package ivy
  :load-path "site-lisp/swiper"
  :defer 5
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume))
  :bind (:map ivy-minibuffer-map
              ("C-r" . ivy-previous-line-or-history)
              ("M-r" . ivy-reverse-i-search))
  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 20)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-sort-matches-functions-alist '((t)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)  
  :preface
  (defun ivy-done-or-delete-char ()
    (interactive)
    (call-interactively
     (if (eolp)
         #'ivy-done
       #'ivy-delete-char)))

  (defun ivy-alt-done-or-space ()
    (interactive)
    (call-interactively
     (if (= ivy--length 1)
         #'ivy-alt-done
       #'self-insert-command)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  ;; This is the value of `magit-completing-read-function', so that we see
  ;; Magit's own sorting choices.
  (defun my-ivy-completing-read (&rest args)
    (let ((ivy-sort-functions-alist '((t . nil))))
      (apply 'ivy-completing-read args)))

  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))
  :bind (("C-x C-f" . counsel-find-file)
         ("C-c e l" . counsel-find-library)
         ("C-c e q" . counsel-set-variable)
         ("C-h e l" . counsel-find-library)
         ("C-h e u" . counsel-unicode-char)
         ("C-h f"   . counsel-describe-function)
         ("C-x r b" . counsel-bookmark)
         ("M-x"     . counsel-M-x)
         ;; ("M-y"     . counsel-yank-pop)

         ("M-s f" . counsel-rg)
         ("M-s j" . counsel-dired-jump)
         ("M-s n" . counsel-file-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date)))


(use-package js2-mode
  :load-path "site-lisp/js2-mode"
  :mode "\\.js\\'"
  :config
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1)

  (bind-key "M-n" #'flycheck-next-error js2-mode-map)
  (bind-key "M-p" #'flycheck-previous-error js2-mode-map)

  ;; (use-package js2-refactor
  ;;   :load-path "site-lisp/js2-refactor"
  ;;   :config
  ;;   (add-hook 'js2-mode-hook #'js2-refactor-mode))

  ;; (use-package xref-js2
  ;;   :load-path "site-lisp/xref-js2"
  ;;   :config
  ;;   (add-hook 'js2-mode-hook #'js2-refactor-mode)
  ;;   (add-hook 'js2-mode-hook (lambda ()
  ;;                              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  )

(use-package json-mode
  :load-path ("site-lisp/json-mode"
              "site-lisp/json-reformat"
              "site-lisp/json-snatcher")
  :mode "\\.json\\'"
  :config
  (use-package json-reformat)
  (use-package json-snatcher))


(use-package redshank
  :load-path "site-lisp/redshank"
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . redshank-mode))

(use-package elisp-slime-nav
  :load-path "site-lisp/elisp-slime-nav"
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))


(use-package eldoc
  :diminish
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode))



(use-package cldoc
  :commands (cldoc-mode turn-on-cldoc-mode)
  :diminish)

(use-package elint
  :commands 'elint-initialize
  :preface
  (defun elint-current-buffer ()
    (interactive)
    (elint-initialize)
    (elint-current-buffer))

  :config
  (add-to-list 'elint-standard-variables 'current-prefix-arg)
  (add-to-list 'elint-standard-variables 'command-line-args-left)
  (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
  (add-to-list 'elint-standard-variables 'emacs-major-version)
  (add-to-list 'elint-standard-variables 'window-system))

(use-package ert
  :bind ("C-c e t" . ert-run-tests-interactively))

(use-package highlight-cl
  :hook (emacs-lisp-mode . highlight-cl-add-font-lock-keywords))

(use-package indent-guide
  :disabled t
  :load-path "site-lisp/indent-guide"
  :hook (python-mode . indent-guide-mode) 
  :config
  (set-face-background 'indent-guide-face "white")
  (setq indent-guide-char ":"))

(use-package info-lookmore
  :load-path "site-lisp/info-lookmore"
  :after info-look
  :config
  (info-lookmore-elisp-cl)
  (info-lookmore-elisp-userlast)
  (info-lookmore-elisp-gnus)
  (info-lookmore-apropos-elisp))

(use-package lisp-mode
  :defer t
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :preface
  (defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

  (defvar slime-mode nil)
  (defvar lisp-mode-initialized nil)
  (defvar lisp-modes '(emacs-lisp-mode
                       inferior-emacs-lisp-mode
                       ielm-mode
                       lisp-mode
                       inferior-lisp-mode
                       lisp-interaction-mode
                       slime-repl-mode))
  :init
  ;; Change lambda to an actual lambda symbol
  (mapc
   (lambda (major-mode)
     (font-lock-add-keywords
      major-mode
      '(("(\\(lambda\\)\\>"
         (0 (ignore
             (compose-region (match-beginning 1)
                             (match-end 1) ?λ))))
        ("(\\|)" . 'esk-paren-face)
        ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face
            nil t)))))
   lisp-modes)
  (dolist (mode '(ielm-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  lisp-interaction-mode
                  lisp-mode
                  emacs-lisp-mode))
    (font-lock-add-keywords
     mode
     '(("(\\(lambda\\)\\>"
        (0 (ignore
            (compose-region (match-beginning 1)
                            (match-end 1) ?λ))))
       ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t))))))

(use-package lispy
  :load-path "site-lisp/lispy"
  :commands lispy-mode
  :bind (:map lispy-mode-map
              ("M-j"))
  :bind (:map emacs-lisp-mode-map
              ("C-1"     . lispy-describe-inline)
              ("C-2"     . lispy-arglist-inline)
              ("C-c C-j" . lispy-goto)))

(use-package lua-mode
  :disabled tm
  :load-path "site-lisp/lua-mode"
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

(use-package lusty-explorer
  :disabled t
  :demand t
  :load-path "site-lisp/lusty-emacs"
  :bind ("C-x C-f" . my-lusty-file-explorer)
  :preface
  (defun lusty-read-directory ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer))
      (lusty--define-mode-map)
      (let* ((lusty--ignored-extensions-regex
              (concat "\\(?:" (regexp-opt completion-ignored-extensions)
                      "\\)$"))
             (minibuffer-local-filename-completion-map lusty-mode-map)
             (lusty-only-directories t))
        (lusty--run 'read-directory-name default-directory ""))))

  (defun lusty-read-file-name ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer))
      (lusty--define-mode-map)
      (let* ((lusty--ignored-extensions-regex
              (concat "\\(?:" (regexp-opt completion-ignored-extensions)
                      "\\)$"))
             (minibuffer-local-filename-completion-map lusty-mode-map)
             (lusty-only-directories nil))
        (lusty--run 'read-file-name default-directory ""))))

  (defun my-lusty-file-explorer ()
    "Launch the file/directory mode of LustyExplorer."
    (interactive)
    (require 'lusty-explorer)
    (let ((lusty--active-mode :file-explorer)
          (helm-mode-prev (and (boundp 'helm-mode) helm-mode)))
      (if (fboundp 'helm-mode)
          (helm-mode -1))
      (unwind-protect
          (progn
            (lusty--define-mode-map)
            (let* ((lusty--ignored-extensions-regex
                    (concat "\\(?:" (regexp-opt
                                     completion-ignored-extensions) "\\)$"))
                   (minibuffer-local-filename-completion-map lusty-mode-map)
                   (file
                    ;; read-file-name is silly in that if the result is equal
                    ;; to the dir argument, it gets converted to the
                    ;; default-filename argument.  Set it explicitly to "" so
                    ;; if lusty-launch-dired is called in the directory we
                    ;; start at, the result is that directory instead of the
                    ;; name of the current buffer.
                    (lusty--run 'read-file-name default-directory "")))
              (when file
                (switch-to-buffer
                 (find-file-noselect
                  (expand-file-name file))))))
        (if (fboundp 'helm-mode)
            (helm-mode (if helm-mode-prev 1 -1))))))

  :config
  (defun my-lusty-setup-hook ()
    (bind-key "SPC" #'lusty-select-match lusty-mode-map)
    (bind-key "C-d" #'exit-minibuffer lusty-mode-map))

  (add-hook 'lusty-setup-hook 'my-lusty-setup-hook)

  (defun lusty-open-this ()
    "Open the given file/directory/buffer, creating it if not
    already present."
    (interactive)
    (when lusty--active-mode
      (ecase lusty--active-mode
        (:file-explorer
         (let* ((path (minibuffer-contents-no-properties))
                (last-char (aref path (1- (length path)))))
           (lusty-select-match)
           (lusty-select-current-name)))
        (:buffer-explorer (lusty-select-match)))))

  (defvar lusty-only-directories nil)

  (defun lusty-file-explorer-matches (path)
    (let* ((dir (lusty-normalize-dir (file-name-directory path)))
           (file-portion (file-name-nondirectory path))
           (files
            (and dir
                 ;; NOTE: directory-files is quicker but
                 ;;       doesn't append slash for directories.
                 ;;(directory-files dir nil nil t)
                 (file-name-all-completions "" dir)))
           (filtered (lusty-filter-files
                      file-portion
                      (if lusty-only-directories
                          (loop for f in files
                                when (= ?/ (aref f (1- (length f))))
                                collect f)
                        files))))
      (if (or (string= file-portion "")
              (string= file-portion "."))
          (sort filtered 'string<)
        (lusty-sort-by-fuzzy-score filtered file-portion)))))

(use-package macrostep
  :disabled t
  :load-path "site-lisp/macrostep"
  :bind ("C-c e m" . macrostep-expand))

(use-package magit 
  :load-path ("site-lisp/magit/lisp"
              "lib/with-editor")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (when (string-match "\\*magit: \\(.+\\)" (buffer-name))
      (let ((name (format "*git-monitor: %s*"
                          (match-string 1 (buffer-name)))))
        (or (get-buffer name)
            (let ((buf (get-buffer-create name)))
              (ignore-errors
                (start-process "*git-monitor*" buf "git-monitor"
                               "-d" (expand-file-name default-directory)))
              buf)))))

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun lusty-magit-status (dir &optional switch-function)
    (interactive (list (if current-prefix-arg
                           (lusty-read-directory)
                         (or (magit-get-top-dir)
                             (lusty-read-directory)))))
    (magit-status-internal dir switch-function))

  (defun eshell/git (&rest args)
    (cond
     ((or (null args)
          (and (string= (car args) "status") (null (cdr args))))
      (magit-status-internal default-directory))
     ((and (string= (car args) "log") (null (cdr args)))
      (magit-log "HEAD"))
     (t (throw 'eshell-replace-command
               (eshell-parse-command
                "*git"
                (eshell-stringify-list (eshell-flatten-list args)))))))

  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)

  (use-package git-commit
    :defer)

  :config
  (setenv "GIT_PAGER" "")

  (use-package magit-commit
    :config
    (remove-hook 'server-switch-hook 'magit-commit-diff))

  (use-package git-gutter+
    :load-path "site-lisp/git-gutter-plus"
    :diminish git-gutter+-mode
    :config
    (git-gutter+-mode t)
    ;; (setq git-gutter+-window-width 2)
    ;; (setq git-gutter+-modified-sign "☁")
    ;; (setq git-gutter+-added-sign "☀")
    ;; (setq git-gutter+-deleted-sign "☂")
    ;;
    )

  (diminish 'magit-wip-after-save-local-mode)
  (diminish 'magit-wip-after-apply-mode)
  (diminish 'magit-wip-before-change-mode)

  (unbind-key "M-h" magit-mode-map)
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-m" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)
  (unbind-key "<C-return>" magit-file-section-map)

  ;; (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
  ;; (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
  (bind-key "U" #'magit-unstage-all magit-mode-map)
  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode 1)))

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

  (remove-hook 'server-switch-hook 'magit-commit-diff))

(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :config  
  (use-package markdown-preview-mode
    :load-path "site-lisp/markdown-preview-mode"
    :config
    (setq markdown-preview-stylesheets (list "~/Downloads/github.css"))
    ))

(use-package mode-line-bell
  :disabled t
  :load-path "site-lisp/mode-line-bell"
  :defer 5
  :config
  (mode-line-bell-mode))

(use-package multi-term
  :disabled t
  :bind (("C-. t" . multi-term-next)
         ("C-. T" . multi-term))
  :init
  (defun screen ()
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer
            (let ((multi-term-program (executable-find "screen"))
                  (multi-term-program-switches "-DR"))
              (multi-term-get-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (switch-to-buffer term-buffer)))

  :config
  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (require 'term)

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map  "\177" 'term-pager-back-page)))

(use-package multifiles
  :disabled t
  :bind ("C-!" . mf/mirror-region-in-multifile)
  :load-path "site-lisp/multifiles-el")

(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              (")"     . paredit-close-round-and-newline)
              ("M-)"   . paredit-close-round)
              ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ("C-M-l" . paredit-recentre-on-sexp)

              ("C-. D" . paredit-forward-down)
              ("C-. B" . paredit-splice-sexp-killing-backward)
              ("C-. C" . paredit-convolute-sexp)
              ("C-. f" . paredit-splice-sexp-killing-forward)
              ("C-. a" . paredit-add-to-next-list)
              ("C-. A" . paredit-add-to-previous-list)
              ("C-. j" . paredit-join-with-next-list)
              ("C-. J" . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map
              ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map
              ("<return>" . paredit-newline))
  :hook (paredit-mode . (lambda ()
                          (unbind-key "M-r" paredit-mode-map)
                          (unbind-key "M-s" paredit-mode-map)))
  :config
  (use-package paredit-ext
    :after paredit)
  )
;; (or (use-package mic-paren
;;       :defer 5
;;       :config
;;       (paren-activate))
;;     (use-package paren
;;       :defer 5
;;       :config
;;       (show-paren-mode 1)))

(use-package personal
  :after crux
  :preface
  ;; Move these in settings.el
  (setq disabled-command-function nil)
  (set-face-attribute 'region nil :background "#CDE7F0")
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-unset-key (kbd "<C-down-mouse-1>"))
  (setq ns-right-alternate-modifier nil)
  :config
  (define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

  (bind-keys ("C-z"             . delete-other-windows)
             ("C-*"             . goto-matching-parens)

             ("M-!"             . async-shell-command)
             ("M-'"             . insert-pair)
             ("M-\""            . insert-pair)
             ("M-`"             . other-frame)
             ("M-j"             . delete-indentation-forward)
             ("M-J"             . delete-indentation)
             ("M-L"             . mark-line)
             ;;("M-S"             . mark-sentence) 
             ("M-<return>"      . open-line-above)

             ("M-g c"           . goto-char)

             ("<C-M-backspace>" . backward-kill-sexp)

             ("C-h f"           . counsel-describe-function)
             ("C-h v"           . describe-variable)

             ("C-x d"           . delete-whitespace-rectangle)
             ("C-x t"           . toggle-truncate-lines)
             ("C-x K"           . delete-current-buffer-file)

             ("C-x C-d"         . duplicate-line)
             ("C-x C-e"         . pp-eval-last-sexp)
             ("C-x C-v"         . find-alternate-file-with-sudo)

             ("C-x M-q"         . refill-paragraph)

             ("C-c SPC"         . just-one-space)
             ("C-c 0"           . recursive-edit-preserving-window-config-pop)
             ("C-c 1"           . recursive-edit-preserving-window-config)
             ("C-c g"           . goto-line)
             ("C-c f"           . flush-lines)
             ("C-c k"           . keep-lines)
             ("C-c m"           . emacs-toggle-size)
             ("C-c n"           . insert-user-timestamp)
             ("C-c q"           . fill-region)
             ;; ("C-c r"        . replace-regexp)
             ("C-c s"           . replace-string)
             ("C-c u"           . rename-uniquely)
             ("C-c V"           . view-clipboard)
             ("C-c )"           . close-all-parentheses)
             ("C-c ="           . count-matches)
             ("C-c ;"           . comment-or-uncomment-region)

             ("C-c C-z"         . delete-to-end-of-buffer)
             ("C-c C-0"         . copy-current-buffer-name)
             ("C-c M-q"         . unfill-paragraph))

  (bind-keys ("C-h e a"         . apropos-value)
             ("C-h e e"         . view-echo-area-messages)
             ("C-h e f"         . find-function)
             ("C-h e k"         . find-function-on-key)
             ("C-h e l"         . counsel-find-library)
             ("C-h e u"         . counsel-unicode-char)
             ("C-h e v"         . find-variable))

  (bind-keys ("C-c e E"         . elint-current-buffer)
             ("C-c e b"         . do-eval-buffer)
             ("C-c e c"         . cancel-debug-on-entry)
             ("C-c e d"         . debug-on-entry)
             ("C-c e e"         . toggle-debug-on-error)
             ("C-c e f"         . emacs-lisp-byte-compile-and-load)
             ("C-c e i"         . crux-find-user-init-file)
             ("C-c e j"         . emacs-lisp-mode)
             ("C-c e l"         . counsel-find-library)
             ("C-c e P"         . check-papers)
             ("C-c e r"         . do-eval-region)
             ("C-c e s"         . scratch)
             ("C-c e p"         . python-mode)
             ("C-c e z"         . byte-recompile-directory))

  (bind-keys ("S-<return>" . open-line-below)))

(use-package projectile
  :load-path "site-lisp/projectile"
  :defer 10
  :diminish
  ;;:bind* ("C-c TAB" . projectile-find-other-file)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode))


(use-package python
  :defer
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defface paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses.")
  :config 

  (use-package anaconda-mode
    :load-path "site-lisp/anaconda-mode"
    :diminish (anaconda-mode))

  (use-package company-anaconda 
    :load-path "site-lisp/company-anaconda"
    :after company-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    ;;(add-to-list 'company-backends '(company-anaconda :with company-capf))

    ;; (eval-after-load "company"
    ;;   '(add-to-list 'company-backends 'company-anaconda))
    )


  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)
      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))

    (setq indicate-empty-lines t)
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil)
    (setq python-indent-offset 4)
    (flycheck-mode)
    (company-mode)
    (smartparens-mode)
    (whitespace-mode)
    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map))

  (add-hook 'python-mode-hook 'my-python-mode-hook)
  (add-hook 'python-mode-hook 
            (lambda ()
              (font-lock-add-keywords nil 
                                      '(("(\\|)" . 'paren-face)))
              (font-lock-add-keywords nil 
                                      '(("{\\|}" . 'paren-face)))
              (font-lock-add-keywords nil 
                                      '(("\\[\\|\\]" . 'paren-face)))
              )))

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (recentf-mode 1))

(use-package regex-tool
  :commands regex-tool
  :load-path "lisp/regex-tool")

(use-package repeat-insert
  :disabled t
  :commands (insert-patterned
             insert-patterned-2
             insert-patterned-3
             insert-patterned-4))

(use-package restart-emacs
  :load-path "site-lisp/restart-emacs"
  :commands restart-emacs)


(use-package restclient
  :disabled t
  :load-path "site-lisp/restclient"
  :mode ("\\.rest\\'" . restclient-mode))

(use-package ruby-mode
  :disabled t
  :load-path "site-lisp/ruby-mode"
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :functions inf-ruby-keys
  :config
  (use-package yari
    :load-path "site-lisp/yari-with-buttons"
    :init
    (progn
      (defvar yari-helm-source-ri-pages
        '((name . "RI documentation")
          (candidates . (lambda () (yari-ruby-obarray)))
          (action  ("Show with Yari" . yari))
          (candidate-number-limit . 300)
          (requires-pattern . 2)
          "Source for completing RI documentation."))

      (defun helm-yari (&optional rehash)
        (interactive (list current-prefix-arg))
        (when current-prefix-arg (yari-ruby-obarray rehash))
        (helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))

  (defun my-ruby-smart-return ()
    (interactive)
    (when (memq (char-after) '(?\| ?\" ?\'))
      (forward-char))
    (call-interactively 'newline-and-indent))

  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys)
    (bind-key "<return>" #'my-ruby-smart-return ruby-mode-map)
    (bind-key "C-h C-i" #'helm-yari ruby-mode-map))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

(use-package sky-color-clock
  :disabled
  :load-path "site-lisp/sky-color-clock"
  :config
  (require 'solar)
  (sky-color-clock-initialize 56.16156)

  (defvar my-sky-color-string "")
  (put 'my-sky-color-string 'risky-local-variable t)
  (defun update-my-sky-color-string ()
    (setq my-sky-color-string (sky-color-clock)))
  (run-at-time 0 60 #'update-my-sky-color-string)
  (push '(:eval my-sky-color-string) (default-value 'mode-line-format))
  )

(use-package selected
  :load-path "site-lisp/selected"
  :defer 5
  :diminish selected-minor-mode
  :config
  (selected-global-mode 1)
  (bind-key "[" #'align-entire selected-keymap)
  (bind-key "f" #'fill-region selected-keymap)
  (bind-key "U" #'unfill-region selected-keymap)
  (bind-key "d" #'downcase-region selected-keymap)
  (bind-key "r" #'reverse-region selected-keymap)
  (bind-key "s" #'sort-lines selected-keymap)
  (bind-key "u" #'upcase-region selected-keymap))

(use-package session
  :disabled t
  :if (not noninteractive)
  :load-path "site-lisp/session"
  :preface
  (defun remove-session-use-package-from-settings ()
    (when (string= (file-name-nondirectory (buffer-file-name)) "settings.el")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^ '(session-use-package " nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))))))

  ;; expanded folded secitons as required
  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode  '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
        (outline-show-subtree))))

  (defvar server-process nil)

  (defun save-information ()
    (with-temp-message "Saving Emacs information..."
      (recentf-cleanup)

      (loop for func in kill-emacs-hook
            unless (memq func '(exit-gnus-on-exit server-force-stop))
            do (funcall func))

      (unless (or noninteractive
                  running-alternate-emacs
                  running-development-emacs
                  (and server-process
                       (eq 'listen (process-status server-process))))
        (server-start))))

  :config
  (add-hook 'before-save-hook 'remove-session-use-package-from-settings)
  (add-hook 'session-after-jump-to-last-change-hook 'le::maybe-reveal)
  (run-with-idle-timer 60 t 'save-information)
  (add-hook 'after-init-hook 'session-initialize t))

(use-package sh-script
  :defer t
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index")))))

  (add-hook 'shell-mode-hook 'initialize-sh-script))

(use-package sh-toggle
  :bind ("C-. C-z" . shell-toggle))

(use-package skewer-mode
  :disabled t
  :load-path "site-lisp/skewer-mode"
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package slime
  :load-path "site-lisp/slime"
  :commands slime
  :init
  (setq inferior-lisp-program "/Users/johnw/.nix-profile/bin/sbcl"
        slime-contribs '(slime-fancy)))

(use-package smart-mode-line
  :load-path "site-lisp/smart-mode-line"
  :defer 10
  :config
  (sml/setup)
  (sml/apply-theme 'light)
  (remove-hook 'display-time-hook 'sml/propertize-time-string)
  ;; (when (eq system-type 'darwin)
  ;;   (setq mac-right-option-modifier 'super))
  )

(use-package smart-tabs-mode
  :commands smart-tabs-mode
  :load-path "site-lisp/smarttabs")

(use-package smartparens 
  :load-path "site-lisp/smartparens"
  :diminish (smartparens-mode)
  :config
  (bind-keys ("C-<right>"    . sp-forward-slurp-sexp)
             ("C-<left>"     . sp-forward-barf-sexp)
             ("C-M-<left>"   . sp-backward-slurp-sexp)
             ("C-M-<right>"  . sp-backward-barf-sexp)

             ("C-d"          . sp-delete-char)
             ;;("C-k"          . sp-kill-whole-line)
             ("M-d"          . sp-kill-word)
             ("C-M-f"        . sp-forward-sexp)
             ("C-. D"        . sp-down-sexp)
             ("C-M-n"        . sp-up-sexp)
             ("M-I"          . sp-splice-sexp)
             ("M-k"          . sp-raise-sexp)
             ("M-S"          . sp-split-sexp)
             ("M-J"          . sp-join-sexp)
             
             ("C-M-S"        . sp-rewrap-sexp)
             ("M-<down>"     . sp-splice-sexp-killing-forward)
             ;;("M-q"          . sp-indent-defun)
             ("C-j"          . sp-newline)

             ("C-. C" . sp-convolute-sexp) 
             ("C-. B" . sp-splice-sexp-killing-backward)
             ("C-. f" . sp-splice-sexp-killing-forward)
             ("C-. a" . sp-add-to-next-sexp)))

(use-package smerge-mode
  :commands smerge-mode
  :config
  (setq smerge-command-prefix (kbd "C-. C-.")))

(use-package sort-words
  :load-path "site-lisp/sort-words"
  :commands sort-words)

(use-package springboard
  ;; (shell-command "rm -fr lisp/springboard")
  ;; (shell-command "git remote rm ext/springboard")
  :disabled t
  :load-path "lisp/springboard")

(use-package stopwatch
  :disabled t
  :bind ("<f8>" . stopwatch))

(use-package swap-regions 
  :load-path "site-lisp/swap-regions"
  :commands swap-regions)


(use-package swiper
  :after ivy
  :bind ("C-s" . swiper)
  :bind (:map swiper-map
              ("M-y" . yank)
              ("M-%" . swiper-query-replace)
              ("C-." . swiper-avy)
              ;; ("M-c" . swiper-mc)
              ("M-c" . haba/swiper-mc-fixed)
              )
  :bind (:map isearch-mode-map
              ("C-o" . swiper-from-isearch))
  :config
  (defun haba/swiper-mc-fixed ()
    (interactive)
    (setq swiper--current-window-start nil)
    (swiper-mc)))

(use-package tablegen-mode
  :mode ("\\.td\\'" . tablegen-mode))

(use-package tex-site
  :load-path "site-lisp/auctex"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
  (add-to-list 'exec-path "/Library/TeX/texbin")
  ;; :preface
  (require 'tex)
  :config
  (defun latex-help-get-cmd-alist ()    ;corrected version:
    "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
          (setq latex-help-cmd-alist nil)
          (Info-goto-node (concat latex-help-file "Command Index"))
          (goto-char (point-max))
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                  (value (buffer-substring (match-beginning 2)
                                           (match-end 2))))
              (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)


  (use-package company-math
    :disabled
    :load-path "site-lisp/site-company/company-math"
    :after company-mode
    :preface
    (use-package math-symbol-lists
      :load-path "site-lisp/math-symbol-lists"
      :defer t))

  (use-package company-auctex
    :disabled t
    :load-path "site-lisp/company-auctex"
    :after company-mode
    :config
    (company-auctex-init)
    )

  (setq TeX-auto-save nil)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)

  
  (use-package ebib
    :disabled  t
    :load-path "site-lisp/ebib"
    :preface
    (use-package parsebib :load-path "site-lisp/parsebib"))

  (use-package latex
    :disabled t
    :config
    (use-package preview)
    (add-hook 'LaTeX-mode-hook 'reftex-mode)


    ;; (load (expand-file-name "site-lisp/auctex/style/minted"
    ;; user-emacs-directory))


    ;; (info-lookup-add-help :mode 'LaTeX-mode
    ;;                       :regexp ".*"
    ;;                       :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
    ;;                       :doc-spec '(("(latex2e)Concept Index" )
    ;;                                   ("(latex2e)Command Index"))))

    )
  (setq TeX-view-program-list (quote (("Preview" "\"open -a Preview.app %o\""))))
  (use-package latex-extra
    :load-path "site-lisp/latex-extra"
    :disabled t
    :diminish latex-extra-mode
    :config
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

  (defun my-latex-mode-hook ()
    (company-mode t)
    (LaTeX-math-mode t)  
    (smartparens-mode t)
    (flycheck-mode t)
    )

  (add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)
  )

(use-package texinfo
  :disabled t
  :defines texinfo-section-list
  :mode ("\\.texi\\'" . texinfo-mode)
  :config
  (defun my-texinfo-mode-hook ()
    (dolist (mapping '((?b . "emph")
                       (?c . "code")
                       (?s . "samp")
                       (?d . "dfn")
                       (?o . "option")
                       (?x . "pxref")))
      (local-set-key (vector (list 'alt (car mapping)))
                     `(lambda () (interactive)
                        (TeX-insert-macro ,(cdr mapping))))))

  (add-hook 'texinfo-mode-hook 'my-texinfo-mode-hook)

  (defun texinfo-outline-level ()
    ;; Calculate level of current texinfo outline heading.
    (require 'texinfo)
    (save-excursion
      (if (bobp)
          0
        (forward-char 1)
        (let* ((word (buffer-substring-no-properties
                      (point) (progn (forward-word 1) (point))))
               (entry (assoc word texinfo-section-list)))
          (if entry
              (nth 1 entry)
            5))))))

(use-package tiny
  :defer 10
  :load-path "site-lisp/tiny"
  :bind ("C-. N" . tiny-expand))

(use-package transpose-mark
  :defer t
  :commands (transpose-mark
             transpose-mark-line
             transpose-mark-region)
  :load-path "site-lisp/transpose-mark")

(use-package vdiff
  :disabled t
  :commands (vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3)
  :load-path "site-lisp/emacs-vdiff")

(use-package vimish-fold
  :disabled t
  :commands vimish-fold
  :load-path "site-lisp/vimish-fold")

(use-package visual-regexp
  :load-path "site-lisp/visual-regexp"
  :commands (vr/replace
             vr/query-replace)
  :bind (("C-. r" . vr/replace)
         ("C-. M-%" . vr/query-replace))
  :config
  (use-package visual-regexp-steroids
    :load-path "site-lisp/visual-regexp-steroids"))

(use-package web-mode
  :defer t
  :load-path "site-lisp/web-mode"
  :mode ("\\.html\\'" . web-mode)
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  )

(use-package which-key
  :load-path "site-lisp/which-key"
  :diminish which-key-mode
  :defer 5
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2.0)
  )


(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "Depending on the file, maybe clean up whitespace."
    (let ((file (expand-file-name ".clean"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".clean" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (when (and (file-exists-p file)
                 (not (file-exists-p ".noclean"))
                 (not (and buffer-file-name
                           (string-match "\\.texi\\'" buffer-file-name))))
        (add-hook 'write-contents-hooks
                  #'(lambda () (ignore (whitespace-cleanup))) nil t)
        (whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)

  ;; For some reason, having these in settings.el gets ignored if whitespace
  ;; loads lazily.
  (setq whitespace-auto-cleanup t
        whitespace-line-column 110
        whitespace-rescan-timer-time nil
        whitespace-silent t
        whitespace-style '(face trailing lines space-before-tab empty)))

(use-package winner
  :disabled t
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package workgroups
  :disabled t
  :load-path "site-lisp/workgroups"
  :diminish (workgroups-mode)
  :bind-keymap ("C-\\" . wg-map)
  :demand t
  :config
  (workgroups-mode 1)

  (let ((workgroups-file (expand-file-name "workgroups" user-data-directory)))
    (if (file-readable-p workgroups-file)
        (wg-load workgroups-file)))

  (bind-key "C-\\" #'wg-switch-to-previous-workgroup wg-map)
  (bind-key "\\" #'toggle-input-method wg-map))

(use-package xray
  :disabled t
  :commands (xray-symbol xray-position xray-buffer xray-window
                         xray-frame xray-marker xray-overlay xray-screen
                         xray-faces xray-hooks xray-features))

(use-package yaml-mode
  :disabled t
  :load-path "site-lisp/yaml-mode"
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :load-path "site-lisp/yasnippet"
  :defer 10
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y a" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory (emacs-path "snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :disabled t
  :load-path "site-lisp/yasnippet-snippets"
  :after yasnippet)


;;; Layout

(defvar display-name
  (let ((width (display-pixel-width)))
    (cond ((>= width 2560) 'retina-imac)
          ((= width 1920) 'macbook-pro-vga)
          ((= width 1680) 'macbook-pro)
          ((= width 1440) 'retina-macbook-pro))))

(defvar emacs-min-top 30)

(defvar emacs-min-left
  (cond ((eq display-name 'retina-imac) 975)
        ((eq display-name 'macbook-pro-vga) 521)
        ((eq display-name 'macbook-pro-vga) 837)
        (t 521)))

(defconst emacs-min-height
  (cond (running-alternate-emacs 57)
        ((eq display-name 'retina-imac) 57)
        ((eq display-name 'macbook-pro-vga) 67)
        ((eq display-name 'macbook-pro) 47)
        (t 44)))

(defconst emacs-min-width
  (cond (running-alternate-emacs 90)
        ((eq display-name 'retina-imac) 100)
        (t 100)))

(defvar emacs-min-font
  (cond
   ((eq display-name 'retina-imac)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"
      ))
   ((eq display-name 'macbook-pro)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"
      ))
   ((eq display-name 'macbook-pro-vga)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"
      ))
   ((string= (system-name) "ubuntu")
    ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
    "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
    )
   (t
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-17-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
      ))))

(let ((frame-alist
       (list (cons 'top    emacs-min-top)
             (cons 'left   emacs-min-left)
             (cons 'height emacs-min-height)
             (cons 'width  emacs-min-width)
             (cons 'font   emacs-min-font))))
  (setq initial-frame-alist frame-alist))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-font emacs-min-font)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)
  )

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

;;; Finalization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed
                      (float-time
                       (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed))) t))

;;; init.el ends here
