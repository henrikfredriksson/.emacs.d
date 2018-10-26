;;; (require 'profiler)
;;; (profiler-start 'cpu+mem)

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      garbage-collection-message t
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.6)
             (garbage-collect)) t)

(run-with-idle-timer 20 t 'garbage-collect)
(setq garbage-collection-messages t)

;;; Functions

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (defsubst add-load-path (path)
    (add-to-list 'load-path(emacs-path path)))

  (defsubst lookup-password (host user port)
    (require 'auth-source)
    (funcall (plist-get (car (auth-source-search: host host: user user: type 'netrc: port port)): secret)))

  (defun get-jobhours-string()
    (with-current-buffer(get-buffer-create "*scratch*")
      (let((str(shell-command-to-string "jobhours")))
        (require 'ansi-color)
        (ansi-color-apply(substring str 0 (1 - (length str))))))))

(defun save-all()
  (interactive)
  (save-some-buffers t));; (add-hook 'focus-out-hook 'save-all)

;; ; Environment

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'seq)

  (defconst emacs-environment (getenv "NIX_MYENV_NAME"))

  (mapc #'add-load-path
        (append (directory-files (emacs-path "site-lisp") t
                                 "site-[A-Z0-9a-z-]+\\'")
                '("site-lisp" "lisp/use-package" "lisp" "")))

  (defun nix-read-environment(name)
    (with-temp-buffer
      (insert-file-contents-literally
       (with-temp-buffer
         (insert-file-contents-literally
          (executable-find(concat "load-env-" name)))
         (and (re-search-forward "^source \\(.+\\)$" nil t)
              (match-string 1))))
      (and (or (re-search-forward "^  nativeBuildInputs=\"\\(.+?\\)\"" nil t)
               (re-search-forward "^  buildInputs=\"\\(.+?\\)\"" nil t))
           (split-string(match-string 1)))))

  (when(executable-find "nix-env")
    (mapc  #'(lambda (path)
               (let((share(expand-file-name "share/emacs/site-lisp" path)))
                 (if (file-directory-p share)
                     (add-to-list 'load-path share))))
           (nix-read-environment emacs-environment)))


  (require 'use-package)

  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t)))

;;(require 'bind-key)
(require 'diminish nil t)

;;; Utility macros and functions

;; (defsubst hook-into-modes (func &rest modes)
;;   (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun get-jobhours-string ()
  (with-current-buffer (get-buffer "*scratch*")
    (let ((str (shell-command-to-string "jobhours")))
      (require 'ansi-color)
      (ansi-color-apply (substring str 0 (1 - (length str)))))))

;;; Load customization settings

(defvar running-alternate-emacs nil)
(defvar running-development-emacs nil)


(defvar user-data-directory (emacs-path "data"))
(load (expand-file-name "settings" user-emacs-directory))

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

(use-package alert         :defer  t  :load-path "lisp/alert")
(use-package anaphora      :defer  t  :load-path "lib/anaphora")
(use-package apiwrap       :defer  t  :load-path "lib/apiwrap")
(use-package asoc          :defer  t  :load-path "lib/asoc")
(use-package async         :defer  t  :load-path "lisp/emacs-async")
(use-package button-lock   :defer  t  :load-path "lib/button-lock")
(use-package crux          :demand t  :load-path "lib/crux")
(use-package ctable        :defer  t  :load-path "lib/emacs-ctable")
(use-package dash          :defer  t  :load-path "lib/dash-el")
(use-package deferred      :defer  t  :load-path "lib/emacs-deferred")
(use-package difflib       :defer  t  :load-path "lib/difflib")
(use-package diminish      :demand t  :load-path "lib/diminish")
(use-package el-mock       :defer  t  :load-path "lib")
(use-package elisp-refs    :defer  t  :load-path "lib/elisp-refs")
(use-package emojify       :defer  t  :load-path "lib/emacs-emojify")
(use-package epc           :defer  t  :load-path "lib/emacs-epc")
(use-package epl           :defer  t  :load-path "lib/epl")
(use-package esxml         :defer  t  :load-path "lib/esxml")
(use-package f             :defer  t  :load-path "lib/f-el")
(use-package fn            :defer  t  :load-path "lib/fn-el")
(use-package fringe-helper :defer  t  :load-path "lib/fringe-helper-el")
(use-package fuzzy         :defer  t  :load-path "lib/fuzzy-el")
(use-package gh            :defer  t  :load-path "lib/gh-el")
(use-package ghub          :defer  t  :load-path "lib/ghub")
(use-package ghub+         :defer  t  :load-path "lib/ghub-plus")
(use-package ht            :defer  t  :load-path "lib/ht-el")
(use-package jedi-core     :defer  t  :load-path "site-lisp/emacs-jedi")
(use-package kv            :defer  t  :load-path "lib/kv")
(use-package list-utils    :defer  t  :load-path "lib/list-utils")
(use-package logito        :defer  t  :load-path "lib/logito")
(use-package loop          :defer  t  :load-path "lib/loop")
(use-package m-buffer      :defer  t  :load-path "lib/m-buffer")
(use-package makey         :defer  t  :load-path "lib/makey")
(use-package marshal       :defer  t  :load-path "lib/marshal-el")
(use-package names         :defer  t  :load-path "lib/names")
(use-package noflet        :defer  t  :load-path "lib/noflet")
(use-package oauth2        :defer  t  :load-path "lib/oauth2")
(use-package ov            :defer  t  :load-path "lib/ov-el")
(use-package parent-mode   :defer  t  :load-path "lib/parent-mode")
(use-package parsebib      :defer  t  :load-path "lib/parsebib")
(use-package parsec        :defer  t  :load-path "lib/parsec")
(use-package pcache        :defer  t  :load-path "lib/pcache")
(use-package peval         :defer  t  :load-path "lib/peval")
(use-package pfuture       :defer  t  :load-path "lib/pfuture")
(use-package pkg-info      :defer  t  :load-path "lib/pkg-info")
(use-package popup         :defer  t  :load-path "lib/popup-el")
(use-package popup-pos-tip :defer  t  :load-path "lib")
(use-package popwin        :defer  t  :load-path "lib/popwin")
(use-package pos-tip       :defer  t  :load-path "lib")
(use-package pythonic      :defer  t  :load-path "site-lisp/pythonic")
(use-package request       :defer  t  :load-path "lib/emacs-request")
(use-package rich-minority :defer  t  :load-path "lib/rich-minority")
(use-package s             :defer  t  :load-path "lib/s-el")
(use-package spinner       :defer  t  :load-path "lib/spinner")
(use-package tablist       :defer  t  :load-path "lib/tablist")
(use-package uuidgen       :defer  t  :load-path "lib/uuidgen-el")
(use-package web           :defer  t  :load-path "lib/emacs-web")
(use-package web-server    :defer  t  :load-path "lib/emacs-web-server")
(use-package websocket     :defer  t  :load-path "lib/emacs-websocket")
(use-package with-editor   :defer  t  :load-path "lib/with-editor")
(use-package xml-rpc       :defer  t  :load-path "lib")
(use-package zoutline      :defer  t  :load-path "lib/zoutline")

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
  :defer
  :load-path "site-lisp/ggtags"
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package cc-mode
  :disabled t
  :load-path "override/cc-mode"
  :mode (("\\                                  . h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\                                  . m\\'"                   . c-mode)
         ("\\                         . mm\\'" . c++-mode))
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
        (insert (format "std::cerr << \"step %d . .\" << std::endl;\n"
                        (setq printf-index (1+ printf-index))))
      (insert (format "printf(\"step %d         . .\\n\");\n"
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
  :defer 10
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
  :bind* ("<C-return>" . ace-window))

(use-package ag
  :load-path "site-lisp/ag"
  :commands ag
  :config
  (setq ag-reuse-window 't))

(use-package agda2-mode
  :disabled t
  :mode "\\ . agda\\'"
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
  :defer 5
  :load-path "site-lisp/aggressive-indent-mode"
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))

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
  :defer 30
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
  :defer 10
  :load-path "site-lisp/avy"
  :bind ("M-h" . avy-goto-char)
  :config
  (avy-setup-default))

(use-package backup-each-save
  :commands backup-each-save
  :preface
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (expand-file-name (file-truename file))))

  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  :hook after-save
  :config
  (setq backup-each-save-filter-function 'backup-each-save-filter
        backup-enable-predicate 'my-dont-backup-files-p))

(use-package browse-kill-ring
  :commands browse-kill-ring)

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
  :mode (("CMakeLists . txt" . cmake-mode)
         ("\\         . cmake\\'"    . cmake-mode)))

(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o"   . isearch-moccur)
         ("M-O"   . isearch-moccur-all)))

(use-package company
  :load-path "site-lisp/company-mode"
  :defer 5
  :diminish
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  c-mode-common-hook
                  python-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-idle-delay 0.1)
  (setq company-auto-complete nil)
  (setq company-show-numbers t)
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1))))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  ;; (define-key company-mode-map [tab]
  ;;   '(menu-item "maybe-company-expand" nil
  ;;               :filter (lambda (&optional _)
  ;;                         (when (check-expansion)
  ;;                           #'company-complete-common))))

  (eval-after-load "coq"
    '(progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends))))

  (global-company-mode 1))

(use-package company-web
  :after (company web-mode)
  :load-path "site-lisp/company-web"
  :config
  (use-package web-completion-data
    :after (company web-mode)
    :load-path "site-lisp/web-completion-data" )

  (use-package company-web-html
    :after (company web-mode)
    :load-path "site-lisp/company-web")

  (use-package company-web-slim
    :after (company web-mode)
    :load-path "site-lisp/company-web")

  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))

(use-package company-php
  :after (company web-mode php-mode)
  :load-path "site-lisp/ac-php"
  :config
  (ac-php-core-eldoc-setup)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ac-php-backend))

(use-package company-jedi
  :after (company python)
  :load-path "site-lisp/emacs-company-jedi"
  :config
  (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-jedi)

  (dolist (hook '(python-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "M-p")
                                 #'company-jedi)))))

(use-package company-tern
  :after (company js2-mode tern)
  :load-path "site-lisp/company-tern"
  :config
  (setq company-tern-property-marker nil)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-tern)
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package company-math
  :after (company tex-site)
  :load-path "site-lisp/company-math"
  :preface
  (use-package math-symbol-lists
    :load-path "site-lisp/math-symbol-lists"
    :defer t)
  :config
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package company-auctex
  :disabled t
  :after (company tex-site)
  :load-path "site-lisp/company-auctex"
  :config
  (company-auctex-init))

(use-package company-quickhelp
  :after (company)
  :load-path "site-lisp/company-quickhelp"
  :config
  (company-quickhelp-mode))

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
  :mode "\\ . use\\'")

(use-package cursor-chg
  :defer 5
  :config
  (setq curchg-default-cursor-color 'Purple)
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

(use-package dbwebb
  :load-path "site-lisp/dbwebb"
  :bind (("C-c d b v" . dbwebb-validate)
         ("C-c d b i" . dbwebb-inspect)
         ("C-c d b u" . dbwebb-update)
         ))

(use-package debbugs-gnu
  :disabled t
  :load-path "elpa/packages/debbugs"
  :commands (debbugs-gnu debbugs-gnu-search))

(use-package dedicated
  :disabled t
  :bind ("C- . D" . dedicated-mode))

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
      (if (string-match "^\\( . +?\\)-[0-9._-]+$" pat)
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
                                nil nil "Documents/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Downloads/")))
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

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  (use-package dired-x)
  (use-package dired+
    :disabled t
    :config
    (unbind-key "M-s f" dired-mode-map))


  (use-package dired-ranger
    :bind (:map dired-mode-map ("W" . dired-ranger-copy)
                ("X" . dired-ranger-move)
                ("Y" . dired-ranger-paste)))

  (use-package dired-toggle
    :load-path "site-lisp/dired-toggle"
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
    "Make sure to remove \"Omit\" from the modeline . "
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory . "
    ad-do-it
    (while (and  (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory . "
    ad-do-it
    (while (and  (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  )
(defun dired-omit-regexp ()
  (let ((file (expand-file-name "                              . git"))
        parent-dir)
    (while (and (not (file-exists-p file))
                (progn
                  (setq parent-dir
                        (file-name-directory
                         (directory-file-name
                          (file-name-directory file))))
                  ;; Give up if we are already at the root dir .
                  (not (string= (file-name-directory file)
                                parent-dir))))
      ;; Move up to the parent dir and try again               .
      (setq file (expand-file-name "                           . git" parent-dir)))
    ;; If we found a change log in a parent, use that          .
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
      (funcall dired-omit-regexp-orig))))

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
  :mode (" . *Dockerfile.*" . dockerfile-mode)
  :load-path "site-lisp/dockerfile-mode/")

(use-package dot-gnus
  :disabled t
  :load-path ("override/gnus/lisp" "override/gnus/contrib")
  :bind (("M-G" . switch-to-gnus)
         ("C-x m" . compose-mail))
  :init
  (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
        gnus-home-directory "~/Messages/Gnus/"))

(use-package dot-org
  :defer 20
  :load-path ("site-lisp/org-mode/contrib/lisp"
              "site-lisp/org-mode/lisp")
  :mode (("\\. org\\'" . org-mode)
         ("\\. txt\\'" . org-mode))
  :commands my-org-startupk
  :bind (("M-C"   . jump-to-org-agenda)
         ("M-m"   . org-smart-capture)
         ("M-M"   . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read))
  )


(use-package doxymacs
  :disabled t
  :load-path "site-lisp/doxymacs/lisp/")

(use-package edebug
  :defer t
  :preface
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented . ")

  (defconst modi/fns-regexp
    (concat "(\\s-*"
            "\\(defun\\|defmacro\\)\\s-+"
            "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
    "Regexp to find defun or defmacro definition . ")

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
             :prefix "C- . ="
             ("b"        . ediff-buffers)
             ("B"        . ediff-buffers3)
             ("c"        . compare-windows)
             ("="        . ediff-files)
             ("f"        . ediff-files)
             ("F"        . ediff-files3)
             ("r"        . ediff-revision)
             ("p"        . ediff-patch-file)
             ("P"        . ediff-patch-buffer)
             ("l"        . ediff-regions-linewise)
             ("w"        . ediff-regions-wordwise)))

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
    "Keymap used in isearch in Eshell . ")

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input . "
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
    (exec-path-from-shell-initialize)))

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
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  ;; :bind (("M-n" . flycheck-next-error)
  ;;        ("M-p" . flycheck-previous-error))
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))

(use-package flyspell
  :bind (("C-c i b"   . flyspell-buffer)
         ("C-c i f"   . flyspell-mode))
  :init
  (use-package ispell
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)
           ("C-c i w" . ispell-word)))
  :config
  (unbind-key "C-. " flyspell-mode-map))

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
  :bind ("C- . G" . git-link)
  :commands (git-link git-link-commit git-link-homepage)
  :load-path "site-lisp/git-link")

(use-package git-timemachine
  :disabled t
  :load-path "site-lisp/git-timemachine"
  :commands git-timemachine)

(use-package graphviz-dot-mode
  :disabled t
  :mode "\\ . dot\\'")

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s n" . find-name-dired)
         ("M-s f" . find-grep)
         ("M-s G" . grep))
  :config
  (grep-apply-setting 'grep-command "grep -nH "))

(use-package gud
  :disabled t
  :commands gud-gdb
  :bind ("C- . g" . show-debugger)
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
  :mode (("\\ . hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\ . lhs\\'" . literate-haskell-mode)
         ("\\ . cabal\\'" . haskell-cabal-mode))
  :init
  (setenv "PATH" (concat "~/Library/Haskell/bin:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "~/Library/Haskell/bin")

  :preface
  (defvar interactive-haskell-mode-map)
  (defun snippet (name)
    (interactive "sName: ")
    (find-file (expand-file-name (concat name " . hs") "~/src/notes"))
    (haskell-mode)
    (goto-char (point-min))
    (when (eobp)
      (insert "hdr")
      (yas-expand)))

  (defvar hoogle-server-process nil)
  (defun my-haskell-hoogle (query &optional arg)
    "Do a Hoogle search for QUERY                             . "
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
      (message "Starting local Hoogle server on port 8687 .   . .")
      (with-current-buffer (get-buffer-create " *hoogle-web*")
        (cd temporary-file-directory)
        (setq hoogle-server-process
              (start-process "hoogle-web" (current-buffer) "hoogle"
                             "server" "--local" "--port=8687")))
      (message "Starting local Hoogle server on port 8687     . ..done"))
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
                            (regexp       . ,(cdr x))
                            (modes quote (haskell-mode literate-haskell-mode))))
              '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
                (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+"))))))

(use-package helm
  :load-path "site-lisp/helm"
  :defer 5
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("C-i" . helm-execute-persistent-action)
              ("C-z" . helm-select-action)
              ("A-v"   . helm-previous-page))
  :config
  (use-package helm-config
    :bind (("C-h v" . helm-apropos)
           ("C-h f" . helm-apropos))
    :config
    (helm-autoresize-mode 1)))


(use-package helm-ag
  :after helm
  :load-path "site-lisp/helm-ag"
  :commands (helm-ag helm-ag-this-file))

(use-package helm-dash
  :after helm
  :load-path "site-lisp/helm-dash"
  :commands helm-dash)

(use-package helm-descbinds
  :load-path "site-lisp/helm-descbinds"
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-describe-modes
  :load-path "site-lisp/helm-describe-modes"
  :after helm
  :bind ("C-h m" . helm-describe-modes))

(use-package helm-hoogle
  :load-path "lisp/helm-hoogle"
  :after helm
  :commands helm-hoogle
  ;;:
  :config
  (add-hook
   'helm-c-hoogle-transform-hook
   #'(lambda ()
       (goto-char (point-min))
       (while (re-search-forward "file:///nix/store" nil t)
         (replace-match "http://127 . 0.0.1:8687/file//nix/store" t t)))))


(use-package helm-navi
  :disabled t
  :load-path "site-lisp/helm-navi"
  :after (helm navi)
  :commands helm-navi)

(use-package helm-swoop
  :load-path "site-lisp/helm-swoop"
  :bind (("M-s M-s" . helm-swoop )
         ("M-s S"   . helm-swoop-without-pre-input)
         ("M-s s"   . helm-swoop-from-isearch)))

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
  :disabled t
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
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window))
  :config
  (ido-mode 'buffer)
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-max-work-file-list 100))

(use-package ido-grid-mode
  :disabled t
  :after ido
  :load-path "site-lisp/ido-grid-mode"
  :config
  (ido-grid-mode 1)
  (setq ido-grid-mode-max-rows 10)
  (set-face-attribute 'ido-grid-mode-match nil
                      :background "black"
                      :foreground "white"))

(use-package ido-hacks
  :disabled t
  :after ido
  :demand t
  :load-path "site-lisp/ido-hacks"
  ;;  :bind ("M-x" . my-ido-hacks-execute-extended-command)
  :bind ("M-x" . ido-hacks-execute-extended-command)
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
      (ido-hacks-execute-extended-command arg)))

  (use-package flx-ido
    :load-path "site-lisp/flx"
    :config
    (flx-ido-mode 1))

  (add-hook 'ido-minibuffer-setup-hook
            #'(lambda ()
                (bind-key "<return>" #'ido-smart-select-text
                          ido-file-completion-map))))

(use-package iedit
  :disabled t
  :load-path "site-lisp/iedit")

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
  :disabled t
  :load-path "site-lisp/swiper"
  :defer 5
  ;; :diminish ivy-mode
  ;; :bind (
  ;;        ("C-x b"   . ivy-switch-buffer)
  ;;        ("C-x B"   . ivy-switch-buffer-other-window)
  ;;        ("M-H"     . ivy-resume))
  ;; :bind (:map ivy-minibuffer-map
  ;;             ("C-r" . ivy-previous-line-or-history)
  ;;             ("M-r" . ivy-reverse-i-search))
  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
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
  :disabled t
  :after ivy
  :demand t
  :commands (counsel-M-x)
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

         ("M-s f" . counsel-file-jump)
         ("M-s j" . counsel-dired-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivys--sort-files-by-date)))

(use-package js2-mode
  :load-path "site-lisp/js2-mode"
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  ;; (setq flycheck-disabled-checkers
  ;;       (append flycheck-disabled-checkers
  ;;               '(javascript-jshint)))
  (defvar js2-mode-initialized nil)

  (defun my-js2-mode-hook ()
    (unless js2-mode-initialized
      (setq js2-mode-initialized t))
    (setq indent-tabs-mode nil)
    (tern-mode 1)
    (flycheck-mode 1)
    (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (prettify-symbols-mode)
    (company-mode 1)
    (smartparens-mode 1)
    (whitespace-mode 1))

  (add-hook 'js2-mode-hook #'my-js2-mode-hook)


  (bind-key "M-n" #'flycheck-next-error js2-mode-map)
  (bind-key "M-p" #'flycheck-previous-error js2-mode-map)

  (use-package js2-refactor
    :load-path "site-lisp/js2-refactor"
    :diminish
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode))

  (use-package xref-js2
    :load-path "site-lisp/xref-js2"
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (add-hook 'js2-mode-hook (lambda ()
                               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

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

(use-package hardcore-mode
  :defer 5
  :load-path "site-lisp/hardcore-mode"
  :diminish (hardcore-mode)
  :preface
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package highlight-cl
  :hook (emacs-lisp-mode . highlight-cl-add-font-lock-keywords))

(use-package info-lookmore
  :load-path "site-lisp/info-lookmore"
  :after info-look
  :config
  (info-lookmore-elisp-cl)
  (info-lookmore-elisp-userlast)
  (info-lookmore-elisp-gnus)
  (info-lookmore-apropos-elisp))

(use-package langtool
  :load-path "site-lisp/langtool"
  :commands (langtool-check)
  :preface
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.3/libexec/languagetool-commandline.jar"))

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

(use-package loccur
  :load-path "site-lisp/loccur"
  :defer 10
  :commands loccur)

(use-package lua-mode
  :disabled t
  :load-path "site-lisp/lua-mode"
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

(use-package lusty-explorer
  :defer 5
  :load-path "site-lisp/lusty-emacs"
  :bind (("C-x C-f" . my-lusty-file-explorer)
         ;; ("C-x b" . lusty-buffer-explorer)
         )
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

(use-package magit-popup
  :load-path "site-lisp/magit-popup"
  :defer t)

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

(use-package mc-extras
  :after multiple-cursors
  :load-path "site-lisp/mc-extras"
  :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
         ("<C-m> M-C-b" . mc/mark-previous-sexps)
         ("<C-m> <"     . mc/mark-all-above)
         ("<C-m> >"     . mc/mark-all-below)
         ("<C-m> C-d"   . mc/remove-current-cursor)
         ("<C-m> C-k"   . mc/remove-cursors-at-eol)
         ("<C-m> M-d"   . mc/remove-duplicated-cursors)
         ("<C-m> |"     . mc/move-to-column)
         ("<C-m> ~"     . mc/compare-chars)))

(use-package multiple-cursors
  :load-path "site-lisp/multiple-cursors"
  :after phi-search
  :defer 5

  ;; - Sometimes you end up with cursors outside of your view. You can scroll
  ;;   the screen to center on each cursor with `C-v` and `M-v`.
  ;;
  ;; - If you get out of multiple-cursors-mode and yank - it will yank only
  ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
  ;;   cursor use yank-rectangle, normally found at C-x r y.

  :bind (("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region)
         ("<C-m> s"     . set-rectangular-region-anchor)
         ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-x"   . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align)
         ("<C-m> >"     . mc/skip-to-next-like-this)
         ("<C-m> <"     . mc/skip-to-previous-like-this)

         ("S-<down-mouse-1>")
         ("S-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this))

  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark)))

(use-package nyan-mode
  :disabled t
  :load-path "site-lisp/nyan-mode"
  :config
  (nyan-mode 1))

(use-package phi-search
  :load-path "site-lisp/phi-search"
  :defer 5)

(use-package phi-search-mc
  :after (phi-search multiple-cursors)
  :load-path "site-lisp/phi-search-mc"
  :config
  (phi-search-mc/setup-keys)
  (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))

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
    :after paredit))

(use-package mic-paren
  :defer 5
  :config
  (paren-activate))

(use-package php-mode
  :defer 10
  :load-path "site-lisp/php-mode"
  :config
  (use-package php-ext
    :load-path "site-lisp/php-mode/skeleton")

  (add-hook 'php-mode-hook 'smartparens-mode)
  (add-hook 'php-mode-hook 'flycheck-mode)
  (add-hook 'php-mode-hook 'whitespace-mode)
  (add-hook 'php-mode-hook 'company-mode)

  ;;(add-hook 'auto-save-hook 'indent-buffer)
  ;;(add-hook 'before-save-hook 'indent-buffer)

  ;;(add-hook 'auto-save-hook 'whitespace-cleanup)
  ;;(add-hook 'before-save-hook 'whitespace-cleanup)
  (setq indent-tabs-mode nil)
  (setq tab-width 0)

  (eval-after-load 'flycheck
    '(flycheck-add-mode 'php-phpcs 'php-mode))

  (eval-after-load 'flycheck
    '(flycheck-add-mode 'php-phpmd 'php-mode)))

(use-package personal
  :after crux
  :preface
  ;; hfn (2018-09-03):
  ;; Move these in settings.el
  (setq disabled-command-function nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-unset-key (kbd "<C-down-mouse-1>"))
  (setq ns-right-alternate-modifier nil)
  :config
  (define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))
  ;; (prefer-coding-system 'utf-8)
  ;; (set-default-coding-systems 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (set-selection-coding-system 'utf-8)
  ;; (set-file-name-coding-system 'utf-8)
  ;; (set-clipboard-coding-system 'utf-8)
  ;; (set-buffer-file-coding-system 'utf-8)

  (bind-keys ("C-z"             . delete-other-windows)
             ("C-*"             . goto-matching-parens)

             ("M-!"             . async-shell-command)
             ("M-'"             . insert-pair)
             ("M-\""            . insert-pair)
             ("M-`"             . other-frame)
             ("M-j"             . delete-indentation-forward)
             ("M-J"             . delete-indentation)
             ("M-D" . dired)
             ("M-L"             . mark-line)
             ("M-<return>"      . open-line-above)
             ("C-,"             . pop-global-mark)

             ("M-g c"           . goto-char)

             ("<C-M-backspace>" . backward-kill-sexp)
             ("C-M-<return>"    . emacs-fullscreen)

             ("C-h v"           . describe-variable)

             ("C-x +"           . enlarge-window)
             ("C-x -"           . shrink-window)
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
             ("C-c r"           . replace-regexp)
             ("C-c s"           . replace-string)
             ("C-c u"           . rename-uniquely)
             ("C-c V"           . view-clipboard)
             ("C-c )"           . close-all-parentheses)
             ("C-c ="           . count-matches)
             ("C-;"             . comment-line)
             ("C-c ;"           . comment-or-uncomment-region)
             ("C-c C-z"         . delete-to-end-of-buffer)
             ("C-c C-0"         . copy-current-buffer-name)
             ("C-c M-q"         . unfill-paragraph))

  (bind-keys ("C-h e a"         . apropos-value)
             ("C-h e e"         . view-echo-area-messages)
             ("C-h e f"         . find-function)
             ("C-h e k"         . find-function-on-key)
             ("C-h e v"         . find-variable))

  (bind-keys ("C-c e E"         . elint-current-buffer)
             ("C-c e b"         . do-eval-buffer)
             ("C-c e c"         . cancel-debug-on-entry)
             ("C-c e d"         . debug-on-entry)
             ("C-c e e"         . toggle-debug-on-error)
             ("C-c e f"         . emacs-lisp-byte-compile-and-load)
             ("C-c e I"         . crux-find-user-init-file)
             ("C-c e j"         . emacs-lisp-mode)
             ("C-c e P"         . check-papers)
             ("C-c e r"         . do-eval-region)
             ("C-c e s"         . scratch)
             ("C-c e p"         . python-mode)
             ("C-c e z"         . byte-recompile-directory))

  (bind-keys ("S-<return>" . open-line-below)))

(use-package projectile
  :load-path "site-lisp/projectile"
  :defer 30
  :bind-keymap ("C-c p" . projectile-command-map)
  :diminish
  :config
  (use-package helm-projectile
    :load-path "site-lisp/helm-projectile"
    :config
    (helm-projectile-on)
    )
  (setq projectile-indexing-method 'native)
  (setq projectile-completion-method 'helm)
  (projectile-global-mode 1))

(use-package processing-mode
  :disabled t
  ;; hfn (2018-09-29): fix processing-location
  :load-path "site-lisp/processing2-emacs"
  :config
  (setq processing-location "/usr/local/bin/processing-java")
  (setq processing-application-dir "/Applications/Processing.app")
  (setq processing-sketchbook-dir "~/Documents/Processing"))

(use-package py-autopep8
  :load-path "site-lisp/py-autopep8"
  :after python
  :bind ("C-c P" . python-autopep8)
  :preface
  (defcustom python-autopep8-path (executable-find "autopep8")
    "autopep8 executable path."
    :group 'python
    :type 'string)
  :config
  (defun python-autopep8 ()
    (interactive)
    (interactive)
    (when (eq major-mode 'python-mode)
      (shell-command
       (format "%s --in-place --aggressive --aggressive %s" python-autopep8-path
               (shell-quote-argument (buffer-file-name))))
      (revert-buffer t t t))))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :bind (:map inferior-python-mode-map
              ("M-k" . comint-clear-buffer))
  :preface
  (defface paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses.")

  (defvar python-prettify-symbols-alist
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
      ;; ("not"    . ?¬)
      ("&&"     . ?∧)
      ("and"    . ?∧)
      ("||"     . ?∨)
      ("or"     . ?∨)
      ("!="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)
      ("lambda" . ?λ)
      ("sqrt"   . ?√)
      ("pi"     . ?π)
      ("sum"    . ?∑)
      ;; ("int" .      #x2124)
      ;; ("float" .    #x211d)
      ;; ("str" .      #x1d54a)
      ;; ("True" .     #x1d54b)
      ;; ("False" .    #x1d53d)

      ("`in`"             . ?∈)
      ("`not in`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)))

  (defun wrap-print ()
    (interactive)
    (wrap-region-with-fun "print"))
  :config
  (bind-key "M-P"  #'wrap-region-with-fun)
  (bind-key "M-p"  #'wrap-print)
  (setq-local prettify-symbols-alist python-prettify-symbols-alist)

  ;;   (defvar universal-coding-system-env-list '("PYTHONIOENCODING")
  ;;     "List of environment variables \\[universal-coding-system-argument] should set")

  ;;   (defadvice universal-coding-system-argument (around provide-env-handler activate)
  ;;     "Augments \\[universal-coding-system-argument] so it also sets environment variables

  ;; Naively sets all environment variables specified in
  ;; `universal-coding-system-env-list' to the literal string
  ;; representation of the argument `coding-system'.

  ;; No guarantees are made that the environment variables set by this advice support
  ;; the same coding systems as Emacs."
  ;;     (let ((process-environment (copy-alist process-environment)))
  ;;       (dolist (extra-env universal-coding-system-env-list)
  ;;         (setenv extra-env (symbol-name (ad-get-arg 0))))
  ;;       ad-do-it))


  (setq python-python-command "/usr/local/bin/python3")
  ;;(setq python-shell-interpreter "python3")
  (setq py-python-command "python3")


  (defvar python-mode-initialized nil)

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
                  (match-string 1 item))))))))

  (setq indicate-empty-lines nil)
  (set (make-local-variable 'parens-require-spaces) nil)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)

  (prettify-symbols-mode)
  (company-mode 1)
  (smartparens-mode 1)
  (whitespace-mode 1)
  (setenv "LANG" "UTF-8")
  (bind-key "C-c C-z" #'python-shell python-mode-map)
  (unbind-key "C-c c" python-mode-map)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (set-variable 'py-indent-offset 4)

  ;; (eval-after-load 'flycheck
  ;;   '(flycheck-add-mode 'python-pylint 'python-mode))

  (flycheck-mode 1)


  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)

  (add-hook 'auto-save-hook 'whitespace-cleanup)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  (add-hook 'python-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("(\\|)" . 'paren-face)))
              (font-lock-add-keywords nil
                                      '(("{\\|}" . 'paren-face)))
              (font-lock-add-keywords nil
                                      '(("\\[\\|\\]" . 'paren-face)))
              ))
  )

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

(use-package selected
  :load-path "site-lisp/selected"
  :defer 5
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("["       . align-entire)
              ("f"       . fill-region)
              ("U"       . unfill-region)
              ("d"       . downcase-region)
              ("r"       . reverse-region)
              ("s"       . sort-lines)
              ("C-<tab>" . indent-shift-right)
              ("S-<tab>" . indent-shift-left)
              ("u"       . upcase-region))
  :config
  (selected-global-mode 1))

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

(use-package shackle
  :defer 5
  :load-path "site-lisp/shackle"
  :commands shackle-mode
  :config
  (shackle-mode 1))

(use-package shrink-whitespace
  :load-path "site-lisp/shrink-whitespace"
  :commands shrink-whitespace
  :bind ("C-c SPC" . shrink-whitespace))

(use-package slime
  :disabled t
  :load-path "site-lisp/slime"
  :commands slime
  :init
  (setq inferior-lisp-program "/Users/hfn/.nix-profile/bin/sbcl"
        slime-contribs '(slime-fancy)))

(use-package smart-mode-line
  :disabled t
  :load-path "site-lisp/smart-mode-line"
  :defer 10
  :config
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (sml/setup)
  (sml/apply-theme 'light)
  (add-to-list 'sml/replacer-regexp-list '("^~/src/" ":src:") t)
  (delete '("^~/\\.emacs\\.d/" ":ED:") sml/replacer-regexp-list)
  (add-to-list 'sml/replacer-regexp-list '("^~/dbwebb-kurser/" ":dbwebb:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/" ":dot-emacs:"))
  (add-hook 'display-time-hook 'sml/propertize-time-string))

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

(use-package smex
  :load-path "site-lisp/smex"
  :commands smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file "~/.emacs.d/smex-items")
  (smex-initialize))

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

(use-package string-inflection
  :defer 10
  :load-path "site-lisp/string-inflection"
  :bind ("C-c C-u" . string-inflection-java-style-cycle))

(use-package swap-regions
  :load-path "site-lisp/swap-regions"
  :commands swap-regions)

(use-package swiper
  :disabled t
  :after ivy
  :load-path "site-lisp/swiper"
  ;; :bind ("C-s" . swiper)
  ;; :bind (:map swiper-map
  ;;             ("M-y" . yank)
  ;;             ("M-%" . swiper-query-replace)
  ;;             ("C-." . swiper-avy)
  ;;             ;; ("M-c" . swiper-mc)
  ;;             ("M-c" . haba/swiper-mc-fixed))
  ;; :bind (:map isearch-mode-map
  ;;             ("C-o" . swiper-from-isearch))
  :config
  (defun haba/swiper-mc-fixed ()
    (interactive)
    (setq swiper--current-window-start nil)
    (swiper-mc)))

(use-package tablegen-mode
  :mode ("\\.td\\'" . tablegen-mode))

(use-package tern
  :load-path "lib/tern/emacs"
  :diminish
  :commands (tern-mode)
  :init
  (add-hook 'js2-minor-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (tern-mode-enable))))
  )

(use-package tex-site
  :defer 10
  :load-path "site-lisp/auctex"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . LaTeX-mode)
  ;; :init
  ;; :preface
  :config
  (require 'tex)
  ;;(unbind-key "\\" latex-mode-map)

  (setq reftex-plug-into-AUCTeX t)
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-extra-bindings t)

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
  :defer 10
  :commands vimish-fold
  :bind (("C-c v" . vimish-fold)
         ("C-c x" . vimish-fold-delete)
         ("C-+" . vimish-fold-toggle)
         ("C--" . vimish-fold-unfold))
  :load-path "site-lisp/vimish-fold"
  :config
  (set-face-background 'vimish-fold-overlay "#f8f8ff")
  (set-face-foreground 'vimish-fold-overlay "gray")
  )

(use-package visual-regexp
  :load-path "site-lisp/visual-regexp"
  :commands (vr/replace
             vr/query-replace)
  :bind (("C-. r" . vr/replace)
         ("C-. M-%" . vr/query-replace))
  :config
  (use-package visual-regexp-steroids
    :load-path "site-lisp/visual-regexp-steroids"))

(use-package emmet-mode
  :defer 10
  :load-path "site-lisp/emmet-mode"
  :diminish emmet-mode
  :hook ((web-mode-hook) . emmet-mode)
  :config
  (emmet-mode)
  (unbind-key "C-<return>" emmet-mode-keymap))

(use-package multi-web-mode
  :disabled t
  :defer 5
  :load-path "site-lisp/multi-web-mode"
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'"  . web-mode)
         ("\\.php\\'"  . web-mode))
  :after (php-mode web-mode)
  :diminish
  :config
  (setq mweb-default-major-mode 'web-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

(use-package web-mode
  :defer
  :load-path "site-lisp/web-mode"
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'"  . web-mode)
         ("\\.php\\'"  . web-mode))
  :config
  (add-hook 'web-mode-hook 'smartparens-mode)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'whitespace-mode)
  (add-hook 'web-mode-hook 'company-mode)


  (add-hook 'before-save-hook 'indent-buffer)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  (eval-after-load 'flycheck
    '(flycheck-add-mode 'html-tidy 'web-mode))

  (defvar web-mode-initialized nil)

  (defun my-web-mode-hook ()
    (unless web-mode-initialized
      (setq web-mode-initialized t)
      (setq web-mode-engines-alist '(("php" . "\\.html\\'")))
      (setq indicate-empty-lines t)
      (make-local-variable 'web-mode-code-indent-offset)
      (make-local-variable 'web-mode-markup-indent-offset)
      (make-local-variable 'web-mode-css-indent-offset)
      (setq web-mode-code-indent-offset 4)
      (setq web-mode-css-indent-offset 4)
      (setq web-mode-markup-indent-offset 4)
      (unbind-key "C-c TAB" web-mode-map))
    (add-hook 'web-mode-hook 'my-web-mode-hook)))

(use-package which-key
  :load-path "site-lisp/which-key"
  :diminish which-key-mode
  :defer 10
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2.0))

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

(use-package zoom-window
  :defer 10
  :load-path "site-lisp/emacs-zoom-window"
  :config
  (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
  (custom-set-variables
   '(zoom-window-mode-line-color "Lightgrey")))

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
  :defer 30
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
  :load-path "site-lisp/yasnippet-snippets"
  :after yasnippet)

(use-package whole-line-or-region
  :defer 10
  :diminish (whole-line-or-region-local-mode)
  :config (whole-line-or-region-global-mode 1))

;;; Layout

(defconst display-name
  (pcase (display-pixel-width)
    (`3840 'dell-wide)
    (`2560 'imac)
    (`1920 'macbook-pro-vga)
    (`1680 'macbook-pro)))

(defconst emacs-min-width 100)
(defconst emacs-min-top 50)
(defconst emacs-min-left 550)
(defconst emacs-min-height 67)

(defconst emacs-min-font
  "-*-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"
  ;; "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"
  )

;; (defconst emacs-max-font
;;   "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")


(defconst emacs-max-font
  "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")

(defun emacs-min ()
  (interactive)
  (cl-flet ((set-param (p v) (set-frame-parameter (selected-frame) p v)))
    (set-param 'fullscreen nil)
    (set-param 'vertical-scroll-bars nil)
    (set-param 'horizontal-scroll-bars nil))
  (set-frame-font emacs-min-font)
  (set-frame-position (selected-frame) emacs-min-left emacs-min-top)
  (set-frame-height (selected-frame) emacs-min-height)
  (set-frame-width (selected-frame) emacs-min-width))

(defun emacs-max ()
  (interactive)
  (cl-flet ((set-param (p v) (set-frame-parameter (selected-frame) p v)))
    (set-param 'fullscreen 'fullboth)
    (set-param 'vertical-scroll-bars nil)
    (set-param 'horizontal-scroll-bars nil))
  (set-frame-font emacs-max-font))

(defun emacs-toggle-size ()
  (interactive)
  (if (alist-get 'fullscreen (frame-parameters))
      (emacs-min)
    (emacs-max)))

(add-hook 'emacs-startup-hook #'emacs-min t)


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
