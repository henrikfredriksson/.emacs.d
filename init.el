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
    (add-to-list 'load-path (emacs-path path)))

  )

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
;; (use-package emojify       :defer  t  :load-path "lib/emacs-emojify")
(use-package epc           :defer  t  :load-path "lib/emacs-epc")
(use-package epl           :defer  t  :load-path "lib/epl")
(use-package esxml         :defer  t  :load-path "lib/esxml")
(use-package f             :defer  t  :load-path "lib/f-el")
(use-package fn            :defer  t  :load-path "lib/fn-el")
(use-package fringe-helper :defer  t  :load-path "lib/fringe-helper-el")
;;(use-package fuzzy         :defer  t  :load-path "lib/fuzzy-el")
(use-package gh            :defer  t  :load-path "lib/gh-el")
(use-package ghub          :defer  t  :load-path "lib/ghub")
(use-package ghub+         :defer  t  :load-path "lib/ghub-plus")
(use-package ht            :defer  t  :load-path "lib/ht-el")
;; (use-package jedi-core     :defer  t  :load-path "site-lisp/emacs-jedi")
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
;;(use-package popwin        :defer  t  :load-path "lib/popwin")
(use-package pos-tip       :defer  t  :load-path "lib")
;;(use-package pythonic      :defer  t  :load-path "site-lisp/pythonic")
(use-package request       :defer  t  :load-path "lib/emacs-request")
(use-package s             :defer  t  :load-path "lib/s-el")
(use-package spinner       :defer  t  :load-path "lib/spinner")
(use-package tablist       :defer  t  :load-path "lib/tablist")
(use-package uuidgen       :defer  t  :load-path "lib/uuidgen-el")
(use-package web           :defer  t  :load-path "lib/emacs-web")
(use-package web-server    :defer  t  :load-path "lib/emacs-web-server")
(use-package websocket     :defer  t  :load-path "lib/emacs-websocket")
(use-package with-editor   :defer  t  :load-path "lib/with-editor")
;;(use-package xml-rpc       :defer  t  :load-path "lib")
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
  :defer
  :load-path "site-lisp/ace-window"
  :bind* ("<C-return>" . ace-window))

(use-package ag
  :load-path "site-lisp/ag"
  :commands ag
  :config
  (setq ag-reuse-window 't))

(use-package aggressive-indent
  :defer 20
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

  (global-company-mode 1))

(use-package company-auctex
  :after (company tex-site)
  :load-path "site-lisp/company-auctex"
  :config
  (company-auctex-init))

(use-package company-quickhelp
  :after (company)
  :load-path "site-lisp/company-quickhelp"
  :config
  (company-quickhelp-mode))

(use-package cursor-chg
  :defer 5
  :config
  (setq curchg-default-cursor-color 'Purple)
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1))

(use-package diff-mode
  :commands diff-mode
  :config
  (use-package diff-mode-))

(use-package dired
  :bind (("C-x D" . dired)
         ("C-c j" . dired-two-pane))
  :bind (:map dired-mode-map
              ("j"     . dired)
              ("z"     . pop-window-configuration)
              ("e"     . ora-ediff-files)
              ("l"     . dired-up-directory)
              ("q"     . dired-up-directory)
              ("Y"     . ora-dired-rsync)
              ("M-!"   . async-shell-command)
              ("<tab>" . dired-next-window)
              ("M-G")
              ("M-s f"))
  :hook (dired-mode . dired-hide-details-mode)
  :preface
  (defun dired-two-pane ()
    (interactive)
    (push-window-configuration)
    (let ((here default-directory))
      (delete-other-windows)
      (dired "~/dl")
      (split-window-horizontally)
      (dired here)))

  (defun dired-next-window ()
    (interactive)
    (let ((next (car (cl-remove-if-not #'(lambda (wind)
                                           (with-current-buffer (window-buffer wind)
                                             (eq major-mode 'dired-mode)))
                                       (cdr (window-list))))))
      (when next
        (select-window next))))

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

  (defun ora-dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          (tmtxt/rsync-command "rsync -aP "))
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))

  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      `(lambda ()
                         (setq ediff-after-quit-hook-internal nil)
                         (set-window-configuration ,wnd))))
        (error "no more than 2 files should be marked"))))

  :config
  (setq dired-dwim-target-directory t)
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

(use-package dired-x
  :after dired)

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


(use-package edit-var
  :bind ("C-c e v" . edit-variable))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :load-path "site-lisp/expand-region-el"
  :commands er/expand-region
  :bind (("M-+" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package flycheck
  :load-path "site-lisp/flycheck"
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
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

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s n" . find-name-dired)
         ("M-s f" . find-grep)
         ("M-s G" . grep))
  :config
  (grep-apply-setting 'grep-command "grep -nH "))

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
  :defer 10
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
         (replace-match "http://127.0.0.1:8687/file//nix/store" t t)))))

(use-package hippie-exp
  :bind (("M-/" . dabbrev-expand)
         ("M-?" . hippie-expand))
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

  (bind-key "M-i" #'my-ido-hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package indent
  :commands indent-according-to-mode)

(use-package indent-shift
  :bind (("C-c <" . indent-shift-left)
         ("C-c >" . indent-shift-right))
  :load-path "site-lisp/indent-shift")

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)
  :config
  (defadvice Info-exit (after remove-info-window activate)
    "When info mode is quit, remove the window."
    (if (> (length (window-list)) 1)
        (delete-window))))

(use-package info-look
  :commands info-lookup-add-help)

(use-package isearch
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :bind (:map isearch-mode-map
              ("C-c" . isearch-toggle-case-fold)
              ("C-t" . isearch-toggle-regexp)
              ("C-^" . isearch-edit-string)
              ("C-i" . isearch-complete))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-forward)))

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
           nil t)))))

  (when (eq major-mode 'js2-mode)
    (flycheckmode nil)))

(use-package lispy
  :load-path "site-lisp/lispy"
  :commands lispy-mode
  :bind (:map lispy-mode-map
              ("M-j"))
  :bind (:map emacs-lisp-mode-map
              ("C-1"     . lispy-describe-inline)
              ("C-2"     . lispy-arglist-inline)
              ("C-c C-j" . lispy-goto)))

(use-package lusty-explorer
  :defer 5
  :load-path "site-lisp/lusty-emacs"
  :bind (("C-x C-f" . my-lusty-file-explorer)
         ("C-x b" . lusty-buffer-explorer))
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

(use-package personal
  :after crux
  :preface
  ;; hfn (2018-09-03):
  ;; Move these in settings.el
  ;; (setq disabled-command-function nil)
  ;;(global-unset-key (kbd "<C-down-mouse-1>"))
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

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  (setq python-python-command "/usr/local/bin/python3")
  ;;(setq python-shell-interpreter "python3")
  (setq py-python-command "python3")


  (setq indicate-empty-lines nil)
  (set (make-local-variable 'parens-require-spaces) nil)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)

  (smartparens-mode 1)
  (whitespace-mode 1)
  (setenv "LANG" "UTF-8")
  (bind-key "C-c C-z" #'python-shell python-mode-map)
  (unbind-key "C-c c" python-mode-map)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (set-variable 'py-indent-offset 4)

  (flycheck-mode 1)


  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))


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

(use-package restart-emacs
  :load-path "site-lisp/restart-emacs"
  :commands restart-emacs)

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

(use-package shrink-whitespace
  :load-path "site-lisp/shrink-whitespace"
  :commands shrink-whitespace
  :bind ("C-c SPC" . shrink-whitespace))

(use-package smartparens
  :load-path "site-lisp/smartparens"
  :diminish (smartparens-mode)
  :config
  (bind-keys ("C-<right>"    . sp-forward-slurp-sexp)
             ("C-<left>"     . sp-forward-barf-sexp)
             ("C-M-<left>"   . sp-backward-slurp-sexp)
             ("C-M-<right>"  . sp-backward-barf-sexp)

             ("C-d"          . sp-delete-char)
             ;; ("C-k"          . sp-kill-whole-line)
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
  :bind ("M-x" . smex))

(use-package sort-words
  :load-path "site-lisp/sort-words"
  :commands sort-words)

(use-package tex-site
  :defer 5
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

(use-package visual-regexp
  :load-path "site-lisp/visual-regexp"
  :commands (vr/replace
             vr/query-replace)
  :bind (("C-. r" . vr/replace)
         ("C-. M-%" . vr/query-replace))
  :config
  (use-package visual-regexp-steroids
    :load-path "site-lisp/visual-regexp-steroids"))



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
