;;; -*- lexical-binding: t -*-
;;; init.el --- Emacs configuration.
;;
;; Filename: init.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Thu Oct  2 08:04:34 2014 (-0700)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; See: https://github.com/sandinmyjoints/.emacs.d
;;
;; This file bootstraps initialization.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(eval-when-compile
  (require 'use-package))

;; Are we on a mac?
(defvar is-mac (equal system-type 'darwin))

(setq user-full-name "William Bert")
(setq user-mail-address "william.bert@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require Common Lisp. (cl in <=24.2, cl-lib in >=24.3.)
(if (require 'cl-lib nil t)
  ;; Madness: cl-block-wrapper was an alias for identity in 24.2, then it was
  ;; renamed cl--block-wrapper in 24.3, but somehow my 10.6.8 machine still
  ;; wants cl-block-wrapper when running 24.3 (though my 10.8.3 machine has no
  ;; such problem), so help it out.
  (progn
    (defalias 'cl-block-wrapper 'identity)
    (defalias 'member* 'cl-member)
    (defalias 'adjoin 'cl-adjoin))
  ;; Else we're on an older version so require cl.
  (require 'cl))

;; Use server.
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(when is-mac (require 'setup-mac))

;; ========================================
;; Machine-local custom configuration.
;; ========================================

(load custom-file t t)

;; ========================================
;; Package management.
;; ========================================

(require 'setup-package)

;; From purcell. TODO: replace with use-package.
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; For Emacs Lisp not available as submodule or package (e.g., windata.el).
(use-package auto-install
  :config
  (setq auto-install-directory "~/.emacs.d/elisp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ========================================
;; Require/autoload and config packages.
;; ========================================

(require 'setup-grep)

(require 'sane-defaults)

(require 'defuns)

(when (require 'so-long nil :noerror)
  (so-long-enable))

(require 'vlf-setup)

(use-package flycheck
  :ensure t
  :init
  ;; This turns on Flycheck globally in only these modes. Others can be turned on
  ;; per-buffer.
  (defvar flycheck-global-modes
    '(js2-mode
      js2-jsx-mode
      rjsx-mode
      coffee-mode
      emacs-lisp-mode
      json-mode
      sh-mode
      yaml-mode
      python-mode
      json-mode))
  (global-flycheck-mode)
  :config
  (require 'setup-flycheck))

(require 'wjb)

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package elisp-mode
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 70)))
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header))

(autoload 'auto-make-header "header2")

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize)
  (setq smex-auto-update nil)
  (smex-auto-update 10)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package beacon
  :defer t
  :config
  (setq beacon-blink-duration 0.1)
  (beacon-mode 1))

(use-package ido
  :config
  (require 'setup-ido))

;; ibuffer.
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Imenu.
(when (require 'imenu nil t)
  (autoload 'idomenu "idomenu" nil t))

(defadvice ido-imenu (before push-mark activate)
    (push-mark))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(use-package dired
  :init
  ;; This line must run *before* dired is loaded:
  ;; See http://emacs.stackexchange.com/questions/28016/dired-does-not-respect-listing-dired-listing-switches
  ;; Details toggling is bound to "(" in `dired-mode' by default.
  (setq diredp-hide-details-initially-flag nil)
  :bind (:map dired-mode-map
              ("C-c f" . find-name-dired))
  :config
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (add-hook 'dired-mode-hook  (lambda () (setq auto-revert-verbose nil)))
  ;; bsd ls vs. gls: this is written for bsd, but gls is probably
  ;; better
  ;;
  (setq dired-listing-switches "-laFhp"
        dired-dwim-target t
        dired-recursive-copies 'always )
  (use-package dired+
    :config
    (require 'dired+)
    (toggle-diredp-find-file-reuse-dir 1)
    (add-to-list 'dired-compress-files-alist '("\\.gz\\'" . "gzip -c %i > %o"))
    (setq diredp-hide-details-propagate-flag t)))

(use-package ediff
  :init
  (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
  (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
  (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :config
  (setq diff-switches "-u"
        ediff-diff-options "-w"
        ediff-custom-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package help-mode
  :init
  (add-hook 'help-mode-hook 'visual-line-mode)
  :diminish visual-line-mode
  )

(use-package autorevert
  :diminish auto-revert-mode)

(use-package abbrev
  :init
  (add-hook 'text-mode-hook 'abbrev-mode)
  :diminish abbrev-mode)

;; Org-mode.
;; (require 'org-install)
;; (eval-after-load 'org '(require 'setup-org))
(use-package org ;; why org not org-mode: https://emacs.stackexchange.com/q/17710
  :defer t
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  :diminish visual-line-mode
  :config
  (setq org-src-fontify-natively t)
  (setq org-log-done t)
  (setq org-clock-persist 'history)
  (setq org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil))) ;; can switch back to auto soon
  (setq org-todo-keywords
        '((sequence "TODO" "ACTIVE" "|" "DONE" "INACTIVE")))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido t)
  (setq org-return-follows-link t)
  (require 'setup-org)
  ;; Load ODT backend to allow for exporting to open document format.
  (require 'ox-odt))

;; Magit.
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (autoload 'magit-log "magit"))

;; Experiment, might want to do this for everything:
(use-package setup-magit
  :after magit)

;; Python.
(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (require 'setup-python))

;; Rainbow mode.
(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)
  (add-hook 'conf-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config
  (require 'setup-projectile))

(use-package dumb-jump
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'helm)
  (add-hook 'prog-mode-hook #'dumb-jump-mode))

(require 'setup-tramp)

(require 'mode-mappings)

;; Lua mode.
(use-package lua
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package gitignore-mode
  :mode "global.gitignore")

(use-package json-mode)

;; Must come before js2-mode or coffee-mode so they can set proper nvm
;; for file.
(use-package nvm)

(eval-when-compile (require 'cl))
(defcustom preferred-javascript-mode
  (cl-first (cl-remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-javascript-indent-level 2)

(eval-after-load 'js2-mode '(require 'setup-js2-mode))

(use-package js-comint
  :init
  ;; Fix garbage in prompt: http://stackoverflow.com/questions/13862471
  (setenv "NODE_NO_READLINE" "1")

  (setq js-comint-program-arguments '("--interactive"))

  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
  ;;(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
  ;;(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
  ;;(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)
  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil "" inferior-js-minor-mode-map)
  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode))

  (autoload 'js-comint "js-select-node-version" "Add directory to tree view")
  (autoload 'js-comint "run-js" "Add directory to tree view")

  :config
  (js-do-use-nvm))

(use-package coffee-mode
  :mode "\\.coffee\\.erb\\'"
  :init
  (add-hook 'coffee-mode-hook #'nvm-use-for-buffer)

  (defun my/use-coffeelint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (coffeelint (and root
                            (expand-file-name "node_modules/coffeelint/bin/coffeelint"
                                              root))))
      (when (and coffeelint (file-executable-p coffeelint))
        (setq-local flycheck-coffee-coffeelint-executable coffeelint))))
  (add-hook 'coffee-mode-hook #'my/use-coffeelint-from-node-modules)

  (defun my/use-coffee-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (coffee (and root
                        (expand-file-name "node_modules/.bin/coffee"
                                          root))))
      (when (and coffee (file-executable-p coffee))
        (setq-local flycheck-coffee-executable coffee))))
  (add-hook 'coffee-mode-hook #'my/use-coffee-from-node-modules)

  :config
  (setq coffee-tab-width preferred-javascript-indent-level))

(use-package rainbow-delimiters
  :init
  (add-hook 'json-mode-hook #'rainbow-delimiters-mode))

;; dims parens.
(use-package paren-face
  :config
  (add-to-list 'paren-face-modes 'js2-mode)
  (global-paren-face-mode))

(use-package restclient
  ;; (local-set-key (kbd "C-c C-c") 'restclient-http-send-current)
  ;; (local-set-key (kbd "C-c C-v") 'restclient-http-send-current-stay-in-window)
  :bind (:map restclient-mode-map
              ("C-c C-c" . 'restclient-http-send-current-stay-in-window)
              ("C-c C-v" . 'restclient-http-send-current))
  :config
  (defadvice restclient-http-handle-response (around my-compile-goto-error activate)
    (let ((display-buffer-overriding-action '(display-buffer-reuse-window (inhibit-same-window . nil))))
      ad-do-it)))

;; multiple-cursors.
;;
;; See: https://github.com/magnars/multiple-cursors.el
;;
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-x t") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-M-<") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-M->") 'mc/edit-ends-of-lines))

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; (require 'lisp-stuff)

;; EPG.
(use-package epa-file
  :defer t
  :config
  (epa-file-enable))

(use-package gitignore-mode
  :mode "\\.dockerignore\\'")

(use-package dockerfile-mode
  :mode "Dockerfile-*")

(use-package docker-compose-mode)

(use-package docker-tramp)

(use-package conf-mode
  :mode "credentials$"
  "pylintrc"
  "ads.txt"
  ".htaccess")

;; Yasnippet.
;; TODO: get this to work with use-package, it doesn't like it.
(when (require 'yasnippet nil t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/yasnippet-coffee-script-snippets/" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/js-snippets" t)
  (yas-global-mode 1)

;; Work-around for tab complaining when yas is active in ansi-term. See:
;; https://github.com/capitaomorte/yasnippet/issues/289
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1))))

(eval-after-load 'yasnippet '(diminish 'yas-minor-mode))

;; RVM.
(use-package rvm
  :ensure t
  :defer t
  :config
  (rvm-use-default)) ;; use rvm's default ruby for the current Emacs session

(use-package beginend
  :ensure t
  :defer t
  :config
  (beginend-global-mode))

(use-package dotenv-mode
  :mode "\\.env\\'")

(use-package coffee-mode
  :ensure t
  :defer t
  :config
  (require 'setup-coffee))

(use-package discover
  :ensure t
  :defer t
  :config
  (global-discover-mode 1))

(use-package know-your-http-well
  :ensure t
  :defer t)

(use-package sane-term
  :commands (sane-term sane-term-create)
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create))
  :ensure t
  :defer t)

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (require 'setup-modeline))

(use-package web-mode
  :mode "\\.html?\\'"
  "\\.hbs\\'"
  "\\.ejs\\'"
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-enable-current-element-highlight t)
  (require 'setup-webmode))

(require 'setup-markdown)
(require 'setup-company)

(require 'key-bindings)

(use-package shell-script-mode
  :mode "\\.bash*")

;; TODO: need to spend time tweaking this for it to be really helpful.
(use-package smart-jump
  :ensure t
  :disabled
  :config
  (smart-jump-setup-default-registers))

(use-package atomic-chrome
  :disabled
  :config
  (atomic-chrome-start-server))

(use-package google-this
  ;; C-c / n|SPC|l
  :diminish google-this-mode
  :config
  (google-this-mode 1))

(use-package vimish-fold
  :config
  (vimish-fold-global-mode 0)
  ;; TODO: make this only true in vimish-fold key map
  (global-set-key (kbd "C-c `") #'vimish-fold-toggle)
  (global-set-key (kbd "C-c ~") #'vimish-fold))

(use-package yafolding
  :config
  (add-hook 'prog-mode-hook #'yafolding-mode))

(use-package wgrep
  :config
  (setq wgrep-enable-key "w"))

(use-package sql
  :config
  (add-to-list 'sql-mysql-login-params '(port :default 3311)))

(use-package helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

;; Fill column indicator.
;; See: https://github.com/alpaker/Fill-Column-Indicator
;;
(when (require 'fill-column-indicator nil t)
  (setq fci-rule-color "#555")

  ;; Turn on fci for these modes:
  (dolist (hook '(prog-mode-hook yaml-mode-hook))
    (add-hook hook 'fci-mode))

  ;; ...except for these modes.
  (defun turn-off-fci ()
    (fci-mode -1))

  (dolist (hook '(web-mode-hook lsp-ui-mode-hook))
    (add-hook hook 'turn-off-fci))

  ;; fci-mode doesn't play well with popups
  (defun on-off-fci-before-company (command)
    (when (and (bound-and-true-p fci-mode) (string= "show" command))
      (set (make-local-variable 'wjb/fci-mode-was-on) t)
      (turn-off-fci-mode))
    (when (and (bound-and-true-p wjb/fci-mode-was-on) (string= "hide" command))
      (turn-on-fci-mode)))

  (advice-add 'company-call-frontends
              :before #'on-off-fci-before-company))

(use-package lsp-mode)

(use-package lsp-ui
  :config (setq lsp-ui-flycheck-enable 0
                lsp-ui-peek-enable 0
                lsp-ui-sideline-enable 1
                lsp-ui-doc-enable 0
                lsp-ui-imenu-enable 1)

  ;; Disabling this for now because the default ui stuff is kind of
  ;; annoying. TODO: only select the UI things I want.
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  ;; How to turn on sideline mode.
  ;; (lsp-ui-sideline-enable)
  )

;; Smart-tab. See: https://raw.github.com/genehack/smart-tab/master/smart-tab.el
;; and https://www.emacswiki.org/emacs/TabCompletion#SmartTab
(use-package smart-tab
  :disabled
  :diminish smart-tab-mode
  :config
  (global-smart-tab-mode))

(use-package smartparens-mode
  :disabled
  :config
  (require 'setup-smartparens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ctags.
(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R --exclude=node_modules --exclude=local_notes --exclude=test --exclude=lib-cov %s" path-to-ctags (directory-file-name dir-name))))

;; Linum: put spaces around line numbers.
;; TODO: Not using linum anymore. nlinum is maybe better.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; Keep region active when hit C-g. From http://emacs.stackexchange.com/a/11064
(defun my-keyboard-quit-advice (fn &rest args)
  (let ((region-was-active (region-active-p)))
    (unwind-protect
         (apply fn args)
      (when region-was-active
        (activate-mark t)))))

(advice-add 'keyboard-quit :around #'my-keyboard-quit-advice)

;; from http://rawsyntax.com/blog/learn-emacs-use-defadvice-modify-functions/
;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; Paired tick is useful in some modes.
;; TODO: Probably Can't run these until the mode has been loaded or something.
;; TODO: Could use smartparens for this instead.
(modify-syntax-entry ?\` "$" markdown-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" text-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" rst-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" org-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" coffee-mode-syntax-table)

(setq aw-keys '(?1 ?2 ?3 ?4))

(setq nginx-indent-level 2)

;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below).
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y")
              (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))

(require 'appearance)

(require 'setup-dirtree)

(message (concat "exec-path is " (format "%s" exec-path)))

(defvar initial-file (expand-file-name "init.el" user-emacs-directory))

(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

(defvar desktop-restore-eager 32)
(desktop-save-mode 1)

;; To prevent opening stuff from dirtree from splitting the one reusable window that I use:
;; From https://www.reddit.com/r/emacs/comments/80pd2q/anyone_could_help_me_with_window_management/dux9cme/
;; also potentially useful: https://emacs.stackexchange.com/a/338/2163
(setq display-buffer-alist
      ;; Let popup buffers pop up.
      '(("\*.*popup\*" . (display-buffer-pop-up-window))
        ("\*helm-imenu\*" . (display-buffer-pop-up-window))
        ;; Catchall: always allow same window, which is the one reusable window.
        (".*" . (display-buffer-use-some-window .
                              '((inhibit-same-window . nil)
                                (inhibit-switch-frame . t))))
        ))



(use-package ein
  :config
  (require 'ein-loaddefs)
  (require 'ein-notebook)
  (require 'ein-subpackages)
  :bind
  (:map ein:notebooklist-mode-map
        ("C-c C-g" . 'ein:notebooklist-open))
  (:map ein:notebook-mode-map
        ("C-c C-g" . 'ein:notebooklist-open)))

;; (global-set-key (kbd ')

(provide 'main)

;; Byte-recompile site-lisp-dir.
;;(byte-recompile-directory site-lisp-dir 0)

;; TODO: this is stuff I need to run manually. Make it automatic. Maybe in
;; after-init hooks.
;; (do-nvm-use "6.11.1")
;;
;; (start-process NAME BUFFER PROGRAM &rest PROGRAM-ARGS)
;;
;; (setq debug-on-quit nil)
;;
; TODO: when is semantic-symref-filepattern-alist available? Is it part of grep?
;; (eval-after-load "grep"
;;   '(progn
;;      (add-to-list 'semantic-symref-filepattern-alist '(js2-mode "*.js") t)
;;      (add-to-list 'semantic-symref-filepattern-alist '(coffee-mode "*.coffee") t)
;;      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
