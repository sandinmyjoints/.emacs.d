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

;; Auto-compile elisp to bytecode. This should be as early as possible.
(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

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
;; TODO: this package is not listed anymore -- what's up?
(use-package auto-install
  :disabled
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

(use-package which-key
  :config
  (which-key-mode))

(use-package vlf
  ;; put this in vlf-setup.el, L104:
  ;; ((string-equal filename "TAGS")
  ;;  (let ((large-file-warning-threshold nil))
  ;;    (ad-do-it)))
  :config
  (require 'vlf-setup))

(use-package flycheck
  :ensure t
  :defer 5
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
      json-mode
      perl6-mode))
  (global-flycheck-mode)
  :config
  (setq-default flycheck-display-errors-delay 0.8
                flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                flycheck-disabled-checkers '(javascript-jshint html-tidy emacs-lisp-checkdoc))
  (setq flycheck-global-modes
        '(not org-mode text-mode conf-mode restclient-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-status-emoji-mode 1)
  (flycheck-inline-mode)
  (require 'setup-flycheck))

(require 'wjb)

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package emacs-lisp-mode
  :diminish eldoc-mode
  :mode "abbrev_defs"
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 70)))
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header))

(autoload 'auto-make-header "header2")

(defun auto-fill-comments ()
  "Automatically fill comments, but nothing else"
  (setq-local comment-auto-fill-only-comments t)
  (setq truncate-lines nil))
(add-hook 'prog-mode-hook 'auto-fill-comments)

(use-package comment-dwim-2)

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
  (setq dired-listing-switches "-lahp"
        dired-dwim-target t
        dired-recursive-copies 'always)
  )
(use-package dired+
  :after dired
  :config
  (require 'dired+)
  (toggle-diredp-find-file-reuse-dir 1)
  (add-to-list 'dired-compress-files-alist '("\\.gz\\'" . "gzip -c %i > %o"))
  (setq diredp-hide-details-propagate-flag t))

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
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Abbrevs.html#Editing-Abbrevs
  ;; (list-abbrevs)
  :init
  (add-hook 'fundamental-mode 'abbrev-mode)
  (add-hook 'text-mode-hook 'abbrev-mode)
  (add-hook 'markdown-mode-hook 'abbrev-mode)
  :diminish abbrev-mode)

(use-package ein
  :config
  :disabled
  (require 'ein-loaddefs)
  (require 'ein-notebook)
  (require 'ein-subpackages)
  :bind
  (:map ein:notebooklist-mode-map
        ("C-c C-g" . 'ein:notebooklist-open))
  (:map ein:notebook-mode-map
        ("C-c C-g" . 'ein:notebooklist-open)))

;; Org-mode.
;; (require 'org-install)
;; (eval-after-load 'org '(require 'setup-org))
(use-package org ;; why org not org-mode: https://emacs.stackexchange.com/q/17710
  :defer t
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  :diminish visual-line-mode
  :config
  (setq org-src-fontify-natively t
        org-catch-invisible-edits 'smart
        org-log-done t
        org-clock-persist 'history
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)) ;; can switch back to auto soon
        org-todo-keywords '((sequence "TODO" "ACTIVE" "|" "DONE" "INACTIVE"))
        org-outline-path-complete-in-steps nil
        org-completion-use-ido t
        org-return-follows-link t)
  (require 'setup-org)
  ;; Load ODT backend to allow for exporting to open document format.
  (require 'ox-odt))

(use-package sql
  :config
  ;; needs sqlparse package, which can be gotten with homebrew
  (add-hook 'sql-mode-hook 'sqlformat-mode))

;; Magit.
(use-package magit
  :bind (("C-x g" . magit-status))
  :init
  (mapc (lambda (hook)
          (add-hook hook (lambda ()
                           ;; Turn off smartscan
                           (smartscan-mode -1))))
        '(git-rebase-mode-hook
          magit-mode-hook
          magit-popup-mode-hook))
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

(use-package tsv-mode
  :mode "\\.tsv\\'"
  :init
  (add-hook 'tsv-mode-hook #'display-line-numbers-mode))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))

(use-package phi-search
  :disabled
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward))
  :config)

(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (require 'setup-projectile))

(use-package quickrun
  :config
  (defalias #'runthis #'quickrun))

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

(use-package date-at-point
  :ensure)

;; dims parens.
(use-package paren-face
  :config
  (add-to-list 'paren-face-modes 'js2-mode)
  (global-paren-face-mode))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
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

(push '(:source "~/.emacs.d/.authinfo.gpg") auth-sources)

;; EPG.
(use-package epa-file
  :defer t
  :config
  (epa-file-enable)
  (setenv "GPG_AGENT_INFO" nil))

(use-package smartscan
  :config
  ;; Turn off smartscan in these modes.
  (mapc (lambda (hook)
          (add-hook hook (lambda ()
                           (smartscan-mode -1))))
        '(git-rebase-mode-hook
          magit-mode-hook
          magit-popup-mode-hook
          compilation-mode-hook))

  ;; Use word instead of symbol in these modes.
  (mapc (lambda (hook)
          (add-hook hook (lambda ()
                           (setq smartscan-symbol-selector "word"))))
        '(text-mode-hook
          fundamental-mode-hook
          org-mode-hook))
  ;; TODO: smartscan should use select "word" in comments!
  (setq-default smartscan-symbol-selector "symbol")
  (global-smartscan-mode 1))

(use-package gitignore-mode
  :mode "\\.dockerignore\\'")

(use-package dockerfile-mode
  :mode "Dockerfile-*")

;; Highlight the current column in indentation-sensitive languages
(use-package highlight-indentation
  :commands highlight-indentation-current-column-mode
  :diminish highlight-indentation-current-column-mode
  :init
  (mapc (lambda (hook)
          (add-hook hook 'highlight-indentation-current-column-mode))
        '(coffee-mode-hook
          python-mode-hook
          yaml-mode-hook
          web-mode-hook
          sass-mode-hook))
  :config
  ;; Just a bit lighter than the background
  (require 'color)
  ;; this has to be called after the theme is loaded, because that
  ;; changes what the default background is. but there's no hook:
  ;; https://emacs.stackexchange.com/questions/22686/is-there-a-universal-after-theme-load-hook#26325
  (set-face-background 'highlight-indentation-current-column-face
                       (color-lighten-name
                        (face-attribute 'default :background) 15)))

(use-package highlight-indent-guides
  :disabled ;; doesn't seem to work right
  :init
  (mapc (lambda (hook)
          (remove-hook hook 'highlight-indent-guides-mode))
        '(coffee-mode-hook
          python-mode-hook
          web-mode-hookp
          sass-mode-hook))
  :config
  (setq highlight-indent-guides-method 'column))


(use-package docker-compose-mode)

(use-package docker-tramp)

(use-package conf-mode
  :mode "credentials$"
  "pylintrc"
  "ads.txt"
  "robots.txt"
  "requirements.*.txt"
  "\\.htaccess"
  "\\.curlrc"
  "\\..*rc\\'")

;; Yasnippet.
;; TODO: get this to work with use-package, it doesn't like it.
(when (require 'yasnippet nil t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/aws-snippets/snippets" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/es6-snippets/snippets" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/js-snippets" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/yasnippet-coffee-script-snippets/" t)
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
  (beginend-global-mode)
  (diminish 'beginend-global-mode))

(use-package dotenv-mode
  :mode "\\.env\\'")

(use-package coffee-mode
  :ensure t
  :defer t
  :config
  (require 'setup-coffee)
  (defun wjb/paragraph-boundaries-for-coffee-mode ()
    ;; paragraph-start from js2-mode:    "[ 	]*\\(//+\\|\\**\\)[ 	]*\\(@[[:alpha:]]+\\>\\|$\\)\\|^"
    ;; paragraph-separate from js2-mode: "[ 	]*\\(//+\\|\\**\\)[ 	]*$\\|^"
    ;;
    ;; TODO: remove the parts that have to deal with forward slash,
    ;; which isn't in Coffeescript comments.
    (setq-local paragraph-start "[ 	]*\\(//+\\|\\#*\\)[ 	]*\\(@[[:alpha:]]+\\>\\|$\\)\\|^")
    (setq-local paragraph-separate "[ 	]*\\(//+\\|\\#*\\)[ 	]*$\\|^"))
  (add-hook 'coffee-mode-hook #'wjb/paragraph-boundaries-for-coffee-mode))

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

;; while sml is disabled, just do this:
;; (require 'setup-modeline)

(use-package smart-mode-line
  :ensure t
  :config
  :disabled
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

(require 'setup-word)
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

(use-package helm-aws)

(use-package npm-mode
  ;; Prefer dir locals activation: https://github.com/mojochao/npm-mode#project-activation
  ;; :config
  ;; (npm-global-mode)
  )

(defvar wjb/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

;; based on https://github.com/bhollis/dotfiles/blob/86a1c854050a9ac1e5a205471802373328ee0b4f/emacs.d/init.el#L378
(use-package compile
  :init
  (setq compilation-scroll-output t)
  ;; (setq compilation-scroll-output 'first-error)
  (setq compilation-ask-about-save nil)
  ;; Don't save *anything*
  (setq compilation-save-buffers-predicate '(lambda () nil))
  :config
  ;; Add NodeJS error format
  (setq compilation-error-regexp-alist-alist
        ;; Tip: M-x re-builder to test this out
        ;; original:
        ;; (cons '(node "^[ ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
        ;; messing with it:
        (cons '(node "at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                     1 ;; file
                     2 ;; line
                     3 ;; column
                     nil ;; type
                     nil ;; hyperlink
                     (1 compilation-error-face)
                     )
              ;; (2 compilation-error-face)
              ;; (3 compilation-error-face))
              compilation-error-regexp-alist-alist))
  (setq compilation-error-regexp-alist
        (cons 'node compilation-error-regexp-alist))

  ;; Make *compilation* buffer use visual-line-mode
  ;; TODO: make a key binding for turning vlmode on and off
  (add-hook 'compilation-mode-hook
            (lambda () (visual-line-mode 1)))

  (add-hook 'compilation-minor-mode-hook
            (lambda () (visual-line-mode 1)))

  ;; Allow color in compilation buffers
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (read-only-mode 1)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode -1))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (defun wjb/switch-to-dirtree ()
    "Switch to dirtree buffer."
    (interactive)
    ;; (pop-to-buffer "*dirtree*")

    ;; see display-buffer docs:
    ;; action preserve-size
    ;; (display-buffer-reuse-window . ((preserve-size . (t . t))))
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Action-Functions.html

    ;; why doesn't this work??
    ;; this may be why: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dedicated-Windows.html
    ;; TODO: undedicate dirtree window while this runs, then re-dedicate it
    ;; (pop-to-buffer "*dirtree*" '(display-buffer-reuse-window . ((preserve-size . (t . t)))) t)

    ;; with window-fixed-size set on dirtree window, this works unless
    ;; there are 3+ windows, so TODO undedicate dirtree window
    (unless (s-equals? (buffer-name) "*dirtree*")
      (switch-to-buffer-other-window "*dirtree*" t))
    )

  (defun wjb/switch-to-compilation-buffer ()
    "Switch to *compilation*"
    (interactive)
    (switch-to-buffer "*compilation*"))

  (defun wjb/switch-to-last-compilation-buffer ()
    "Switch to last compilation buffer."
    (interactive)
    (when wjb/last-compilation-buffer
      (switch-to-buffer wjb/last-compilation-buffer)))

  (defun wjb/switch-to-last-grep-buffer ()
    "Switch to last grep buffer."
    (interactive)
    (when wjb/last-grep-buffer
      (switch-to-buffer wjb/last-grep-buffer)))

  ;; based on purcell
  (defadvice compilation-start (after wjb/save-compilation-buffer activate)
    "Save the compilation buffer to find it later."
    (let ((buf-name (buffer-name next-error-last-buffer)))
      (when (s-contains? "compil" buf-name t)
        (setq wjb/last-compilation-buffer next-error-last-buffer))
      (when (s-contains? "grep" buf-name t)
        (setq wjb/last-grep-buffer next-error-last-buffer))))

  (defadvice recompile (around wjb/find-prev-compilation (&optional edit-command) activate)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             wjb/last-compilation-buffer
             (buffer-live-p (get-buffer wjb/last-compilation-buffer)))
        (with-current-buffer wjb/last-compilation-buffer
          ad-do-it)
      ad-do-it))

  :bind
  (("C-c <return>" . compile)
   ("C-c C-<return>" . recompile)))

;; (global-set-key (kbd "C-c <return>") 'compile)
;; (global-set-key (kbd "C-c C-<return>") 'recompile)

;; To (temporarily) disable automatic recompilation turn off
;; (global-recompile-on-save-mode -1).
;;

;; use M-x (reset-recompile-on-save).
;;
;; TODO: mesh this with --watch
(use-package recompile-on-save
  :disabled
  :config
  (recompile-on-save-advice compile)
  (recompile-on-save-advice recompile))


;; Fill column indicator.
;; See: https://github.com/alpaker/Fill-Column-Indicator
;;
(use-package fci-mode
  :disabled
  :config
  (require 'setup-fci)
  (setq fci-rule-color "#555")

  ;; Turn on fci for these modes:
  (dolist (hook '(prog-mode-hook yaml-mode-hook))
    (add-hook hook 'fci-mode))

  ;; ...except for these modes.
  (defun turn-off-fci ()
    (fci-mode -1))

  (dolist (hook '(web-mode-hook lsp-ui-mode-hook))
    (add-hook hook 'turn-off-fci))

  ;; fci-mode doesn't play well with flycheck inlines
  (defun turn-off-fci-before-inlines (errors)
    (when (bound-and-true-p fci-mode)
      (set (make-local-variable 'wjb/fci-mode-was-on) t)
      (turn-off-fci-mode)))

  (defun restore-fci-after-inlines ()
    (when (bound-and-true-p wjb/fci-mode-was-on)
      (turn-on-fci-mode)
      (setq wjb/fci-mode-was-on nil)))

  (advice-add 'flycheck-inline-display-errors
              :before #'turn-off-fci-before-inlines)

  (advice-add 'flycheck-inline-hide-errors
              :after #'restore-fci-after-inlines)

  ;; (advice-remove 'flycheck-inline-display-errors #'turn-off-fci-before-inlines)
  ;; (advice-remove 'flycheck-inline-hide-errors #'restore-fci-after-inlines)


  ;; fci-mode doesn't play well with popups
  (defun on-off-fci-before-company (command)
    (when (and (bound-and-true-p fci-mode) (string= "show" command))
      (set (make-local-variable 'wjb/fci-mode-was-on) t)
      (turn-off-fci-mode))
    (when (and (bound-and-true-p wjb/fci-mode-was-on) (string= "hide" command))
      (turn-on-fci-mode)))

  (advice-add 'company-call-frontends
              :before #'on-off-fci-before-company))

(use-package knot-mode
  :mode "\\.knot\\'")

(use-package lsp-mode
  :config
  (setq lsp-project-blacklist '("neodarwin")))

(use-package lsp-ui
  :config
  (setq lsp-ui-flycheck-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-flycheck nil
        lsp-ui-doc-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-sideline-ignore-duplicate t)
  ;; (require 'lsp-ui-flycheck)

  ;; once lsp-ui is registered as a checker, somehow it seems to stop
  ;; other checkers from running. may be related to
  ;; https://github.com/emacs-lsp/lsp-ui/issues/190 but when I checked
  ;; the value of flycheck-checker, it was nil. So I don't understand
  ;; how it is stopping other checkers. Here is how to disable lsp-ui:

  ;; (setq flycheck-disabled-checkers (append '(lsp-ui) flycheck-disabled-checkers))

  ;; Until it works better, instead of using lsp-ui always...
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  ;; just use it for a whitelist of modes.
  (dolist (hook '(sh-mode-hook))
    (add-hook hook 'lsp-ui-mode)))

;; Caveats:
;;
;; - Not sure if it is respecting jsconfig.json or not.
;;
;; - In Neodarwin, got (lsp-timed-out-error), probably because the
;;   repo is so big. So, Neodarwin is on the lsp blacklist.
;;
;; - TODO: defer starting lsp javascript server until nvm is figured
;;   out.
(use-package lsp-javascript-typescript
  :disabled
  :config
  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable))

;; TODO: this is not doing anything
;; TODO: also use less for flycheck
(use-package lsp-css
  :config
  (add-hook 'less-mode-hook #'lsp-less-enable))

(use-package lsp-sh
  :config
  (add-hook 'sh-mode-hook #'lsp-sh-enable))

(use-package lsp-html
  :config
  (add-hook 'html-mode-hook #'lsp-html-enable))

;; from https://gitlab.petton.fr/nico/emacs.d/
(use-package whitespace
  :config
  (setq whitespace-display-mappings
        '(
          (space-mark 32 [183] [46]) ; normal space, ·
          (space-mark 160 [164] [95])
          (space-mark 2208 [2212] [95])
          (space-mark 2336 [2340] [95])
          (space-mark 3616 [3620] [95])
          (space-mark 3872 [3876] [95])
          (newline-mark 10 [182 10]) ; newlne, ¶
          (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
          )))

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
        (".*" .
         (display-buffer-use-some-window .
                                         '((inhibit-same-window . nil)
                                           (inhibit-switch-frame . t))))
        ))

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
