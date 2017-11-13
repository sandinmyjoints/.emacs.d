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

;; Are we on a mac?
(defvar is-mac (equal system-type 'darwin))

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

;; Save desktop.
(desktop-save-mode 1)
(defvar desktop-restore-eager 32)

;; ========================================
;; Machine-local custom configuration.
;; ========================================

(load custom-file t t)

;; ========================================
;; Package management.
;; ========================================

(require 'setup-package)

;; From purcell. TODO: replace with use-package. A bunch of stuff uses this,
;; including in sane-defaults.
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

(require 'setup-flycheck)

(require 'wjb)

(require 'appearance)

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 80)))

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; ibuffer.
(autoload 'ibuffer "ibuffer" "List buffers." t)

(use-package smex
  :config
  (smex-initialize)
  (setq smex-auto-update nil)
  (smex-auto-update 10))

(use-package beacon
  :config
  (setq beacon-blink-duration 0.1)
  (beacon-mode 1))

(use-package ido
  :config
  (require 'setup-ido))

;; Imenu.
(when (require 'imenu nil t)
  (autoload 'idomenu "idomenu" nil t))

(defadvice ido-imenu (before push-mark activate)
    (push-mark))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; This line must run *before* dired is loaded:
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (require 'setup-dired+))

;; Org-mode.
;; (require 'org-install)
;; (eval-after-load 'org '(require 'setup-org))
(use-package org
  :defer t
  :config
  (require 'setup-org))

;; Magit.
(autoload 'magit-status "magit")
(autoload 'magit-log "magit")
(eval-after-load 'magit '(require 'setup-magit))

;; Python.
(require 'setup-python)

;; Rainbow mode.
(use-package rainbow-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

;; Smart-tab. See: https://raw.github.com/genehack/smart-tab/master/smart-tab.el
;; and https://www.emacswiki.org/emacs/TabCompletion#SmartTab
(use-package smart-tab
  :config
  (global-smart-tab-mode -1))

(use-package anzu
  :config
  (global-anzu-mode 1))

(require 'setup-projectile)
(require 'setup-tramp)

(defun mine-goto-symbol-at-point ()
  "Will navigate to the symbol at the current point of the cursor."
  (interactive)
  (ido-goto-symbol (thing-at-point 'symbol)))

;; Map files to modes.
(require 'mode-mappings)

;; Lua mode.
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Fill column indicator.
;; See: https://github.com/alpaker/Fill-Column-Indicator
;;
(when (require 'fill-column-indicator nil t)

  (defun turn-on-fci ()
    (fci-mode 1))

  (add-hook 'coffee-mode-hook 'turn-on-fci)
  (add-hook 'js2-mode-hook 'turn-on-fci)
  (add-hook 'python-mode-hook 'turn-on-fci)

  ;; Make fci-mode global...
  ;; (define-globalized-minor-mode global-fci-mode fci-mode
  ;;   (lambda () (fci-mode 1)))
  ;; (global-fci-mode 1)

  ;; ...except for these modes.
  (defun turn-off-fci ()
    (fci-mode -1))

  ;; (add-hook 'dirtree-mode-hook 'turn-off-fci)
  ;; (add-hook 'dired-mode-hook 'turn-off-fci)
  ;; (add-hook 'dired+-mode-hook 'turn-off-fci)
  ;; (add-hook 'org-mode-hook 'turn-off-fci)
  ;; (add-hook 'magit-mode-hook 'turn-off-fci)
  ;; (add-hook 'term-mode-hook 'turn-off-fci)
  ;; (add-hook 'shell-mode-hook 'turn-off-fci)
  ;; (add-hook 'edit-server-start-hook 'turn-off-fci)
  ;; (add-hook 'edit-server-start-hook 'my-edit-server-hook)
  ;; (add-hook 'edit-server-edit-mode-hook 'my-edit-server-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nvm)

;; js2-mode
(eval-after-load 'js2-mode '(require 'setup-js2-mode))

(add-hook 'js2-mode-hook '(lambda () (set-fill-column 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :config
  (epa-file-enable))

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

;; Ctags.
(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R --exclude=node_modules --exclude=test %s" path-to-ctags (directory-file-name dir-name))))

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
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.env.*\\'" . dotenv-mode)))

;; (eval-after-load 'smartparens-mode '(require 'setup-smartparens))

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
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-x t") 'sane-term)
  (global-set-key (kbd "C-x T") 'sane-term-create))

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (require "setup-modeline"))

(require 'setup-ediff)
(require 'setup-docker)
(require 'setup-webmode)
(require 'setup-markdown)
(require 'key-bindings)

(when is-mac (require 'setup-mac))

(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Linum: put spaces around line numbers.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep region active when hit C-g. From http://emacs.stackexchange.com/a/11064
(defun my-keyboard-quit-advice (fn &rest args)
  (let ((region-was-active (region-active-p)))
    (unwind-protect
         (apply fn args)
      (when region-was-active
        (activate-mark t)))))

(advice-add 'keyboard-quit :around #'my-keyboard-quit-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from http://rawsyntax.com/blog/learn-emacs-use-defadvice-modify-functions/
;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; Load something that might be useful.
;; An initial file to open if it exists.
(defvar initial-file (expand-file-name "init.el" user-emacs-directory))

(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

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

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; Byte-recompile site-lisp-dir.
;;(byte-recompile-directory site-lisp-dir 0)

(require 'setup-dirtree)

(provide 'main)

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
