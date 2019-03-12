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
  (progn
    (require 'use-package)
    (setq use-package-verbose t)))

;; Are we on a mac?
(defvar is-mac (equal system-type 'darwin))

(setq user-full-name "William Bert"
      user-mail-address "william.bert@gmail.com")

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
  :defer 5
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

(use-package paradox
  :defer t
  :config
  (paradox-enable))

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
  :diminish
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
  :defer t
  :init
  ;; This turns on Flycheck globally in only these modes. Others can be turned on
  ;; per-buffer.
  (defvar flycheck-global-modes
    '(js2-mode
      js2-jsx-mode
      rjsx-mode
      json-mode
      coffee-mode
      sql-mode
      emacs-lisp-mode
      sh-mode
      yaml-mode
      python-mode
      perl-mode
      css-mode
      less-css-mode
      perl6-mode))
  ;; (setq flycheck-global-modes
  ;;       '(not org-mode text-mode conf-mode restclient-mode))
  (global-flycheck-mode)
  :config
  (setq-default flycheck-display-errors-delay 0.8
                flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                flycheck-disabled-checkers '(javascript-jshint html-tidy emacs-lisp-checkdoc))
  (add-to-list 'safe-local-variable-values '(flycheck-javascript-eslint-executable . "eslint_d"))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-status-emoji-mode 1)
  (require 'setup-flycheck)
  (flycheck-inline-mode))

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

(use-package beacon
  :defer t
  :config
  (setq beacon-blink-duration 0.1)
  (beacon-mode 1))

(use-package dired
  :init
  ;; This line must run *before* dired is loaded:
  ;; See http://emacs.stackexchange.com/questions/28016/dired-does-not-respect-listing-dired-listing-switches
  ;; Details toggling is bound to "(" in `dired-mode' by default.
  (setq diredp-hide-details-initially-flag nil)
  :bind (:map dired-mode-map
              ("C-c f" . find-name-dired))
  :config
  ;; unbind C-o (was diredp-find-file-other-frame) for use by helm-mini
  (unbind-key (kbd "C-o") dired-mode-map)

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
  :defer t
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
  :defer 4
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Abbrevs.html#Editing-Abbrevs
  ;; (list-abbrevs)
  :init
  (add-hook 'fundamental-mode 'abbrev-mode)
  (add-hook 'text-mode-hook 'abbrev-mode)
  (add-hook 'markdown-mode-hook 'abbrev-mode)
  :diminish abbrev-mode)

;; Org-mode.
;; (require 'org-install)
;; (eval-after-load 'org '(require 'setup-org))
(use-package org ;; why org not org-mode: https://emacs.stackexchange.com/q/17710
  :defer t
  :diminish visual-line-mode
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
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

(use-package sql)
(use-package sqlformat
  :after sql
  :config
  ;; needs sqlparse package, which can be gotten with homebrew
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))

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

(defun wjb/set-highlight-indentation-current-column-face ()
  "Just a bit lighter than the background."
  (set-face-background 'highlight-indentation-current-column-face
                       (color-lighten-name
                        (face-attribute 'default :background) 15)))

;; Highlight the current column in indentation-sensitive languages. Just want
;; 0.6.0 because later versions cause breakage with elpy, I think.
(use-package highlight-indentation
  :commands highlight-indentation-current-column-mode
  :diminish highlight-indentation-current-column-mode
  :defer 4
  :disabled
  :config
  (require 'color)
  (mapc (lambda (hook)
          (add-hook hook #'wjb/set-highlight-indentation-current-column-face)
          (add-hook hook 'highlight-indentation-current-column-mode))
        '(coffee-mode-hook
          yaml-mode-hook
          ;; python-mode-hook ;; let elpy set this up
          ;; web-mode-hook ;; breaks due to absence of web-mode-html-offset
          sass-mode-hook))
  )

(use-package elpy
  :disabled
  :config
  (elpy-enable)
  (setq elpy-modules (-remove-item 'elpy-module-flymake elpy-modules)))

;; Python.
(use-package python
  :defer t
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 2)
  (setq-default python-fill-docstring-style 'django)

  ;; This is https://github.com/jorgenschaefer/pyvenv
  ;; - pyvenv-* commands
  ;; - comes with elpy
  (defalias 'workon 'pyvenv-workon)

  (add-hook 'python-mode-hook (lambda ()
                                (hack-local-variables)
                                (setq fill-column 79)
                                (set-face-background 'highlight-indentation-face "#111")
                                (when (boundp 'project-venv-name)
                                  (venv-workon project-venv-name)
                                  (pyvenv-workon project-venv-name))))

  ;; This is https://github.com/porterjamesj/virtualenvwrapper.el
  ;; - venv-* commands.
  ;; - TODO: might get rid of virtualenvwrapper.el now that using elpy.
  (require-package 'virtualenvwrapper)

  ;; To use, put the following into custom.el:
  ;; (setq venv-location "path/to/virtualenvs/")

  ;; if you want interactive shell support
  (venv-initialize-interactive-shells)

  ;; if you want eshell support
  ;;(venv-initialize-eshell)

  (defadvice run-python (before setup-repl ())
    "Use IPython if available."
    (if (executable-find "ipython")
        (setq
         python-shell-interpreter "ipython"
         ;;python-shell-interpreter-args "--no-banner --gui=osx"
         python-shell-prompt-regexp "In \\[[0-9]+\\]: "
         python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
         python-shell-completion-setup-code
         "from IPython.core.completerlib import module_completion"
         python-shell-completion-module-string-code
         "';'.join(module_completion('''%s'''))\n"
         python-shell-completion-string-code
         "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
      (setq
       python-shell-interpreter "python"
       python-shell-interpreter-args "-i"
       python-shell-prompt-regexp ">>> "
       python-shell-prompt-output-regexp ""
       python-shell-completion-setup-code
       "try:\n    import readline\nexcept ImportError:\n    def __COMPLETER_all_completions(text): []\nelse:\n    import rlcompleter\n    readline.set_completer(rlcompleter.Completer().complete)\n    def __COMPLETER_all_completions(text):\n        import sys\n        completions = []\n        try:\n            i = 0\n            while True:\n                res = readline.get_completer()(text, i)\n                if not res: break\n                i += 1\n                completions.append(res)\n        except NameError:\n            pass\n        return completions"
       python-shell-completion-module-string-code ""
       python-shell-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))"
       )))
  (ad-activate 'run-python)

  ;; from https://emacs.stackexchange.com/a/30970/2163
  ;; (with-eval-after-load 'python
  ;;   (defun python-shell-completion-native-try ()
  ;;     "Return non-nil if can trigger native completion."
  ;;     (let ((python-shell-completion-native-enable t)
  ;;           (python-shell-completion-native-output-timeout
  ;;            python-shell-completion-native-try-output-timeout))
  ;;       (python-shell-completion-native-get-completions
  ;;        (get-buffer-process (current-buffer))
  ;;        nil "_"))))
  )

(use-package ein
  :after python
  :disabled
  :bind
  (:map ein:notebooklist-mode-map
        ("C-c C-g" . 'ein:notebooklist-open))
  (:map ein:notebook-mode-map
        ("C-c C-g" . 'ein:notebooklist-open)))

(use-package pip-requirements
  :after python
  :defer t)

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

;; Rainbow mode.
(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)
  (add-hook 'conf-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

(use-package tsv-mode
  :defer t
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
  (require 'setup-projectile)
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
  (counsel-projectile-mode)
  )

(use-package ido
  :config
  (require 'setup-ido)
  (global-set-key (kbd "C-x C-f") 'ido-find-file))

;; ibuffer.
(autoload 'ibuffer "ibuffer" "List buffers." t)

(defun wjb/bury-ibuffer (orig-fun &rest args)
  "Never want to switch back to *Ibuffer* after choosing a buffer from it."
  (bury-buffer "*Ibuffer*")
  (apply orig-fun args))

(advice-add 'ibuffer-visit-buffer :around #'wjb/bury-ibuffer)

;; Imenu.
(when (require 'imenu nil t)
  (autoload 'idomenu "idomenu" nil t))

(defadvice ido-imenu (before push-mark activate)
  (push-mark))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(use-package flx)

(use-package smex
  :bind (("M-X" . smex-major-mode-commands)
         ;; ("M-x" . smex)
         )
  :config
  (smex-initialize)
  (setq smex-auto-update nil)
  (smex-auto-update 10)
  (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command))

;; switching/finding/opening/running things
;; - C-o = helm-mini -> buffers, recent files, bookmarks, more?
;; - C-x b = switch buffer (among open buffers)
;;   - C-x C-b = ibuffer
;;   - H-x b = helm-buffers-list
;;   - switch buffer among buffers limited to current project?
;; - helm-mini limited to current project?-> H-0 C-o = helm-browse-project
;; - C-c p f = find file in project
;; - M-x commands to run
;; - C-x C-f -> open/find file (least used)
(use-package ivy
  :demand
  :diminish
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer) ;; Use C-M-j to call ivy-immediate-done to create new buffer
  ;; consider:
  ;; (global-set-key (kbd "C-s") 'swiper)
  ;; (global-set-key (kbd "C-x C-f") 'ido-find-file)
  ;;
  (setq ivy-height-alist '((counsel-evil-registers . 5)
                           (counsel-yank-pop . 10)
                           (counsel-git-log . 10)
                           (counsel--generic . 12)
                           (counsel-el . 12)))

  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-height 12
        ivy-on-del-error-function 'ignore
        ivy-display-function nil
        ;; overlay would be great if:
        ;; - border around the box
        ;; - consistent placement of the box; it seems to be related to where point is
        ;; ivy-display-function #'ivy-display-function-overlay
        ivy-format-function 'ivy-format-function-arrow
        ivy-virtual-abbreviate 'abbreviate
        ivy-magic-tilde nil
        ivy-initial-inputs-alist nil

        ;; references on ivy-re-builders-alist:
        ;; - https://emacs.stackexchange.com/a/36748/2163
        ;; - https://oremacs.com/2016/01/06/ivy-flx/
        ;;
        ivy-re-builders-alist '((swiper . ivy--regex-ignore-order)
                                (counsel-projectile-switch-project . ivy--regex-ignore-order)
                                (ivy-switch-buffer . ivy--regex-fuzzy)
                                (t . ivy--regex-fuzzy))
        )
  (ivy-mode 1))

(use-package counsel
  :defer t
  :config
  (setq counsel-find-file-at-point t
        counsel-preselect-current-file t
        counsel-yank-pop-height 12
        counsel-yank-pop-preselect-last t))

(require 'helm-config)
;; TODO: C-g when helm-mini is showing actually quits
(use-package helm
  :defer t
  :config
  (global-set-key (kbd "C-o") #'helm-mini)  ;; within helm-mini, helm-mini again jumps to next section -- nice!
  (global-set-key (kbd "H-o") #'helm-mini)
  (global-set-key (kbd "H-x b") #'helm-buffers-list)
  (global-set-key (kbd "M-o") #'helm-browse-project)
  (global-set-key (kbd "H-0 C-o") #'helm-browse-project)

  ;; useful commands, but probably shouldn't be bound globally:
  ;; (global-set-key (kbd "C-'") 'helm-mark-all)
  ;; (global-set-key (kbd "C-\"") 'helm-ff-run-marked-files-in-dired)

  (require 'helm-dired-recent-dirs)
  ;; TODO: would like to add a source of files in the current project, maybe even all files
  ;;
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-file-cache
                                    helm-source-files-in-current-dir
                                    helm-source-dired-recent-dirs
                                    helm-source-buffer-not-found
                                    ))

  (setq
   ;; helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
   ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
   ;; helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
   ;; helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
   ;; helm-ff-file-name-history-use-recentf t
   helm-echo-input-in-header-line t
  ;; make helm-mini use fuzzy matching, so weborg will not match sd-web.org.
  ;; Because ivy-switch-buffers is using fuzzy matching, want the muscle memory
  ;; to be the same.
   helm-recentf-fuzzy-match t
   helm-buffers-fuzzy-matching t
   )
  )

(use-package helm-org-rifle
  :defer)

(use-package ace-jump-helm-line
  :config
  (setq ace-jump-helm-line-idle-delay 3
        ace-jump-helm-line-style 'pre
        ;; ace-jump-helm-line-style 'de-bruijn

        ;; Select by default
        ace-jump-helm-line-default-action 'select
        ;; Set the move-only and persistent keys
        ace-jump-helm-line-select-key ?s ;; this line is not needed
        ace-jump-helm-line-move-only-key ?m
        ace-jump-helm-line-persistent-key ?p
        )
  (ace-jump-helm-line-idle-exec-add 'helm-mini)
)

(use-package helm-xref
  :after helm
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-aws
  :after helm)

(use-package quickrun
  :defer t
  :config
  (defalias #'runthis #'quickrun))

(use-package dumb-jump
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))
  ;; C-M-g dumb-jump-go -- would like to use M-.
  ;; C-M-p dumb-jump-back -- M-,
  :config
  (setq dumb-jump-selector 'helm)
  ;; (setq dumb-jump-selector 'ivy)
  (add-hook 'prog-mode-hook #'dumb-jump-mode))

(use-package smart-jump
  :bind (("C-M-g" . smart-jump-go)
         ("C-M-p" . smart-jump-back))
  :config
  (smart-jump-setup-default-registers)
  ;; this binds to M-. and M-, in prog-mode-map:
  (smart-jump-bind-jump-keys 'prog-mode))

(require 'setup-tramp)

(require 'mode-mappings)

;; Lua mode.
(use-package lua
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package gitignore-mode
  :mode "global.gitignore")

(use-package json-mode
  :defer t)

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
  :defer t
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
  :defer t
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
  :defer t
  :init
  (add-hook 'json-mode-hook #'rainbow-delimiters-mode))

(use-package date-at-point
  :defer t)

;; Highlight matching parentheses when point is on them.
;;
(use-package paren
  :config
  (setq show-paren-delay 0
        show-paren-style 'parenthesis
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-highlight-openparen t)
  (show-paren-mode 1))

;; Dims parens in certain modes.
(use-package paren-face
  :defer t
  :config
  (add-to-list 'paren-face-modes 'js-mode 'js2-mode)
  (global-paren-face-mode))

(use-package restclient
  :defer t
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
  :defer t
  :bind (:map global-map
              ("C-x t" . 'set-rectangular-region-anchor)
              ("C->" . 'mc/mark-next-like-this)
              ("C-<" . 'mc/mark-previous-like-this)
              ("C-*" . 'mc/mark-all-like-this)
              ("C-M-<" . 'mc/edit-beginnings-of-lines)
              ("C-M->" . 'mc/edit-ends-of-lines)))

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(use-package expand-region
  :defer t
  :bind (:map global-map
              ("C-=" . 'er/expand-region)))

(require 'setup-dirtree)
(with-eval-after-load 'dirtree
  (progn
    ;; Free up for helm-mini.
    (unbind-key (kbd "C-o")  dirtree-mode-map)
    (bind-key (kbd "<return>") 'dirtree-display dirtree-mode-map)))

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

;; (require 'lisp-stuff)

;; Lines in this file take the form of:
;; machine api.github.com login sandinmyjoints^magit password SECRET_THING
;;
;; They can be retrieved with (auth-source-user-and-password "api.github.com" "sandinmyjoints^magit")
;; (take the cadr of what's returned by that)
;;
;; To see if cache is being used: (setq auth-source-debug 'trivia)
;; then check messages buffer.
;;
;; To clear auth source cache: (setq auth-source-netrc-cache '())
;;
(push '(:source "~/.emacs.d/.authinfo.gpg") auth-sources)

(setq pivotal-api-token
      (cadr (auth-source-user-and-password "api.pivotaltracker.com" "williambert"))

      paradox-github-token
      (cadr (auth-source-user-and-password "api.github.com" "sandinmyjoints^paradox")))

;; EPG.
(use-package epa-file
  :defer t
  :config
  (epa-file-enable)
  (setenv "GPG_AGENT_INFO" nil))

;; Usage
;;
;; To use `symbol-overlay' in your Emacs, you need only to bind these keys:
;; (require 'symbol-overlay)
;; (global-set-key (kbd "M-i") 'symbol-overlay-put)
;; (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
;; (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
;; (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
;;
;; Default key-bindings are defined in `symbol-overlay-map'.
;; You can re-bind the commands to any keys you prefer by simply writing
;; (define-key symbol-overlay-map (kbd "your-prefer-key") 'any-command)
;; prog-mode-hook
(use-package symbol-overlay
  :defer t
  :bind (:map prog-mode-map
              ("M-i" . 'symbol-overlay-put)
              ("M-n" . 'symbol-overlay-jump-next)
              ("M-p" . 'symbol-overlay-jump-prev)
              ("<f7>" .  'symbol-overlay-mode)
              ("<f8>" .  'symbol-overlay-remove-all)))

;; TODO
         ;; ("M-p" . smart-jump-back)))
  ;; :hook (prog-mode . symbol-overlay-mode))

;; disabled: in jsx file, AudioToggle move next tries to go "audio" even when
;; using symbol. Prefer symbol-overlay.
(use-package smartscan
  :disabled
  :defer t
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
  :mode "\\.dockerignore\\'"
  ".*gitignore\\'"
  :defer t)

(use-package dockerfile-mode
  :mode "Dockerfile-*"
  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package docker-tramp
  :defer t)

(use-package conf-mode
  :defer t
  :mode "credentials$"
  "pylintrc"
  "ads.txt"
  "robots.txt"
  "requirements.*.txt"
  "\\.htaccess"
  "\\.curlrc"
  "\\..*rc\\'"
  )

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
;; These are great snippets, but loading them is causing some warnings:
;; (eval-after-load 'yasnippet '(use-package emacs-snippets))

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
  (require 'setup-webmode)

  ;; experimental:
  (require 'company-web-html)
  (require 'company-web-jade)
  (defun wjb/web-mode-company ()
    (set (make-local-variable 'company-backends)
         '((company-web-html :with company-dabbrev-code company-gtags company-etags company-keywords)))
    (company-mode t))
  (add-hook 'web-mode-hook #'wjb/web-mode-company))

(require 'key-bindings)

(require 'setup-word)

(require 'setup-markdown)

(defun wjb/set-company-minimum-prefix-length ()
  (setq-local company-minimum-prefix-length 3))

(defun wjb/revert-company-backends ()
  ;; current
  (setq company-backends
        '(
          company-emoji
          company-bbdb
          company-eclim
          company-semantic
          company-clang
          company-xcode
          company-cmake
          company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse
          company-dabbrev)))
(wjb/revert-company-backends)

(defun wjb/experimental-company-backends ()
  "Try some backend orderings."
  ;; mode-specific, smart
  (let (zing (list))
    (dolist
        ;; last ends up first
        (backend '(company-clang company-xcode company-cmake company-capf company-shell company-lsp company-restclient company-css) zing)
      (push
       (list backend :with 'company-dabbrev-code 'company-dabbrev 'company-emoji 'company-keywords)
       zing))

    ;; generic
    (setq zing (append zing '(company-files)))
    ;; fallback backends -- likely to return in almost all cases
    (setq zing (append zing
               '(
                 ;; code
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 ;; text
                 (company-emoji company-dabbrev)
                 )
               ))
    (setq company-backends zing)))
(wjb/experimental-company-backends)

;; company-mode TODO:
;; (company-emoji company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
;;                (company-dabbrev-code company-gtags company-etags company-keywords)
;;                company-oddmuse company-dabbrev)

;; - order backends in a way that makes sense
;; - group backends
;; - set backends based on major mode. For example, higher minimum prefix in text modes (4)
;;   (add-hook 'js-mode-hook '(lambda () (setq-local company-backends '((company-web company-css company-tern :with company-yasnippet)))))
;; - different behavior within comments
;; - understand company-capf

;; - how backends work: https://superuser.com/a/528407/93702
;; - another reference: https://www.reddit.com/r/emacs/comments/8z4jcs/tip_how_to_integrate_company_as_completion/
;; - another: https://www.reddit.com/r/emacs/comments/5q0vmz/anyone_using_yasnippet_companymode_tern/

;; - dynamic backend: 1) mode-specific, grouped with dabbrev stuff in case mode-specific is not smart
;; - strong backend:
;; - text/markdow backend:
;; - org backend:

;; - company-diag
;; - company-yasnippet -- specific binding for this
;; - company-shell
;; - company-web

;; - push mutates, puts newelt in front
;; (setq l '())
;; (push 1 l)
;; - add-to-list mutates, puts newelt in front
;; (add-to-list 'l 2)
;; - but with arg, it puts it in back
;; (add-to-list 'l 4 t)
;; - append does not mutate
;; (append l '(5))
;; - delq mutates
;; (delq 5 l)
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 4)
  (company-show-numbers nil)
  (company-tooltip-align-annotations 't)
  (company-dabbrev-downcase nil)
  (global-company-mode t)

  :config
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (add-hook 'prog-mode-hook #'wjb/set-company-minimum-prefix-length)
  ;; trial:
  (add-hook 'after-init-hook #'company-statistics-mode)
  )

;; trial:
(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package company-buffer-line
  :commands (company-same-mode-buffer-lines)
  :bind ("C-x C-l" . company-same-mode-buffer-lines))

(global-set-key (kbd "H-0 y") #'company-yasnippet)
(global-set-key (kbd "C-c y") #'company-yasnippet)
(global-set-key (kbd "C-c C-y") #'company-yasnippet)

(use-package company-emoji
  :after company
  :config
  ;; TODO: this should probably only be used in non-prog-mode descendents.
  (push 'company-emoji company-backends))

(use-package company-restclient
  :after company
  (push 'company-restclient company-backends))

(use-package shell-script-mode
  :mode "\\.bash*")

(use-package helpful
  :defer 2
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package elisp-demos
  :config
  ;; this is for normal help:
  ;; (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; this is specific to helpful:
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package atomic-chrome
  :disabled
  :config
  (atomic-chrome-start-server))

(use-package google-this
  :defer t
  ;; C-c / n|SPC|l
  :diminish google-this-mode
  :config
  (google-this-mode 1))

(use-package vimish-fold
  :disabled
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

(use-package npm-mode
  ;; Prefer dir locals activation: https://github.com/mojochao/npm-mode#project-activation
  ;; :config
  ;; (npm-global-mode)
  )

;; See:
;; - comint-output-filter-functions
;; - comint-preoutput-filter-functions
;;
;; Ref: https://www.reddit.com/r/emacs/comments/3scsak/incredibly_slow_comint_eg_shell_compile_output_on/
;;
;; comint-strip-ctrl-m
(use-package comint
  :config
  (add-hook 'comint-mode-hook
            (lambda ()
              (toggle-truncate-lines 1)
              (make-local-variable 'jit-lock-defer-timer)
              (set (make-local-variable 'jit-lock-defer-time) 0.25))))

(require 'cl-lib)
(defun endless/toggle-comint-compilation ()
  "Restart compilation with (or without) `comint-mode'."
  (interactive)
  (cl-callf (lambda (mode) (if (eq mode t) nil t))
      (elt compilation-arguments 1))
  (recompile))

(defvar wjb/last-compilation-buffer nil
  "The last buffer in which compilation took place.")
(defvar wjb/last-grep-buffer nil
  "The last grep buffer.")
;; TODO: set this after switching to a restclient buffer.
;; TODO: add key-binding to get to last restclient buffer.
(defvar wjb/last-restclient-buffer nil
  "The last restclient buffer.")

;; based on https://github.com/bhollis/dotfiles/blob/86a1c854050a9ac1e5a205471802373328ee0b4f/emacs.d/init.el#L378
;; comint mode is interactive, compilation-shell-minor-mode is
;; C-c RET starts compilation, keys don't do anything
;; C-u C-c RET starts comint with compilation-minor-mode, keys are sent -- BUT it's not doing anything for Jest, which probably isn't listening for input? maybe because terminfo is set to dumb? TODO: experiment with other values of TERM
(use-package compile
  :config
  (setq compilation-scroll-output t
        ;; set to "dumb" to not get colors codes
        ;; ansi and xterm-256colors both get movement/scrolling codes from jest output, not just colors
        comint-terminfo-terminal "dumb"
        ;; comint-terminfo-terminal "ansi"
        ;; comint-terminfo-terminal "xterm-256colors"
        ;;
        ;; https://stackoverflow.com/a/42467888/599258 talks about dumb-emacs-ansi
        ;; comint-terminfo-terminal "dumb-emacs-ansi"
        ;;
        ;; From https://unix.stackexchange.com/a/237947/14423:
        ;;
        ;; this is telling Jest (and other ncurses programs) they can use color codes
        ;; but not movement codes, I think.
        compilation-environment '("TERM=dumb" "COLORTERM=1") ;; default nil
        comint-prompt-read-only nil
        comint-scroll-to-bottom-on-input t
        compilation-ask-about-save nil
        comint-use-prompt-regexp nil
        ;; Don't save *anything*
        compilation-save-buffers-predicate '(lambda () nil)
        ;; compilation-scroll-output 'first-error
        )

  (add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map [remap kill-region] 'comint-kill-region)
            (define-key comint-mode-map [remap kill-whole-line]
              'comint-kill-whole-line)))

  ;; Add NodeJS error format
  ;; TODO:
  ;; - rewrite using pcre2el
  ;; - can it be optimized so compilation buffers use less cpu?
  ;;   - optimize regex
  ;;   - run on fewer lines?
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
            (lambda () (visual-line-mode 1)
              (npm-mode 1)))

  (add-hook 'compilation-minor-mode-hook
            (lambda () (visual-line-mode 1)))

  ;; Handle ANSI color in compilation buffers.

  ;; Approach 1: xterm-color. This is promising, but disabled because it scrolls
  ;; test output when it should be overwriting. Is this related to the issue
  ;; about tput reset? https://github.com/atomontage/xterm-color/issues/24 Or
  ;; something else? What code is Jest (mocha, etc) using to move the cursor
  ;; back?
  ;;
  ;; TODO: can I use this just for grep buffers?
  ;; TO RE-ENABLE:
  ;; - uncomment setq compilation-environment
  ;; - change remove-hook to add-hook
  ;;
  ;; From https://github.com/atomontage/xterm-color
  ;; (setq compilation-environment '("TERM=xterm-256color")) ;; default nil
  (defun xterm-color-compilation-start-hook (proc)
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint (which at this point we assume
    ;; has been configured separately for xterm-color)
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc
                  (xterm-color-filter string))))))
  (remove-hook 'compilation-start-hook #'xterm-color-compilation-start-hook)

  ;; what I really want is to add-hook compilation-start-hook only when entering a grep mode buffer
  ;; (make-variable-buffer-local 'compilation-environment)
  ;; (make-variable-buffer-local 'compilation-start-hook)
  ;; (add-hook grep-mode-hook
  ;;           #'(lambda ()
  ;;               (setq compilation-environment '("TERM=xterm-256color"))
  ;;               (add-hook 'compilation-start-hook xterm-color-compilation-start-hook)) t t)

  ;; Approach 2:
  ;; From https://stackoverflow.com/a/20788581/599258
  ;;
  ;; Handles test scrolling just fine. Possibly less performant than approach 1.
  ;; Possibly more performant than approach 3. Colors are not as nice as
  ;; xterm-256color. Additional reference:
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  ;;
  ;; (ignore-errors
  ;;   (require 'ansi-color)
  ;;   (defun wjb/colorize-compilation-buffer ()
  ;;     (when (eq major-mode 'compilation-mode)
  ;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
  ;;   (add-hook 'compilation-filter-hook 'wjb/colorize-compilation-buffer))

  ;; Approach 3:
  ;; From https://stackoverflow.com/a/13408008/599258
  ;;
  ;; Works fine. Possibly less performant than approach 2.
  ;;
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (read-only-mode 1)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode -1))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
;; Disabled because too flaky, too many problems with various modes.
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

;; Caveats about lsp for javascript:
;;
;; - Not sure if it is respecting jsconfig.json or not.
;;
;; - In Neodarwin, got (lsp-timed-out-error), probably because the
;;   repo is so big. So, Neodarwin is on the lsp blacklist.
;;
;; - TODO: defer starting lsp javascript server until nvm is figured
;;   out.
(use-package lsp-mode
  :disabled
  :commands lsp
  :hook (less-mode . lsp)
  :hook (sh-mode . lsp)
  :hook (html-mode . lsp)
  :hook (js-mode . lsp)
  :hook (js2-mode . lsp)
  :hook (python-mode. lsp)
  :init
  (setq lsp-prefer-flymake nil)
  :config
  (setq lsp-auto-guess-root t
        lsp-eldoc-enable-hover nil
        lsp-response-timeout 5
        lsp-project-blacklist '("neodarwin" "neodarwin-worktree")))

(use-package lsp-ui
  :disabled
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; TODO: call lsp-ui-flycheck-enable per mode or buffer
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
  ;; (dolist (hook '(sh-mode-hook))
  ;;   (add-hook hook 'lsp-ui-mode))
  )

(use-package company-lsp
  :disabled
  :after lsp-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

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

(use-package hungry-delete
  :diminish
  :config
  (setq hungry-delete-chars-to-skip " \t"
        hungry-delete-except-modes '(help-mode minibuffer-inactive-mode calc-mode))
  ;; TODO: turn off in some modes:
  ;; - org-mode?
  ;; - whitespace sensitive modes?
  (global-hungry-delete-mode))

(use-package nginx-mode
  :config
  (setq nginx-indent-level 2)
  (add-hook 'nginx-mode-hook #'company-nginx-keywords))

;;
;; (defvar paredit-everywhere-mode-map
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "C-)") 'paredit-forward-slurp-sexp)
;;     (define-key m (kbd "C-}") 'paredit-forward-barf-sexp)
;;     (define-key m (kbd "M-(") 'paredit-wrap-round)
;;     (define-key m (kbd "M-)") 'paredit-close-round-and-newline)
;;     (define-key m (kbd "M-]") 'paredit-close-square-and-newline)
;;     (define-key m (kbd "M-\"") 'paredit-meta-doublequote)
;;     (define-key m (kbd "M-S") 'paredit-split-sexp)
;;     (define-key m (kbd "M-J") 'paredit-join-sexps)
;;     (define-key m (kbd "M-s") 'paredit-splice-sexp)
;;     (define-key m (kbd "M-r") 'paredit-raise-sexp)
;;     (define-key m (kbd "M-DEL") 'paredit-backward-kill-word)
;;     (define-key m (kbd "M-d") 'paredit-forward-kill-word)
;;     m)
;;   "Keymap for `paredit-everywhere-mode'.")
;;
(use-package paredit-everywhere
  :diminish
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)

  ;; http://endlessparentheses.com/a-few-paredit-keys-that-take-over-the-world.html
  ;;
  (global-set-key (kbd "C-M-u") #'paredit-backward-up)
  (global-set-key (kbd "C-M-n") #'paredit-forward-up)
  ;; ;; This one's surpisingly useful for writing prose.
  ;; (global-set-key "\M-S"
  ;;   #'paredit-splice-sexp-killing-backward)
  (global-set-key "\M-R" #'paredit-raise-sexp)
  (global-set-key "\M-(" #'paredit-wrap-round)
  (global-set-key "\M-[" #'paredit-wrap-square)
  (global-set-key "\M-{" #'paredit-wrap-curly)
  )

;; It doesn't seem to like this, it thinks the domain name is neodarwin
;; 	url = git@github.com:spanishdict/neodarwin.git
;;
(use-package browse-at-remote
  :defer t
  :config
  (setq browse-at-remote-remote-type-domains '(("bitbucket.org" . "bitbucket")
                                              ("github.com" . "github")
                                              ("neodarwin" . "github")
                                              ("gitlab.com" . "gitlab")
                                              ("git.savannah.gnu.org" . "gnu")
                                              ("gist.github.com" . "gist"))))

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

;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below).
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y")
              (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))

(require 'appearance)

(message (concat "exec-path is " (format "%s" exec-path)))

(defvar initial-file (expand-file-name "init.el" user-emacs-directory))

(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

(defvar desktop-restore-eager 16)
(desktop-save-mode 1)

;; This is voodoo...
(-remove-item "/Users/william/scm/sd/hegemone/TAGS" tags-table-list)

(provide 'main)

;; TODO: am I handling safe-local-variable-values in a sensible way?
;; look at purcell, etc.

;; TODO: Byte-recompile site-lisp-dir during some idle time after startup.
;;(byte-recompile-directory site-lisp-dir 0)
;;
;; (defun wjb/run-once-when-idle (fun)
;;   "Run a command every once in a while, if possible when emacs is idle."
;;   (defun wjb/generate-idle-callback (fun)
;;     (defun ()
;;         (call fun)
;;       (remove-hook 'auto-save-hook thing)))
;;   ;; TODO: need unique name for thing
;;   (setq thing (wjb/generate-idle-callback fun))
;;   (add-hook 'auto-save-hook thing))

;; Experimental:
;; (add-to-list 'load-path "../elisp/emacs-libvterm/build")
;; (let (vterm-install)
;;   (require 'vterm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main.el ends here
