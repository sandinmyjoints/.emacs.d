;;; -*- lexical-binding: t no-byte-compile: t -*-
;;; main.el --- Emacs configuration.
;;
;; Filename: init.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Thu Oct  2 08:04:34 2014 (-0700)
;; Version:
;; Package-Requires: ((emacs "29.1"))
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

(setq wjb/using-company nil) ;; Temporarily disable company while testing corfu

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(setq load-prefer-newer t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(use-package compat)

(defvar is-mac (equal system-type 'darwin))
(defvar initial-file (expand-file-name "init.el" user-emacs-directory))
(defvar wjb/home-directory (getenv "HOME"))

;; TODO(mine)
(require 'sane-defaults)

(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

;; Require Common Lisp. (cl in <=24.2, cl-lib in >=24.3.)
;; TODO: can this be removed?
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


;; byte compiling / native-comp

;; Auto-compile elisp to bytecode. This should be as early as possible.
(use-package auto-compile
  :demand
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; TODO: Byte-recompile site-lisp-dir during some idle time after startup.

;; Byte-recompile by directory. 0 means compile even for files that do not
;; already have an elc file. t means recompile every el files that has an elc
;; file. I have some shell functions for these but they don't set load-path
;; correctly and so tend to not compile a bunch of files.
;;
;; (byte-recompile-directory site-lisp-dir 0 t) ;; /elisp
;; (byte-recompile-directory package-user-dir 0 t)
;;
;; HOWTO native-comp safely:
;; 0. rm all elc files (elc_rm)
;; 1. byte-recompile everything (see above). Note that some elisp files will not be byte compiled, mostly pkg files that have a no-byte-compile flag. These will show up as warnings when running native-comp.
;; 2. then run these:
;; (native-compile-async "~/.emacs.d/elisp" 'recursively)
;; (native-compile-async "~/.emacs.d/elpa" 'recursively)
;; check comp-async-compilation variable to see if it's running.
;; TODO: move to a script

(setq package-native-compile t) ;; t means compile packages on install.

;; see https://github.com/jrblevin/markdown-mode/issues/578#issuecomment-1126380098
(setq native-comp-deferred-compilation-deny-list '("markdown-mode"))
(setq native-comp-jit-compilation-deny-list '("markdown-mode\\.el$"))



;; Base packages.
;;
(use-package recentf)
(use-package diminish)
(use-package autorevert
  :defer 5
  :diminish auto-revert-mode)

(use-package simple
  :diminish auto-fill-function)

(use-package abbrev
  :defer 5
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Abbrevs.html#Editing-Abbrevs
  ;; (list-abbrevs)
  :diminish abbrev-mode
  :config
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook #'abbrev-mode)
  )

(require 'setup-package)

;; Lists.
(use-package dash)
;; Strings.
(use-package s)
;; Filesystem.
(use-package f)
;; Hashtables.
(use-package ht)
;; Alists.
(use-package asoc
  :load-path "elisp/asoc.el")

(use-package hi-lock
  :diminish)

(use-package adaptive-wrap
  :defer t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package pinentry
  :config
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package simple
  :config
  ;; TODO: only use these in modes where it makes sense. Org is not one of
  ;; them. From https://www.reddit.com/r/emacs/comments/e1uyvk/weekly_tipstricketc_thread/f93y0qg?utm_source=share&utm_medium=web2x
  ;;
  ;; I use them in org-pivotal mode because they prevent hard-wrap newlines
  ;; from ending up breaking lines in odd places when the tickets are uploaded
  ;; to PT.
  ;;
  ;; Do word wrapping at fill column in visual-line-mode.
  (remove-hook 'visual-line-mode-hook #'visual-fill-column-mode)

  ;; Preserve indents when wrapping lines in visual-line-mode.
  (remove-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package xref
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))


;; ========================================
;; Package management.
;; ========================================

;; Mine
(when is-mac (require 'setup-mac))
(require 'defuns)
(require 'setup-tramp)
(require 'mode-mappings)


;; Modeline

(defun wjb/doom-modeline-env-node ()
  (setq doom-modeline-env--command "node"
        doom-modeline-env--command-args '("-v" "2>&1")
        doom-modeline-env--parser (lambda (line) (car (split-string line)))))

(use-package nerd-icons)

;; Looks nice but updates frequently and takes CPU/leads to GCs
(use-package doom-modeline
  ;; :disabled
  :after nerd-icons
  :hook (after-init . doom-modeline-mode)
  :config
  (add-hook 'js-mode-hook #'wjb/doom-modeline-env-node)
  (setq doom-modeline-minor-modes t
        doom-modeline-continuous-word-count-modes '()
        doom-modeline-gnus nil
        doom-modeline-gnus-timer 0
        doom-modeline-buffer-file-name-style 'relative-to-project ;; or 'truncate-upto-root
        doom-modeline-buffer-encoding nil))

;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display))

(use-package smart-mode-line
  ;; :after minions
  :disabled
  :config
  ;; Helpful reading:
  ;; - https://github.com/lunaryorn/blog/blob/master/posts/make-your-emacs-mode-line-more-useful.md
  ;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html

  (setq wjb/mode-line-format-original (-copy mode-line-format))

  ;; (setq mode-line-format wjb/mode-line-format-original)
  ;; (setq-default mode-line-format wjb/mode-line-format-original)

  (add-to-list 'sml/replacer-regexp-list '("local_notes" ":LN:") t)

  ;; 'automatic works if :defer is used, but if :defer is used then minor modes
  ;; aren't reliably diminished...
  (setq sml/theme 'automatic
        sml/name-width 32
        mode-line-percent-position '(-3 "%o")
        mode-line-end-spaces " "
        sml/position-percentage-format "%o")

  (defun wjb/sml-after-setup-hook ()
    ;; Splice in virtualenv name and nvm.
    (setq mode-line-format
          (-insert-at 3 '(" " pyvenv-virtual-env-name " " (:eval (car nvm-current-version)) " ") mode-line-format))
    (setq-default mode-line-format mode-line-format))

  ;; try setting it as the global default
  (setq-default mode-line-format
                (-insert-at 3 '(" " pyvenv-virtual-env-name " " (:eval (car nvm-current-version)) " ") mode-line-format))

  ;; TODO why doesn't this run? Or does it, but it gets overwritten? Is minions messing with it somehow?
  ;; (add-hook 'sml/after-setup-hook #'wjb/sml-after-setup-hook)
  )

(use-package minions
  :config
  (setq minions-prominent-modes '(vterm-copy-mode))
  (minions-mode 1))


;; grep

;; TODO(mine)
(require 'setup-grep)

;; writable grep. Similar to wdired, which is a builtin package. Different
;; key-bindings, though.
(use-package wgrep
  :after grep
  :config
  (setq wgrep-enable-key "w"))

;; server

(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (message "Starting server...")
    (server-start)))

;; Used by magit, and probably others. Under the hood, it uses server.
(use-package with-editor
  :defer)

;; see https://www.emacswiki.org/emacs/Edit_with_Emacs
(use-package edit-server
  :disabled
  :commands (edit-server-start)
  :config
  (setq edit-server-new-frame nil)
  (defun wjb/save-edit-server () (kill-ring-save (point-min) (point-max)))
  (add-hook 'edit-server-done-hook #'wjb/save-edit-server)
  (add-hook 'edit-server-start-hook #'gfm-mode)
  (edit-server-start))

(use-package atomic-chrome
  :disabled
  :config
  (atomic-chrome-start-server))


;; prog and text modes

;; TODO(mine)
(require 'setup-word)
(require 'setup-markdown)

;; Generate a TOC from a markdown file: M-x markdown-toc-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x markdown-toc-generate-toc
(use-package markdown-toc
  :after markdown-mode)

(use-package prog-mode
  :bind (:map prog-mode-map
              ;; in text-mode, M-k is kill-sentence
              ("M-k" . kill-whole-line))
  :config
  (defun auto-fill-comments ()
    "Automatically fill comments, but nothing else"
    (setq-local comment-auto-fill-only-comments t)
    (setq truncate-lines nil))
  (add-hook 'prog-mode-hook #'auto-fill-comments)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Text and fill modes.
(use-package text-mode
  :bind (:map text-mode-map
              ;; In textual modes, C-M-n and C-M-p are bound to
              ;; forward-paragraph and backward-paragraph.
              ("C-M-n" . forward-paragraph)
              ("C-M-p" . backward-paragraph))
  :config
  (add-hook 'text-mode-hook #'wjb/hard-wrap-text)
  (add-hook 'text-mode-hook #'goto-address-mode)
  )

;; TODO: Use a second frame for:
;; - markdown live preview
;; - email composing/editing
;;
(use-package olivetti
  :disabled
  :config
  (defun wjb/olivetti ()
    "Turn on settings for writing prose."
    (interactive)
    (gfm-mode)
    (olivetti-mode))

  (setq-default olivetti-body-width 80)
  (add-hook 'olivetti-mode-hook #'wjb/soft-wrap-text))


;; long lines and large files

;; (global-so-long-mode)

(use-package vlf
  :disabled
  :defer 5
  ;; put this in vlf-setup.el, L104:
  ;; ((string-equal filename "TAGS")
  ;;  (let ((large-file-warning-threshold nil))
  ;;    (ad-do-it)))
  :config
  (require 'vlf-setup))


;; flycheck

(use-package flycheck
  :defer 5
  :init
  ;; This turns on Flycheck globally in only these modes. Others can be turned on
  ;; per-buffer.
  (defvar flycheck-global-modes
    '(js2-mode
      js2-jsx-mode
      typescript-mode
      typescript-ts-mode
      tsx-ts-mode
      web-mode
      rjsx-mode
      json-mode
      cc-mode
      coffee-mode
      css-mode
      less-css-mode
      sql-mode
      emacs-lisp-mode
      sh-mode
      yaml-mode
      python-mode
      python-ts-mode
      perl-mode
      perl6-mode))
  :config
  (setq flycheck-global-modes
    '(js2-mode
      js2-jsx-mode
      typescript-mode
      typescript-ts-mode
      tsx-ts-mode
      web-mode
      rjsx-mode
      json-mode
      cc-mode
      coffee-mode
      css-mode
      less-css-mode
      sql-mode
      emacs-lisp-mode
      sh-mode
      yaml-mode
      python-mode
      python-ts-mode
      perl-mode
      perl6-mode))

  ;; Most basic way: flycheck errors in minibuffer (works in consoles).
  ;; (unless (display-graphic-p (selected-frame))
  ;;   (with-eval-after-load 'flycheck
  ;;     (setq-default flycheck-display-errors-function 'flycheck-display-error-messages)))

  ;; for convenience, to turn off inline-mode:
  ;; (flycheck-inline-mode -1)

  (setq-default flycheck-display-errors-delay 0.4
                flycheck-idle-change-delay 0.6 ;; but this is really set below ↓
                ;; flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch)
                flycheck-disabled-checkers '(javascript-jshint html-tidy emacs-lisp-checkdoc)
                flycheck-temp-prefix ".flycheck"
                flycheck-navigation-minimum-level 'warning
                flycheck-error-list-minimum-level 'warning)
  (setq flycheck-eslint-args '("--no-color"))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (advice-add 'flycheck-jump-in-buffer :around
              (lambda (orig-fun &rest args)
                (let ((display-buffer-overriding-action '((display-buffer-same-window))))
                  (apply orig-fun args))))

  (require 'wjb-byte-compile)
  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)

  (defun replace-path-prefix (filepath old-prefix new-prefix)
    "Replace OLD-PREFIX in FILEPATH with NEW-PREFIX.
Returns FILEPATH unchanged if OLD-PREFIX is not a prefix of it."
    (if (string-prefix-p old-prefix filepath)
        (concat new-prefix (substring filepath (length old-prefix)))
      filepath))

  (flycheck-define-checker python-hegemone-pycodestyle
    "A Python syntax/style checker using a Docker command via invoke."
    :command ("docker" "run"
              "--entrypoint=/usr/bin/env"
              "--rm"
              "-v" "/Users/wbert/scm/sd/hegemone/sd_hegemone:/usr/src/app/sd_hegemone"
              "sd-hegemone"
              "/usr/src/app/.venv/bin/invoke"
              "pycodestyle"
              "--target"
              (eval
               (replace-path-prefix
                (buffer-file-name)
                "/Users/wbert/scm/sd/hegemone"
                "/usr/src/app"))
              )
    :error-patterns
    ((error   line-start (file-name) ":" line ":" column ": " "E" (id (one-or-more (not (any ": ")))) (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": " "W" (id (one-or-more (not (any ": ")))) (message) line-end))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'python-hegemone-pycodestyle)

  (flycheck-define-checker python-hegemone-pylint
    "A Python syntax/style checker using a Docker command via invoke."
    :command ("docker" "run"
              "--entrypoint=/usr/bin/env"
              "--rm"
              "-v" "/Users/wbert/scm/sd/hegemone/sd_hegemone:/usr/src/app/sd_hegemone"
              "sd-hegemone"
              "/usr/src/app/.venv/bin/pylint"
              "--output-format=text"
              (eval
               (replace-path-prefix
                (buffer-file-name)
                "/Users/wbert/scm/sd/hegemone"
                "/usr/src/app"))
              )
    :error-patterns ;; appropriate for very old pylint

    ;; ************* Module sd_hegemone.wordoftheday.admin
    ;;            sd_hegemone/wordoftheday/admin.py:241:0: R0901: Too many ancestors (8/7) (too-many-ancestors)
    ((info    line-start (file-name) ":" line ":" column ": C" (one-or-more (not (any ":"))) ": " (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": R" (one-or-more (not (any ":"))) ": " (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": W" (one-or-more (not (any ":"))) ": " (message) line-end)
     (error   line-start (file-name) ":" line ":" column ": E" (one-or-more (not (any ":"))) ": " (message) line-end)
     (error   line-start (file-name) ":" line ":" column ": F" (one-or-more (not (any ":"))) ": " (message) line-end))
    ;; :error-parser flycheck-parse-pylint ;; needs newer pylint I think?
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'python-hegemone-pylint)
  (flycheck-add-next-checker 'python-hegemone-pycodestyle 'python-hegemone-pylint)

  (flycheck-define-checker python-pycodestyle
    "A Python style guide checker using pycodestyle. See URL `https://pycodestyle.readthedocs.io/'."
    :command ("pycodestyle" source-inplace)
    :error-patterns
    ((error   line-start (file-name) ":" line ":" column ": " "E" (id (one-or-more (not (any ": ")))) (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": " "W" (id (one-or-more (not (any ": ")))) (message) line-end))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'python-pycodestyle)

  ;; see https://github.com/flycheck/flycheck/issues/186#issuecomment-32773904
  ;; (flycheck-add-next-checker 'python-pycompile 'python-flake8)
  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-pycompile 'python-pycodestyle)
  (flycheck-add-next-checker 'python-pycodestyle 'python-pylint)

  (flycheck-add-mode 'python-flake8 'python-ts-mode)
  (flycheck-add-mode 'python-pycompile 'python-ts-mode)
  (flycheck-add-mode 'python-pylint 'python-ts-mode)

  (push 'rustic-clippy flycheck-checkers)

  ;; configure javascript-tide checker to run after your default javascript checker.
  ;; too many typescript errors, and complains about missing definitions
  ;; files. And can it find anything that eslint can't?
  ;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide)
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide)

  ;; (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-to-list 'safe-local-variable-values '(flycheck-javascript-eslint-executable . "eslint_d"))

  ;; override handlebars so the predicate works for word-of-the-day. I have
  ;; commented out the flycheck definition of handlebars in flychec.el as a
  ;; temporary hack b/c there doesn't seem to be a way to delete a check once
  ;; it has been defined
  (flycheck-define-checker handlebars
  "A Handlebars syntax checker using the Handlebars compiler.

See URL `http://handlebarsjs.com/'."
  :command ("handlebars" "-i-")
  :standard-input t
  :error-patterns
  ((error line-start
          "Error: Parse error on line " line ":" (optional "\r") "\n"
          (zero-or-more not-newline) "\n" (zero-or-more not-newline) "\n"
          (message) line-end))
  :modes (handlebars-mode handlebars-sgml-mode web-mode)
  :predicate
  (lambda ()
    (if (eq major-mode 'web-mode)
        ;; Check if this is a handlebars file since web-mode does not store the
        ;; non-canonical engine name
        (let* ((regexp-alist (bound-and-true-p web-mode-engine-file-regexps))
               (pattern (cdr (assoc "handlebars" regexp-alist))))
          (or
           (string-equal (projectile-project-name) "word-of-the-day")
           (and pattern (buffer-file-name)
               (string-match-p pattern (buffer-file-name)))))
      t)))

  (global-flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

;; This works fine, but I prefer tooltips as long as they're working OK.
(use-package flycheck-inline
  :after flycheck
  :disabled
  :config
  (remove-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; B/c of https://github.com/pitkali/pos-tip/pull/17/files
;; This is also in /elpa, so monitor it for changes.
(use-package pos-tip
  :load-path "elisp/pos-tip")

(use-package flycheck-posframe
  :after flycheck
  :config
  ;; monkey patch for transparency
(defun flycheck-posframe-show-posframe (errors)
  "Display ERRORS, using posframe.el library."
  (posframe-hide flycheck-posframe-buffer)
  (when (and errors
             (not (run-hook-with-args-until-success 'flycheck-posframe-inhibit-functions)))
    (let ((poshandler (intern (format "posframe-poshandler-%s" flycheck-posframe-position))))
      (unless (functionp poshandler)
        (setq poshandler nil))
      (flycheck-posframe-check-position)
      (posframe-show
       flycheck-posframe-buffer
       :override-parameters '((alpha . 90))
       :string (flycheck-posframe-format-errors errors)
       :background-color (face-background 'flycheck-posframe-background-face nil t)
       :position (point)
       :internal-border-width flycheck-posframe-border-width
       :internal-border-color (face-foreground 'flycheck-posframe-border-face nil t)
       :poshandler poshandler
       :hidehandler #'flycheck-posframe-hidehandler))))

  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (flycheck-posframe-configure-pretty-defaults)

  (setq flycheck-posframe-position 'window-top-right-corner
        flycheck-posframe-border-width 1
        ;; flycheck-posframe-border-color
        )
  (setq flycheck-posframe-border-use-error-face t))

(use-package flycheck-status-emoji
  :disabled
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-status-emoji-mode))


;; eldoc

(use-package eldoc
  :diminish eldoc-mode
  :defer 5
  :config
  (diminish 'eldoc-mode)
  (eldoc-add-command 'wjb/forward-symbol)
  (eldoc-add-command 'wjb/backward-symbol)
  (eldoc-add-command 'move-beginning-of-line)
  (eldoc-add-command 'move-end-of-line)
  (eldoc-add-command 'symbol-overlay-jump-prev)
  (eldoc-add-command 'symbol-overlay-jump-next)
  (eldoc-add-command 'backward-word)
  (eldoc-add-command 'flycheck-next-error)
  (eldoc-add-command 'flycheck-previous-error)
  (eldoc-add-command 'jump-to-register)
  (eldoc-add-command 'smart-jump-back)
  (eldoc-add-command 'smart-jump-go)
  )

(use-package eldoc-box
  :after (eldoc)
  :hook (prog-mode . eldoc-box-hover-mode)
  :config
  (setq tide-always-show-documentation t)
  (setq eldoc-box-only-multi-line nil)
  ;; these must be integers -- floats turn into zero
  (setq eldoc-box-max-pixel-width (- (frame-pixel-width) 50)
        eldoc-box-max-pixel-height (round (* 0.5 (frame-pixel-height))))
  )

(use-package eldoc-mouse
  :load-path "elisp/eldoc-mouse"
  :hook (eglot-managed-mode . eldoc-mouse-mode))


;; dired
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired)

(use-package dired
  :commands (dired counsel-dired)
  :hook
  ((dired-mode . all-the-icons-dired-mode)
   (dired-mode . hl-line-mode))
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
  ;; (add-to-list 'dired-compress-files-alist '("\\.gz\\'" . "gzip -c %i > %o"))

  ;; problem with dired-async is it doesn't update open buffers when files are moved
  ;; (autoload 'dired-async-mode "dired-async.el" nil t)
  ;; (dired-async-mode -1)

  ;; bsd ls vs. gls: this is written for bsd, but gls is probably
  ;; better
  ;;
  (setq counsel-dired-listing-switches "-lahFG")
  (setq dired-listing-switches "-lahF"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash 'always
        dired-switches-in-mode-line 'as-is))

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode))

;; (use-package dired+
;;   :disabled
;;   :load-path "elisp"
;;   :after dired
;;   :config
;;   (require 'dired+)
;;   (unbind-key (kbd "C-o") dired-mode-map)
;;   (toggle-diredp-find-file-reuse-dir 1)
;;   (setq diredp-hide-details-propagate-flag t))


;; org

;; Org-mode.
;; (require 'org-install)
;; (eval-after-load 'org '(require 'setup-org))
;; Helpful: (org-reload)
;; C-c e -- export to slack
;; C-c C-, -- insert template (or wrap around region)
;;
(use-package org ;; why org not org-mode: https://emacs.stackexchange.com/q/17710
  :defer t
  :diminish visual-line-mode
  :init
  (defface org-checkbox-done-text
    '((t (:strike-through t :slant italic :weight light) ))
    "Face for the text part of a checked org-mode checkbox.")
  :config
  ;; HACK: redefine org-md-plain-text in order to drop ` from the chars to be
  ;; protected.
  (defun org-md-plain-text (text info)
    "Transcode a TEXT string into Markdown format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
    (when (plist-get info :with-smart-quotes)
      (setq text (org-export-activate-smart-quotes text :html info)))
    ;; The below series of replacements in `text' is order sensitive.
    ;; Protect `, *, _, and \ (wjb edit: dropped ` from this regex)
    (setq text (replace-regexp-in-string "[*_\\]" "\\\\\\&" text))
    ;; Protect ambiguous #.  This will protect # at the beginning of
    ;; a line, but not at the beginning of a paragraph.  See
    ;; `org-md-paragraph'.
    (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
    ;; Protect ambiguous !
    (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
    ;; Handle special strings, if required.
    (when (plist-get info :with-special-strings)
      (setq text (org-html-convert-special-strings text)))
    ;; Handle break preservation, if required.
    (when (plist-get info :preserve-breaks)
      (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
    ;; Return value.
    text)

  (setq org-export-with-sub-superscripts '{}
        org-special-ctrl-a/e 'reversed)

  ;; = as paired delimiter, which keeps syntactic parsing inside of it...
  (modify-syntax-entry ?\= "$$" org-mode-syntax-table)
  ;; ...as opposed to a string, which drops it
  ;; (modify-syntax-entry ?\= "\"" org-mode-syntax-table)
  (modify-syntax-entry ?~ "$$" org-mode-syntax-table)

  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table)
  ;; (modify-syntax-entry ?< "_" org-mode-syntax-table)
  ;; (modify-syntax-entry ?> "_" org-mode-syntax-table)

  (font-lock-add-keywords
   'org-mode
   ;; from https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
   ;; TODO: it is striking through the newline at the end of the line
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  (setq org-src-fontify-natively t
        org-directory "~/notes"
        org-startup-folded 'overview
        org-agenda-files "~/.emacs.d/org-agenda-files-list.txt"
        org-catch-invisible-edits 'smart
        org-log-done t
        org-clock-persist 'history
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)) ;; can switch back to auto soon
        org-todo-keywords '((sequence "TODO" "ACTIVE" "|" "DONE" "INACTIVE"))
        org-outline-path-complete-in-steps nil
        org-completion-use-ido t
        org-replace-disputed-keys t
        org-edit-src-content-indentation 0
        org-catch-invisible-edits 'show
        org-fontify-done-headline t
        org-adapt-indentation t ;; trying this out
        org-return-follows-link t
        org-export-copy-to-kill-ring t)

  ;; TODO: org-slack-export-to-clipboard-as-slack-dwim that copies the current
  ;; entry, instead of having to mark it
  (define-key org-mode-map (kbd "C-c e") #'org-slack-export-to-clipboard-as-slack)

  ;; org-export-registered-backends is the variable that knows what backends exist
  ;; TODO: replace with copy-as-format
  (defun org-export-to-clipboard-as-md ()
    "Export region to Markdown, and copy to the kill ring for
pasting into other programs."
    (interactive)
    (let* ((org-export-with-toc nil)
           (org-export-with-smart-quotes nil))
      (kill-new (s-replace-all '(("\\_" . "_")) (org-export-as 'md) ))))

  (define-key org-mode-map (kbd "C-c m") #'org-export-to-clipboard-as-md)

  ;; what do these do?
  ;; (define-key global-map "\C-cl" 'org-store-link)
  ;; (define-key global-map "\C-ca" 'org-agenda)

  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)

  (defun wjb/org-mode-hook ()
    ;; (set-fill-column 80) ;; spams messages

    ;; Override some bindings in windmove map that were conflicting with org
    ;; bindings I use.
    (let ((oldmap (cdr (assoc 'windmove-mode minor-mode-map-alist)))
          (newmap (make-sparse-keymap)))
      (set-keymap-parent newmap oldmap)
      (define-key newmap (kbd "<S-up>") 'outline-previous-visible-heading)
      (define-key newmap (kbd "<S-down>") 'outline-next-visible-heading)
      (make-local-variable 'minor-mode-overriding-map-alist)
      (push `(windmove-mode . ,newmap) minor-mode-overriding-map-alist))

    (when (and wjb/using-company (boundp 'wjb/company-backends-org))
      (setq-local company-backends wjb/company-backends-org))
    ;; (setq-local completion-at-point-functions '(pcomplete-completions-at-point))

    ;; (hungry-delete-mode -1)
    (set-face-attribute 'org-headline-done nil :foreground nil)
    (set-face-attribute 'org-headline-done nil :inherit 'shadow)

    ;; reset it:
    ;; (setq-default prettify-symbols-alist '(("lambda" . 955)))

    ;; prettify-symbols-alist is buffer-local, so this only affects org-mode buffers
    (push '("[ ]" . "☐") prettify-symbols-alist) ;; ⚪
    (push '("[X]" . "☑") prettify-symbols-alist) ;; ⚫
    (push '("[-]" . "❆") prettify-symbols-alist) ;; ❍⮽🗳

    (push '("TODO" . ?□) prettify-symbols-alist) ;; ⬜
    (push '("ACTIVE" . ?⇒) prettify-symbols-alist) ;; TODO: something heavier, bolder
    (push '("INACTIVE" . ?❎) prettify-symbols-alist) ;; 🞖
    (push '("DONE" . ?■) prettify-symbols-alist) ;; ⬛
    (push '("ONGOING" . ?∞) prettify-symbols-alist) ;; ⏲⌛⏳⧗⧖∞⧜

    ;; not needed with fonts that support ligatures and have them for these
    ;; (e.g., Fira Code, DejaVu Code):
    ;;
    ;; (push '("->" . ?➔) prettify-symbols-alist)
    ;; (push '("=>" . ?⇒) prettify-symbols-alist)

    (when (boundp 'fci-mode)
      (fci-mode -1))
    )

  (add-hook 'org-mode-hook #'wjb/org-mode-hook t)

  ;; (global-set-key (kbd "H-c") #'org-capture) ;; clobbers js2r

  (require 'org-tempo))

(use-package ox-reveal
  :defer 5
  :after org)

(use-package org-src
  :after org
  :config
  (setq-default
   org-edit-src-content-indentation 0
   org-edit-src-persistent-message t
   org-src-tab-acts-natively t
   org-export-with-sub-superscripts '{}
   ;; TODO try this out:
   ;; org-src-window-setup 'reorganize-frame
   org-src-window-setup 'current-window)
  ;; Some initial languages we want org-babel to support
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (shell . t)
     (python . t)
     (sql . t)
     ;; (ein . t)
     (restclient . t))))

(use-package ob
  :disabled
  :after org
  :config
  ;; see https://github.com/arnm/ob-mermaid/blob/master/README.org
  (setq ob-mermaid-cli-path (home-subdir ".nvm/versions/node/v14.17.5/bin/mmdc"))
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (string= lang "sql-mode")))))

;; alternative Org Babel backend for SQL:
;; https://github.com/nikclayton/ob-sql-mode
(use-package ob-sql-mode
  :disabled
  :after ob
  :config
  ;; HACK: remove ("Q" "#+BEGIN_SRC sql-mode ?\n\n#+END_SRC" "#+BEGIN_SRC
  ;; sql-mode ?\n\n#+END_SRC") from list because it doesn't fit new format.
  ;; ob-sql-mode needs to be updated to work with org-tempo.
  (setq org-structure-template-alist
        '(
          ("a" . "export ascii")
          ("c" . "center")
          ("C" . "comment")
          ("e" . "example")
          ("E" . "export")
          ("h" . "export html")
          ("l" . "export latex")
          ("q" . "quote")
          ("s" . "src")
          ("v" . "verse")))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("Q" . "src sql-mode"))
  )

(use-package org-table
  :after org
  :config
  (setq org-html-table-row-tags
        (cons '(cond (top-row-p "<tr class=\"tr-top\">")
                     (bottom-row-p "<tr class=\"tr-bottom\">")
                     (t (if (= (mod row-number 2) 1)
                            "<tr class=\"tr-odd\">"
                          "<tr class=\"tr-even\">")))
              "</tr>"))

  (defun wjb/cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))
      ))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'wjb/cleanup-org-tables  nil 'make-it-local)))

  ;; Source: http://emacs.stackexchange.com/a/5319/2163
  (defun orgtbl-to-gfm (table params)
    "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
    (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                 org-table-last-alignment ""))
           (params2
            (list
             :splice t
             :hline (concat alignment "|")
             :lstart "| " :lend " |" :sep " | ")))
      (orgtbl-to-generic table (org-combine-plists params2 params))))

  (defun insert-org-to-md-table (table-name)
    (interactive "*sEnter table name: ")
    (insert "<!---
#+ORGTBL: SEND " table-name " orgtbl-to-gfm

-->
<!--- BEGIN RECEIVE ORGTBL " table-name " -->
<!--- END RECEIVE ORGTBL " table-name " -->")
    (forward-line -1)
    (forward-line -1)
    (forward-line -1)))

(use-package org-pivotal
  :load-path "elisp/org-pivotal"
  :config
  (defun wjb/create-todo-for-story-id (story-id)
    "Pull a given story and create an org TODO item for it."
    (interactive "MID: ")
    (funcall (-compose 'wjb/create-todo-for-story
                       'org-pivotal-api--fetch-story)
             "1967621" ;; engage
             story-id))

  (defun wjb/create-todo-for-story (story)
    "Create a new todo item for STORY."
    (progn
      (call-interactively 'org-return-and-maybe-indent)
      (let ((todo
             (format "\n* TODO %s %s - %s"
                     (s-capitalized-words (alist-get 'story_type story))
                     (alist-get 'id story)
                     (alist-get 'name story))))
        (insert todo))))
)


;; jira
(setq org-link-abbrev-alist
      '(("jira" . "https://ixl-learning.atlassian.net/browse/%s")
        ("jira?" . "https://ixl-learning.atlassian.net/secure/QuickSearch.jspa?searchString=%s")
        ))

;; How to search among org files:
;; - helm-org-agenda-files-headings -- search headings among org agenda files
;; - helm-org-rifle -- searches among headings and content
;;   - helm-org-rifle-agenda-files -- helm results, can be slow. C-0
;;   - helm-org-rifle-org-directory
;;   - helm-org-rifle-occur-agenda-files -- occur results (persistent buffer)
;;   - helm-org-rifle-occur-agenda-directory
;;
(use-package helm-org-rifle
  :after helm
  :commands helm-org-rifle-agenda-files
  :init
  (global-set-key (kbd "C-0") #'helm-org-rifle-agenda-files)
  :config
  (setq helm-org-rifle-show-path t)

  (defun helm-org-rifle-show-entry-in-real-buffer (candidate)
    "Show CANDIDATE in its real buffer. Modified: see https://github.com/alphapapa/helm-org-rifle/issues/22"
    (helm-attrset 'new-buffer nil)  ; Prevent the buffer from being cleaned up
    (-let (((buffer . pos) candidate))
      (switch-to-buffer buffer)
      (goto-char pos))
    (org-show-children)))

(use-package ox-odt
  :defer 5
  :after org)
(use-package ox-gfm
  :defer 5
  :after org
  :config
  ;; This is a convenience command. Same thing can accomplished with
  ;; org-export-dispatch, then choosing GFM, then temp buffer. This is basically
  ;; the same as org-gfm-export-as-markdown.
  (defun org-export-to-gfm-temp-buffer ()
    "Export current org buffer to GitHub Flavored Markdown in a temporary buffer."
    (interactive)
    (require 'ox-gfm)
    (let ((temp-buffer (generate-new-buffer "*Org GFM Export*")))
      (org-export-to-buffer 'gfm temp-buffer)
      (switch-to-buffer temp-buffer)
      (markdown-mode)
      (mark-whole-buffer)
      (easy-kill)))
  )

(use-package ox-slack
  :defer 5
  :after org)

(use-package ox-pandoc
  :config
  (setq org-pandoc-options-for-gfm '((wrap . preserve)))
  (defun wjb/post-gfm-hook ()
    (copy-to-register ?x (point-min) (point-max)))
  (add-hook 'org-pandoc-after-processing-gfm-hook #'wjb/post-gfm-hook)
  )



;; sql

(use-package sql
  :defer
  :after page-break-lines
  :config
  (push 'sql-mode page-break-lines-modes)

  (setq-default sql-input-ring-file-name
                (expand-file-name ".sqli_history" user-emacs-directory)
                sql-product 'mysql)

  (add-to-list 'sql-mysql-login-params '(port :default 3311))
  (setq sql-mysql-login-params
        '((user :default "sd")
          (database :default "sd_prod")
          (server :default "127.0.0.1")
          ))
  (setq sql-connection-alist
        '((dev (sql-product 'mysql)
               (sql-port 3311)
               (sql-server "127.0.0.1")
               (sql-user "sd")
               (sql-password "sd_password")
               (sql-database "sd_prod"))))

  (defun sanityinc/pop-to-sqli-buffer ()
    "Switch to the corresponding sqli buffer."
    (interactive)
    (if (and sql-buffer (buffer-live-p sql-buffer))
        (progn
          (pop-to-buffer sql-buffer)
          (goto-char (point-max)))
      (sql-set-sqli-buffer)
      (when sql-buffer
        (sanityinc/pop-to-sqli-buffer))))
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)

  ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
  (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
    (unless (eq 'oracle sql-product)
      (sql-product-font-lock nil nil)))
  (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode))

(use-package sqlformat
  :after sql
  :config
  ;; needs sqlparse package, which can be gotten with homebrew
  (setq sqlformat-command 'sqlformat)
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode) ;; this was getting annoying
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat-buffer))


;; git and magit

(defun wjb/smart-magit-status ()
  (interactive)
  (if (window-dedicated-p (get-buffer-window))
      (progn
        (other-window 1)
        (call-interactively #'magit-status))
    (call-interactively #'magit-status)))

;; Magit Forge uses this.
(use-package ghub)

(use-package transient
  :config
  (setq transient-highlight-mismatched-keys t))

;; Magit.
(use-package magit
  :bind (("C-x g" . wjb/smart-magit-status))
  :config
  (setq ghub-use-workaround-for-emacs-bug t
        magit-last-seen-setup-instructions "1.4.0"
        magit-diff-auto-show '(stage-all log-oneline log-follow log-select blame-follow)
        magit-status-expand-stashes nil
        magit-commit-show-diff nil
        magit-revert-buffers 1 ;; important for not slowing down everything
        magit-completing-read-function 'ivy-completing-read ;; magit does its own completing read, so I could try skipping this.
        magit-push-always-verify nil
        magit-revision-insert-related-refs nil
        magit-branch-read-upstream-first nil
        ;; experimental, see https://magit.vc/manual/magit/The-Branch-Popup.html
        magit-branch-prefer-remote-upstream '(master)
        magit-process-connection-type nil)
  (setq magit-credential-cache-daemon-socket ; location of credential socket
        (if (getenv "XDG_CACHE_HOME")
            (expand-file-name "git/credential/socket"
                              (getenv "XDG_CACHE_HOME"))
          (expand-file-name ".cache/git/credential/socket" (getenv "HOME"))))
        (autoload 'magit-log "magit"))

;; Experiment, might want to do this for everything:
(use-package setup-magit
  :after magit)

;; by the author of magit
(use-package forge
  ;; :disabled
  :after magit
  :config
  ;; HACK to get it to stop completing, see if that fixes hangs
  ;; (defun forge-topic-completion-at-point () nil)
  (setq forge-topic-list-limit '(30 . -10))
  )

;; separate from magit, but integrates: "You can use github-review with forge.
;; When your cursor is over a pull request, you can call
;; github-review-forge-pr-at-point to start a code review."
(use-package github-review
  :after magit
  :disabled
  :config
  (setq github-review-fetch-top-level-and-review-comments t))

(use-package gitignore-mode
  :mode ("\\.dockerignore\\'" "\\.aiderignore\\'" "global.gitignore" ".*gitignore\\'"))

;; It doesn't seem to like this, it thinks the domain name is neodarwin
;; 	url = git@github.com:spanishdict/neodarwin.git
;;
(use-package browse-at-remote
  :commands (browse-at-remote browse-at-remote-kill)
  :config
  (setq browse-at-remote-remote-type-domains '(("bitbucket.org" . "bitbucket")
                                               ("github.com" . "github")
                                               ("neodarwin" . "github")
                                               ("gitlab.com" . "gitlab")
                                               ("git.savannah.gnu.org" . "gnu")
                                               ("gist.github.com" . "gist")))
  ;; HACK: use completing-read (which is ivy-read) instead of read-string so
  ;; that the prompt shows up in the usual place instead of the minibuffer.
  ;; It'd be nice to supply a list of remote branches, but the lib doesn't
  ;; have that. EDIT OK, using magit instead to get a list of remote branches.
  (defun browse-at-remote--get-remote-branch (local-branch)
    "If LOCAL-BRANCH is tracking a remote branch, return
\(REMOTE-NAME . REMOTE-BRANCH-NAME). Returns nil otherwise."
    (let ((remote-and-branch
           ;; Try pushRemote first
           (let ((push-remote (vc-git--run-command-string
                               nil "config"
                               (format "branch.%s.pushRemote" local-branch))))
             (if push-remote
                 (format "%s/%s" (s-trim push-remote) local-branch)

               ;; If there's no pushRemote, fall back to upstream
               (vc-git--run-command-string
                nil "rev-parse"
                "--symbolic-full-name"
                "--abbrev-ref"
                (format "%s@{upstream}" local-branch))
               ))))
      ;; `remote-and-branch' is of the form "origin/master"
      (if remote-and-branch
          ;; Split into two-item list, then convert to a pair.
          (apply #'cons
                 (s-split-up-to "/" (s-trim remote-and-branch) 1))

        ;; Ask user if worst case (TODO: replace with competing-read here)
        ;; (let ((remote-branch (completing-read "Select remote branch: " (magit-list-remote-branch-names))))
        (let ((remote-branch (magit-read-remote-branch "Select remote branch" "origin" "master")))
          (cons (car (browse-at-remote--get-remotes)) remote-branch)))))
  )


;; python

(use-package python
  :mode ("\\.py\\'" . python-mode)
  ;; bind M-q python-fill-paragraph
  :bind (:map python-mode-map
              ("M-q" . python-fill-paragraph))
  :config
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-offset 2)
  (setq-default python-fill-docstring-style 'django)

  ;; Disabling this as an experiment:
  (remove-hook 'python-mode-hook (lambda ()
                                (hack-local-variables)
                                (setq fill-column 79) ;; get rid of this?
                                ;; (set-face-background 'highlight-indentation-face "#111")
                                ;; (pyvenv-tracking-mode) ;; slows cursor down a lot
                                (when (boundp 'project-venv-name)
                                  (venv-workon project-venv-name)
                                  (pyvenv-workon project-venv-name))))

  (defadvice run-python (before setup-repl ())
    "Use IPython if available."
    (if (executable-find "ipython")
        (setq
         python-shell-interpreter "ipython"
         python-shell-interpreter-args "-i --no-banner --gui=osx"
         python-shell-prompt-regexp "In \\[[0-9]+\\]: "
         python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
         python-shell-completion-setup-code
         "from IPython.core.completerlib import module_completion"
         python-shell-completion-module-string-code
         "';'.join(module_completion('''%s'''))\n"
         python-shell-completion-string-code
         "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
      (setq python-shell-interpreter "python"
            python-shell-interpreter-args "-i"
            python-shell-prompt-regexp ">>> "
            python-shell-prompt-output-regexp ""
            python-shell-completion-setup-code
            "try:\n    import readline\nexcept ImportError:\n    def __COMPLETER_all_completions(text): []\nelse:\n    import rlcompleter\n    readline.set_completer(rlcompleter.Completer().complete)\n    def __COMPLETER_all_completions(text):\n        import sys\n        completions = []\n        try:\n            i = 0\n            while True:\n                res = readline.get_completer()(text, i)\n                if not res: break\n                i += 1\n                completions.append(res)\n        except NameError:\n            pass\n        return completions"
            python-shell-completion-module-string-code ""
            python-shell-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))"
       )))
  (ad-activate 'run-python)
  )

;; use elpy-config to check on things
;; use for integration with pipenv: https://github.com/jorgenschaefer/elpy/issues/1217
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (defun wjb/elpy-hook ()
    "From https://elpy.readthedocs.io/en/latest/customization_tips.html?highlight=black#auto-format-code-on-save"
    (elpy-shell-set-local-shell (elpy-project-root))
    (add-hook 'before-save-hook
              'elpy-black-fix-code nil t))
  (add-hook 'elpy-mode-hook #'wjb/elpy-hook)

  (setq elpy-modules (-remove-item 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (-remove-item 'elpy-module-highlight-indentation elpy-modules)))

;; This is https://github.com/jorgenschaefer/pyvenv
;; - pyvenv-* commands
;; - comes with elpy
;;
(use-package pyvenv
  :config
  ;; (setenv "WORKON_HOME" (expand-file-name "~/.local/share/virtualenvs")) ;; this should be unnecessary b/c of exec-path-from-shell
  (setq pyvenv-menu nil)
  :hook (python-base-mode . pyvenv-mode))

(use-package pip-requirements
  :mode
  "requirements\\.txt"
  "requirements\\.*\\.txt")

(when nil
  ;; This is https://github.com/porterjamesj/virtualenvwrapper.el
  ;; - venv-* commands.
  ;; - TODO: might get rid of virtualenvwrapper.el now that using elpy.
  (use-package virtualenvwrapper)

  ;; To use, put the following into custom.el:
  (setq venv-location "~/.local/share/virtualenvs/")

  ;; if you want interactive shell support
  ;; (venv-initialize-interactive-shells) ;; broken

  ;; if you want eshell support
  ;;(venv-initialize-eshell)
  )

(use-package smart-dash
  :disabled
  :hook (python-mode . smart-dash-mode))

(use-package ein
  :after python
  :disabled
  :bind
  (:map ein:notebooklist-mode-map
        ("C-c C-g" . 'ein:notebooklist-open))
  (:map ein:notebook-mode-map
        ("C-c C-g" . 'ein:notebooklist-open)))

(use-package reformatter
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-"))

  (reformatter-define black-format
    :program "black"
    :args '("-"))

  (reformatter-define black-hegemone
    ;; docker run --entrypoint=/usr/bin/env --rm -v ${SD_HEGEMONE_VOLUME} sd-hegemone sh -c "/usr/src/app/.venv/bin/black
    :program "/Users/wbert/scm/sd/hegemone/.venv/bin/black"
    :args '("-")))


;; projectile

(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; (define-key projectile-mode-map (kbd "H-w") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "H-o") 'projectile-command-map)
  ;; (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (require 'setup-projectile))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
  (counsel-projectile-mode))

(use-package counsel-css
  :after (css counsel)
  :config
  (add-hook 'css-mode-hook 'counsel-css-imenu-setup))


;; ibuffer.

(autoload 'ibuffer "ibuffer" "List buffers." t)

(defun wjb/bury-ibuffer (orig-fun &rest args)
  "Never want to switch back to *Ibuffer* after choosing a buffer from it."
  (bury-buffer "*Ibuffer*")
  (apply orig-fun args))

(advice-add 'ibuffer-visit-buffer :around #'wjb/bury-ibuffer)


;; imenu

;; (defadvice ido-imenu (before push-mark activate)
;;   (push-mark))

;; Always rescan buffer for imenu. Turn off locally with dir-locals.
(set-default 'imenu-auto-rescan t)

;; manual rescan.
(defun imenu-rescan ()
  (interactive)
  (imenu--menubar-select imenu--rescan-item))

(use-package imenu-anywhere
  :bind (("C-." . ivy-imenu-anywhere)))

(use-package imenu-list
  :config
  (setq imenu-list-size 0.5)
  ;; TODO how to start collapsed?
  )


;; flx/amx/smex/prescient

;; (use-package ido
;;   :disabled
;;   :config
;;   (require 'setup-ido))

;; use flx b/c ivy uses it for sorting ivy--regex-fuzzy.
;; disabling b/c it may prevent ivy (and others?) from using prescient.
(use-package flx
  :disabled)

;; Use for ordering of commands in counsel-M-x.
(use-package amx
  :disabled)

(use-package smex
  :disabled
  :bind (("M-X" . smex-major-mode-commands)
         ;; ("M-x" . smex)
         )
  :config
  (smex-initialize)
  (setq smex-auto-update nil)
  (smex-auto-update 10)
  (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command))

;; I would expect prescient to remember history based on what I typed and then
;; chose, but it doesn't seem to. For example, for counsel-M-x, I type "eval".
;; "eval-defun" is top result. But I select "eval-region". "eval-region" is at
;; the top when I hit M-x again, but when I type "eval", "eval-defun" is still
;; the top result. It is not remembering that when I type "eval", I choose
;; "eval-region".
(use-package prescient
  :config
  ;; (add-to-list 'ivy-sort-functions-alist '(counsel-projectile-sort-projects . ivy-prescient-sort-function))
  ;; (setq ivy-prescient-sort-commands (butlast ivy-prescient-sort-commands))
  (setq ivy-prescient-sort-commands '(:not swiper swiper-isearch ivy-switch-buffer forge-checkout-pullreq))
  (prescient-persist-mode))

(use-package ivy-prescient
  :after (counsel ivy)
  :config
  ;; if this is t, then ivy-prescient-re-builder is set as the default case in
  ;; ivy-re-builders-alist, which requires spaces between tokens. So I
  ;; disabled it.
  (setq ivy-prescient-enable-filtering nil)
  (ivy-prescient-mode))


;; ivy/counsel/posframe

;; counsel-switch-buffer and magit-status, when run in a dedicated window
;; (dirtree), aren't useful. Switch to another (non-dedicated) window in such
;; cases.

(defun wjb/counsel-switch-buffer-other-window ()
  "Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window."
  (interactive)
  (ivy-read "Switch to buffer in other window: " 'internal-complete-buffer
            :preselect (buffer-name (window-buffer (window-in-direction 'right)))
            :action #'ivy--switch-buffer-other-window-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'counsel-switch-buffer-other-window
            :unwind #'counsel--switch-buffer-unwind
            :update-fn 'counsel--switch-buffer-update-fn))

(defun wjb/smart-counsel-switch-buffer ()
  (interactive)
  (if (window-dedicated-p (get-buffer-window))
      (call-interactively #'counsel-switch-buffer-other-window)
    (call-interactively #'counsel-switch-buffer)))

;; switching/finding/opening/running things
;; - C-o = helm-mini -> buffers, recent files, bookmarks, more? (cf M-o)
;; - C-x b = switch buffer (among open buffers)
;;   - C-x C-b = was ibuffer, helm-buffers-list
;;   - switch buffer among buffers limited to current project?
;; - helm-mini limited to current project?-> M-o = helm-browse-project (cf C-o)
;; - M-x = commands to run
;; - C-c p f = find file in project
;; - C-x C-f = ido-find-file, which I like but it doesn't use posframe
;; - H-0 f = counsel-find-file, which I would prefer to be more like ido-find-file.
;;    - Enter on a dir should descend into it, not open the dir in dired. Have to hit TAB twice.
;;      - good solution: https://github.com/abo-abo/swiper/wiki/ido-style-folder-navigation
;;      - good solution: https://emacs.stackexchange.com/a/33706/2163 (using this one)
;;      - possible solution: https://emacs.stackexchange.com/a/45937/2163
;;      - other: https://github.com/abo-abo/swiper/issues/1333#issuecomment-436960474
;;    - dired-do-rename and copy and other functions are also bad with counsel-find-file.
;;    - https://github.com/jixiuf/ivy-dired-history
(use-package ivy
  :diminish
  :config
  ;; when counsel-M-x is open, maybe others, then C-g calls
  ;; minibuffer-keyboard-quit, by default. But I don't think I need what that
  ;; does, I just need keyboard-escape-quit. So I'll try using that.
  (define-key ivy-minibuffer-map "\C-g" 'keyboard-escape-quit)

  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer) ;; Use C-M-j to call ivy-immediate-done to create new buffer
  (global-set-key (kbd "C-x b") #'wjb/smart-counsel-switch-buffer) ;; giving this a try
  (global-set-key (kbd "H-0 f") 'counsel-find-file)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  ;; list of commands to be replaced with ivy/counsel: https://github.com/syl20bnr/spacemacs/issues/10237
  ;; (global-set-key (kbd "C-x d") 'ido-dired) ;
  (global-set-key (kbd "C-x d") 'counsel-dired)
  (global-set-key (kbd "C-x C-w") 'ido-write-file) ;; TODO: want to replace this with counsel
  ;; consider:
  ;; (global-set-key (kbd "C-s") 'swiper)
  ;; (global-set-key (kbd "C-x C-f") 'ido-find-file)
  ;;
  (setq ivy-height-alist '((counsel-evil-registers . 5)
                           (counsel-yank-pop . 10)
                           (counsel-git-log . 10)
                           (counsel--generic . 12)
                           (counsel-el . 12)))

  (defun ivy--regex-fuzzy-ignore-space (str)
    "Build a regex sequence from STR.
Insert .* between each char."
    (setq str (s-replace-all '((" " . "")) (ivy--trim-trailing-re str)))
    (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
        (prog1
            (concat (match-string 1 str)
                    (let ((lst (string-to-list (match-string 2 str))))
                      (apply #'concat
                             (cl-mapcar
                              #'concat
                              (cons "" (cdr (mapcar (lambda (c) (format "[^%c\n]*" c))
                                                    lst)))
                              (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                      lst))))
                    (match-string 3 str))
          (setq ivy--subexps (length (match-string 2 str))))
      str))
  ;; (ivy--regex-fuzzy-ignore-space "a b")


  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-height 18
        ivy-on-del-error-function 'ignore
        ivy-format-function 'ivy-format-function-arrow
        ivy-virtual-abbreviate 'abbreviate
        ivy-magic-tilde nil
        ivy-initial-inputs-alist nil

        ;; references on ivy-re-builders-alist:
        ;; - https://emacs.stackexchange.com/a/36748/2163
        ;; - https://oremacs.com/2016/01/06/ivy-flx/
        ;;
        ;; Possible choices: ivy--regex, regexp-quote, ivy--regex-plus,
        ;; ivy--regex-fuzzy, ivy--regex-ignore-order.
        ;;
        ;; * ivy--regex-ignore-order requires spaces between tokens.
        ;;
        ;; * ivy--regex-fuzzy does not, b/c it inserts .* between each
        ;; character, but it's order-sensitive so doesn't handle typos/fat
        ;; fingering. See also prescient. It's quite aggressive on matching,
        ;; so on long lists like all unicode characters, it doesn't work well.
        ;;
        ;; * I think what I want is ivy--regex-fuzzy-ignore-space, that pretends
        ;; spaces in input aren't there.

        ivy-re-builders-alist '((swiper . ivy--regex-ignore-order)
                                (swiper-isearch . ivy--regex-ignore-order)
                                (counsel-projectile-switch-project . ivy--regex-ignore-order)
                                (counsel-imenu . ivy--regex)
                                (counsel-unicode-char . ivy--regex-ignore-order)
                                ;; (ivy-switch-buffer . ivy--regex-fuzzy)
                                ;; (counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-fuzzy-ignore-space)))
  (ivy-mode 1))

(use-package counsel
  :after (ivy)
  ;; H-<space> would be better, but that goes to Alfred
  ;; :bind (("C-," . counsel-imenu))
  :config
  (setq counsel-find-file-at-point t
        counsel-preselect-current-file t
        counsel-yank-pop-preselect-last t)

  (ivy-configure 'counsel-M-x
    :initial-input ""
    :display-transformer-fn #'counsel-M-x-transformer)

  ;; from https://emacs.stackexchange.com/a/33706/2163
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
        (alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    ;; TODO: Would like to do this for find-alternate-file as well, what map is active in that?
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))

(use-package posframe
  :config
  ;; (setq posframe-arghandler #'wjb/posframe-arghandler)
  (setq posframe-arghandler #'posframe-arghandler-default)
  (defun wjb/posframe-arghandler (buffer-or-name arg-name value)
    ;; see
    ;; https://github.com/tumashu/posframe/blob/bfd2e55219e0911980f4ea97b5995ce8553dce60/posframe.el#L439
    ;; for a list of parameters
    (let ((info '(
                  :min-width 80
                  :min-height 10
                  :internal-border-width 2
                  ;; :internal-border-color "#000"
                  ;; :left-fringe 4
                  ;; :right-fringe 4
                  :line-spacing 0.05
                  :font "Fira Code-15")))
      (or (plist-get info arg-name) value))))

(use-package ivy-posframe
  :after ivy
  :config
  (defun posframe-poshandler-frame-above-center (info)
    "A custom posframe position handler."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (round (/ (- (plist-get info :parent-frame-height)
                       (plist-get info :posframe-height))
                    3))))

  (defun ivy-posframe-display-at-frame-above-center (str)
    (ivy-posframe--display str #'posframe-poshandler-frame-above-center))

  (setq ivy-posframe-min-width 80
        ivy-posframe-min-height 10
        ivy-truncate-lines nil ;; ensures full path is shown in prompts
        ;; ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-above-center))
        ;; for some reason this has to be changed to take effect
        ivy-posframe-border-width 2
        ivy-posframe-parameters
        '((left-fringe . 4)
          (right-fringe . 4)
          (line-spacing . 0.05)
          (alpha-background . 95)))

  (ivy-posframe-mode 1))

;; uses hydra, hydra-posframe, so has to go after they've been defined
;; TODO(mine)
(require 'services)



;; helm

(use-package helm
  :demand
  :config
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
   helm-ff-skip-boring-files t
   helm-buffer-max-length nil
   helm-buffer-skip-remote-checking t
   helm-buffers-end-truncated-string "…"
   helm-echo-input-in-header-line t
   helm-buffer--pretty-names '((dired-mode . "Dired")
                               (lisp-interaction-mode . "Lisp Inter")
                               (magit-process-mode . "Magit proc")
                               (rest-client-mode . "REST")
                               (shell-script-mode . "Shell"))
   ))
(use-package helm-lib)
(use-package helm-dired-recent-dirs
  :after (helm))
(use-package helm-imenu
  :after (helm)
  :bind (("C-," . helm-imenu)))
(use-package helm-buffers
  :after (helm)
  :bind (;; ("C-x C-b" . helm-buffers-list)
         ("C-x C-b" . helm-mini)
         ("C-o" . helm-mini)
         ("C-x C-o" . helm-mini))) ; Clobbers delete-blank-lines.
(use-package helm-ls-git
  :after (helm)
  :config
  ;; see https://github.com/emacs-helm/helm-ls-git/issues/68
  (delete '("/COMMIT_EDITMSG$" . helm-ls-git-commit-mode) auto-mode-alist)
  (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode) auto-mode-alist)

  (setq helm-ls-git-default-sources '(helm-source-ls-git-buffers
                                      helm-source-ls-git
                                      helm-source-ls-git-status
                                      helm-ls-git-create-branch-source)))
(use-package helm-files
  :after (helm)
  :bind (("C-M-o" . helm-browse-project)) ;; Clobbers split-line.
  :config
  ;; useful commands, but probably shouldn't be bound globally:
  ;; (global-set-key (kbd "C-'") 'helm-mark-all)
  ;; (global-set-key (kbd "C-\"") 'helm-ff-run-marked-files-in-dired)

  ;; TODO:
  ;; - would like to add a source of files in the current project, maybe even all files
  ;; - would like to add dirs as sources, like ~/notes, ~/notes/prof-dev/ Solution: https://stackoverflow.com/a/12708839/599258, also see https://www.reddit.com/r/emacs/comments/6ogkp3/how_to_add_more_source_to_helmfindfiles_or/
  ;; - helm-for-files, helm-for-files-preferred-list, helm-source-locate
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    ;; TODO: improve spotlight, see: https://github.com/syl20bnr/spacemacs/issues/3280
                                    ;; it doesn't order well, when query is myself.org, the result appears very low -- why?
                                    ;; alfred does this when you prefix search query with space
                                    helm-source-mac-spotlight ;; process name is mdfind-process. Does result highlighting.
                                    ;; helm-source-locate ;; process name is locate-process. Does not do result highlighting.
                                    helm-source-file-cache
                                    ;; helm-source-files-in-current-dir
                                    helm-source-dired-recent-dirs
                                    helm-source-buffer-not-found
                                    ))



  (when is-mac
    (setq helm-locate-fuzzy-match nil
          helm-locate-command "mdfind -name %s %s"))
  )

(use-package s3ed)


;; dumb-jump and smart-jump

(use-package dumb-jump
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))
  ;; C-M-g dumb-jump-go -- would like to use M-.
  ;; C-M-p dumb-jump-back -- M-,
  :config
  ;; (use-package semantic/symref/grep
  ;;   :config
  ;;   (add-to-list 'semantic-symref-filepattern-alist '(js2-mode "*.js" "*.jsx"))
  ;;   (add-to-list 'semantic-symref-filepattern-alist '(coffee-mode "*.coffee"))
  ;;   (add-to-list 'semantic-symref-filepattern-alist '(helpful-mode "*"))
  ;;   (add-to-list 'semantic-symref-filepattern-alist '(sql-mode "*.sql"))
  ;;   (add-to-list 'semantic-symref-filepattern-alist '(org-mode "*.org")))

  ;; (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-selector 'helm)
  (unbind-key "C-M-p" dumb-jump-mode-map)
  (setq dumb-jump-force-searcher 'rg)

  ;; to use dumb-jump with xref: (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

  ;; I think this is redundant because smart-jump uses dumb-jump as a fallback
  ;; (add-hook 'prog-mode-hook #'dumb-jump-mode)
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  )

(use-package smart-jump
  ;; trying not deferring so that keys are more likely to be set correctly in prog-mode
  ;; :defer t
  ;; :bind (("M-." . smart-jump-go)
  ;;        ("M-," . smart-jump-back))
  :config
  ;; this binds to M-. and M-, in prog-mode-map:
  (smart-jump-bind-jump-keys 'prog-mode)

  ;; give some room for my custom jumps
  (setq smart-jump-default-order-weight 5)

  ;; set up all the default registers
  (smart-jump-setup-default-registers)
   )


;; parens

(defun wjb/disable-show-paren-mode ()
  ;; See http://endlessparentheses.com/locally-configure-or-disable-show-paren-mode.html
  (setq-local show-paren-mode nil))

;; Highlight matching parentheses when point is on them.
;;
(use-package paren
  :config
  (setq show-paren-delay 0
        show-paren-style 'parenthesis
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-highlight-openparen t)
  ;; unneeded as of 29.1:
  ;; (add-hook 'magit-status-mode-hook #'wjb/disable-show-paren-mode)
  (show-paren-mode 1))

;; Dims parens in certain modes.
(use-package paren-face
  :config
  (add-to-list 'paren-face-modes 'js-mode 'js2-mode)
  (global-paren-face-mode))

(use-package elec-pair
  :config
  (defun my/electric-pair-conservative-inhibit (char)
    ;; (message (format "two back: %c  two back synax: %c  one back: %c  one back syntax: %c"
    ;;                  (char-before (1- (point))) (char-syntax (char-before (1- (point)))) (preceding-char) (char-syntax (preceding-char))))
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     (and (eq char (char-before))
	        (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)
     ;; Don't pair at the end of a word, unless parens.
     (and
      (eq (char-syntax (char-before (1- (point)))) ?w)
      (eq (preceding-char) char)
      (not (eq (char-syntax (preceding-char)) ?\()
           ))))

  (setq electric-pair-inhibit-predicate 'my/electric-pair-conservative-inhibit)

  (defun my/python-electric-pair-conservative-inhibit (char)
    ;; (message (format "two back: %c  two back synax: %c  one back: %c  one back syntax: %c"
    ;;                  (char-before (1- (point))) (char-syntax (char-before (1- (point)))) (preceding-char) (char-syntax (preceding-char))))
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     (and (eq char (char-before))
	        (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)
     ;; Don't pair at the end of a word, unless parens, or f-string.
     (and
      (eq (char-syntax (char-before (1- (point)))) ?w)
      (eq (preceding-char) char)
      (not (or
            (eq (char-syntax (preceding-char)) ?\()
            (eq (char-before (1- (point))) ?f)
            )))))

  (electric-pair-mode))

;; experimental -- try as replacement for electric-pair-mode, as doom uses it.
(use-package smartparens
  :diminish
  :init
  (require 'smartparens-config)
  ;; **Note**: Turn off electric-pair-mode if using smart-parens.
  ;; No thanks.
  ;; (smartparens-global-strict-mode)
  :config
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-max-prefix-length 25
        sp-max-pair-length 4
        sp-escape-quotes-after-insert nil)

  ;; (sp-use-smartparens-bindings) ;; even with this commented out, it still grabbed some bindings

  ;; No thanks.
  ;; (show-smartparens-global-mode)

  ;; (require 'setup-smartparens)
  )

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
  ;; TODO: update these so that if these commands don't work (signal error),
  ;; then fall back to something else, like next-defun and prev-defun.
  ;; Think of down as "into" and up as "out of"
  ;; Command           | default | notes
  ;; forward up/down   | n/d     | forward-down is very useful, think of it as forward-descend or forward-into
  ;; backwards up/down | u/p     |
  (global-set-key (kbd "C-M-n") #'paredit-forward-up)
  (global-set-key (kbd "C-M-d") #'paredit-forward-down)
  (global-set-key (kbd "C-M-u") #'paredit-backward-up) ;; shadows backward-up-list
  (global-set-key (kbd "C-M-p") #'paredit-backward-down)

  (global-set-key (kbd "C-(") #'paredit-backward-slurp-sexp) ;; matches C-)

  ;; ;; This one's surpisingly useful for writing prose.
  ;; (global-set-key "\M-S"
  ;;   #'paredit-splice-sexp-killing-backward)
  (global-set-key "\M-R" #'paredit-raise-sexp)
  (global-set-key "\M-(" #'paredit-wrap-round)
  (global-set-key "\M-[" #'paredit-wrap-square)
  (global-set-key "\M-{" #'paredit-wrap-curly))



;; highlighting

(defun wjb/turn-off-global-hl-line ()
  (global-hl-line-mode -1))
;; TODO use global-hl-line-mode everywhere except vterm buffers
;; (add-hook 'vterm-mode-hook #'wjb/turn-off-global-hl-line)

(defun wjb/set-highlight-indentation-current-column-face ()
  "Just a bit lighter than the background."
  (set-face-background 'highlight-indentation-current-column-face
                       (color-lighten-name
                        (face-attribute 'default :background) 15)))

;; Highlight the current column in indentation-sensitive languages. Required
;; by elpy. Just want 0.6.0 because later versions cause breakage with elpy, I
;; think.
(use-package highlight-indentation
  :commands highlight-indentation-current-column-mode
  :diminish highlight-indentation-current-column-mode
  :defer 5
  :disabled
  :config
  (require 'color)
  ;; (mapc (lambda (hook)
  ;;         (add-hook hook #'wjb/set-highlight-indentation-current-column-face)
  ;;         (add-hook hook 'highlight-indentation-current-column-mode))
  ;;       '(coffee-mode-hook
  ;;         yaml-mode-hook
  ;;         ;; python-mode-hook ;; let elpy set this up
  ;;         ;; web-mode-hook ;; breaks due to absence of web-mode-html-offset
  ;;         sass-mode-hook))
  )

(use-package highlight-indent-guides
  ;; :disabled ;; doesn't seem to work right
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  ;; (mapc (lambda (hook)
  ;;         (remove-hook hook 'highlight-indent-guides-mode))
  ;;       '(coffee-mode-hook
  ;;         python-mode-hook
  ;;         web-mode-hook
  ;;         sass-mode-hook))
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0.0))

(use-package highlight-thing
  :diminish
  :disabled
  :init
  ;; TODO
  ;; - ideal would be for it to be same face but bolded or slightly lighter
  ;; (color-lighten-name (face-name (face-at-point)) 10)
  ;; - ideal would be it only highlights the thing under point and none others
  ;; (put 'highlight-thing 'face-defface-spec nil)
  ;; - highlight word, symbol, sexp, defun, etc.
  (defface highlight-thing
    '((t
       ;; (:background "grey50")
       (color-lighten-name (face-name (face-at-point)) 10)
       (:weight bold)))
    "Face that is used to highlight things."
    :group 'highlight-thing)
  :config
  (global-highlight-thing-mode 1)
  (setq highlight-thing-delay-seconds 0
        highlight-thing-excluded-major-modes '(org-mode gitcommit-mode magit-status-mode text-mode gfm-mode)
        highlight-thing-limit-to-defun t))


;; treemacs

(use-package cfrs)

(use-package treemacs
  ;; :after (exec-path-from-shell)
  ;; :hook (emacs-startup . #'treemacs-find-file) ;; I manually add it to emacs-startup-hook so it can go after require appearance, before wjb/customize-appearance
  :config
  (treemacs-git-mode -1)
  (setq treemacs-collapse-dirs                 3
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 'simple
        treemacs-file-event-delay              2000
        treemacs-file-extension-regex          treemacs-last-period-regex-value
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-expand-after-init             t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         t
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil  ;; set to t and kill/reopen treemacs if laggy
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-read-string-input             'from-minibuffer ;; 'from-child-frame  ;; consider 'from-minibuffer if laggy
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    'on-distance
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-case-insensitive-asc
        treemacs-space-between-root-nodes      nil
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              0.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                         48
        treemacs-select-when-already-in-treemacs 'stay)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  (treemacs-resize-icons 18)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode nil)
  (set-face-attribute 'treemacs-root-face nil :height 1.0 :weight 'normal)

  (defun wjb/treemacs-hook ()
    ;; Preserve indents when wrapping lines in visual-line-mode.
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode nil t)
    (set-face-attribute 'treemacs-root-face nil :height 1.0 :underline nil)
    (setq-local cursor-type 'box)
    (with-selected-window (treemacs-get-local-window)
      (if (wjb/is-small-display) (treemacs--set-width 36) (treemacs--set-width 48))))
  (add-hook 'treemacs-mode-hook #'wjb/treemacs-hook)
  (add-hook 'treemacs-mode-hook 'hidden-mode-line-mode)
  ;; for this to work with visual fill, treemacs tags would need to be able
  ;; to handle wrapped lines
  ;; (add-hook 'treemacs-mode-hook #'visual-line-mode t)

  (treemacs-map-icons-with-auto-mode-alist
   '(".less")
     '(less-css-mode . (treemacs-get-icon-value "css")))

  ;; todo: consider conditionally adding node_modules
  (defun wjb/treemacs-ignore-compiled-files (filename filepath)
    (or
     (s-equals? (file-name-extension filename) "elc")
     (s-equals? (file-name-extension filename) "pyc")))
  (push #'wjb/treemacs-ignore-compiled-files treemacs-ignored-file-predicates)

  :bind
  (:map global-map
        ("H-a"       . treemacs-select-window)
        ("C-c d"     . treemacs-add-project-to-workspace)
        )
  (:map treemacs-mode-map
        ("e" . treemacs-TAB-action)
        ("j" . treemacs-next-neighbour)
        ("k" . treemacs-previous-neighbour))
  )

(use-package treemacs-projectile
  :after (treemacs projectile))

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :config (treemacs-icons-dired-mode))


;; auth / crypto / tls / security

;; EPG.
(use-package epa-file
  :defer t
  :config
  (epa-file-enable)
  (setenv "GPG_AGENT_INFO" nil))

(use-package password-cache
  :config
  (setq password-cache-expiry (* 15 60)))

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

;; TODO: defer setting these until some sensible time (first time will popup a
;; box for the password). For, now at startup they are set to nil and cached, so
;; clear the cache.
(defun wjb/init-auth-source ()
  (interactive)
  (setq auth-source-netrc-cache '())

  (setq pivotal-api-token
        (cadr (auth-source-user-and-password "api.pivotaltracker.com" "williambert"))

        org-pivotal-api-token
        (cadr (auth-source-user-and-password "api.pivotaltracker.com" "williambert"))

        paradox-github-token
        (cadr (auth-source-user-and-password "api.github.com" "sandinmyjoints^paradox"))))

;; TODO(emacs-mac): needed when not using emacs-mac
;; see:
;; - https://github.com/cask/cask/issues/418
;; - https://emacs.stackexchange.com/questions/18045/how-can-i-retrieve-an-https-url-on-mac-os-x-without-warnings-about-an-untrusted/18070#18070
(setq gnutls-log-level 0)
;; requires restart to take effect. supposedly doesn't help in recent versions of emacs.
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(with-eval-after-load 'gnutls
  ;; (add-to-list 'gnutls-trustfiles "/usr/local/etc/gnutls/cert.pem") ;; causes error: duplicate extension detected
  ;; (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")
  ;; (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
  )
(setq gnutls-trustfiles '("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/etc/ssl/cert.pem" "/etc/certs/ca-certificates.crt"))
(setq gnutls-trustfiles '("/usr/local/etc/openssl/cert.pem"))


;; Yasnippet.

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; Work-around for tab complaining when yas is active in ansi-term. See:
  ;; https://github.com/capitaomorte/yasnippet/issues/289
  (defun wjb/disable-yas-minor-mode ()
    (yas-minor-mode -1))
  (add-hook 'term-mode-hook #'wjb/disable-yas-minor-mode)

  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/es6-snippets/snippets" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/js-snippets" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/js-react-redux-yasnippets/snippets" t)
  ;; These are great snippets, but loading them is causing some warnings:
  ;; (eval-after-load 'yasnippet '(use-package emacs-snippets))

  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)
(use-package js-react-redux-yasnippets
  :after yasnippet)


;; docker

(use-package docker
  :defer 5
  :bind (:map wjb-map
              ("d" . docker))
  :config
  (setq docker-container-default-sort-key '("")
        docker-compose-command "sdc"))

(use-package dockerfile-mode
  :mode "Dockerfile"
  "Dockerfile-*\\'")

(use-package docker-compose-mode
  :mode "docker-compose*\\.yml")

;; Open a file on a running Docker container:
;;
;;     C-x C-f /docker:USER@CONTAINER:/path/to/file
(use-package tramp-container)


;; various modes

(use-package yaml-mode)

(use-package yaml-imenu
  :after yaml-mode)

(use-package css-mode
  :mode ("\\.css\\'")
  :config
  (setq css-indent-offset 2)
  (when wjb/using-company
    (defun wjb/css-mode-hook ()
      (setq company-backends wjb/company-backends-css))
    (add-hook 'css-mode-hook #'wjb/css-mode-hook))
  )

(use-package less-css-mode
  :mode ("\\.less\\'"))

(use-package elisp-mode
  :mode "abbrev_defs"
  :config
  (diminish 'lisp-interaction-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  ;; (add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 70)))
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header))

(use-package knot-mode
  :disabled
  :mode "\\.knot\\'")

;; TODO: set this after switching to a restclient buffer.
;; TODO: add key-binding to get to last restclient buffer.
(defvar wjb/last-restclient-buffer nil
  "The last restclient buffer.")

(use-package restclient
  :defer t
  :after page-break-lines
  :mode
  ("\\.rest\\'" . restclient-mode)
  ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify))
  :config
  (setq restclient-inhibit-cookies t)
  (push 'restclient-mode page-break-lines-modes)
  (make-variable-buffer-local 'url-max-redirections)

  ;; see https://github.com/joshwnj/json-mode/issues/28#issuecomment-363489644
  (add-hook 'restclient-response-loaded-hook
            (defun mad/js-to-cjson-mode ()
              (when (equal major-mode 'js-mode)
                (jsonc-mode)
                (when (fboundp 'tree-sitter-mode)
                  (tree-sitter-mode -1))
                (flycheck-mode -1)))))

(use-package kill-dollar-mode
  :hook ((org-mode . kill-dollar-mode)
         (markdown-mode . kill-dollar-mode)))

(use-package shell-script-mode
  :mode "\\.bash*")

(use-package conf-mode
  :mode "credentials$"
  "pylintrc"
  "ads\\.txt"
  "robots\\.txt"
  "\\.htaccess"
  "\\.curlrc"
  "\\.flake8"
  "\\..*rc\\'"
  "my.cnf"
  "Pipfile"
  )

(use-package nginx-mode
  :config
  (setq nginx-indent-level 2))

;; RVM.
(use-package rvm
  :defer
  :config
  (rvm-use-default)) ;; use rvm's default ruby for the current Emacs session

(use-package dotenv-mode
  :mode "\\.env\\'")

(use-package csv-mode
  :disabled
  :mode "\\.tsv\\'" "\\.csv\\'"
  :init
  (add-hook 'csv-mode-hook #'display-line-numbers-mode))


;; various packages

(autoload 'auto-make-header "header2")

(use-package comment-dwim-2
  ;; Default for builtin comment-line is C-x C-;
  :bind (("M-;" . comment-dwim-2)))

(use-package ediff
  :defer
  :init
  ;; this seems to work fine; no need to use the internal hook.
  (add-hook 'ediff-cleanup-hook 'winner-undo)
  ;; (remove-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :config
  (setq diff-switches "-u"
        ediff-diff-options "-w"
        ediff-custom-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package quickrun
  :disabled
  :config
  (defalias #'runthis #'quickrun))

(use-package date-at-point)

(use-package anzu
  :diminish anzu-mode
  :config
  (setq anzu-search-threshold 1200)
  (global-anzu-mode 1))

(use-package phi-search
  :disabled
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward))
  :config)

(use-package elisp-demos
  :disabled
  :config
  ;; this is for normal help:
  ;; (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; this is specific to helpful:
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package google-this
  :disabled
  ;; C-c / n|SPC|l
  :diminish google-this-mode
  :config
  (google-this-mode 1))

(use-package beginend
  :diminish
  :defer 5
  :config
  (beginend-global-mode)
  (diminish 'beginend-global-mode)
  (diminish 'beginend-prog-mode))

(use-package mwim
  :defer 1
  :commands (mwim-beginning mwim-end)
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key org-mode-map (kbd "C-a") #'org-beginning-of-line)

  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key text-mode-map (kbd "C-e") #'move-end-of-line)
  (define-key org-mode-map (kbd "C-e") #'move-end-of-line)
  ;; (global-set-key [remap move-beginning-of-line] #'mwim-beginning)
  ;; (global-set-key [remap move-end-of-line] #'mwim-end)
  )

(use-package know-your-http-well
  :defer 5)

(use-package dashboard
  :disabled
  :config
  (dashboard-setup-startup-hook))


;; helpful / info

(use-package help-mode
  :init
  (add-hook 'help-mode-hook 'visual-line-mode)
  :diminish visual-line-mode)

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable))
  :config
  (add-hook 'helpful-mode-hook 'visual-line-mode)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

;; between which-key and which-key-posframe, they are making typing and
;; navigation within buffers slow
;; this probably has a fix: https://github.com/justbur/emacs-which-key/issues/226
;; manual activation is probably the way to go: https://github.com/justbur/emacs-which-key#manual-activation
(use-package which-key
  :diminish
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right)
  (which-key-mode))

(use-package which-key-posframe
  :after (which-key posframe)
  :config
  (which-key-posframe-mode)
  (setq which-key-posframe-parameters '((line-spacing . 0.0))
        which-key-posframe-poshandler #'posframe-poshandler-frame-above-center))

(use-package discover
  :disabled
  :config
  (global-discover-mode 1))

(use-package lua
  :mode "\\.lua\\'"
  :interpreter "lua")

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))



;; ======================
;; Corfu / CAPF completion trial (replacing company temporarily)
;; ======================
(defvar wjb/using-corfu t)

;; Keep history so Corfu can sort by recent usage.
(use-package savehist
  :init (savehist-mode 1))

(use-package orderless
  :when wjb/using-corfu
  :init
  ;; Orderless for richer matching (company was fuzzy-ish via backends).
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; Keep file completion sane (orderless sometimes awkward for paths).
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :when wjb/using-corfu
  :init
  (global-corfu-mode)          ;; Enable Corfu globally.
  (corfu-history-mode)         ;; Persist per-session usage ordering.
  (corfu-popupinfo-mode)       ;; In-buffer docs (like company tooltip docs).
  :custom
  (corfu-auto t)               ;; Like company-idle completion.
  (corfu-auto-delay 0.2)       ;; ≈ company-idle-delay
  (corfu-auto-prefix 4)        ;; Global default (we lower to 3 in prog modes below).
  (corfu-cycle t)              ;; Wrap around.
  (corfu-preselect 'first)
  (corfu-scroll-margin 2)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.4 . 0.2)) ;; (show . hide)
  :config
  ;; Match old M-/ habit (was company-complete / company-other-backend).
  (global-set-key (kbd "M-/") #'completion-at-point)
  ;; Optional: TAB to move through candidates (comment out if it conflicts with indentation you rely on).
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "<tab>") #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") #'corfu-previous))

(use-package cape
  :when wjb/using-corfu
  :init
  ;; Helper to append a list of CAPFs without losing existing (LSP/Eglot/pcomplete/etc).
  (defun wjb/append-capfs (&rest fns)
    (dolist (fn fns)
      (add-to-list 'completion-at-point-functions fn t)))
  ;; Text-ish modes (Org/Markdown already set some CAPFs; we append gently).
  (defun wjb/cape-text-mode ()
              (wjb/append-capfs
               #'cape-dabbrev
               #'cape-abbrev
               #'cape-emoji
               (cape-company-to-capf #'company-files)
               #'cape-file))
  (add-hook 'text-mode-hook #'wjb/cape-text-mode)

  (defun wjb/cape-org-mode ()
              (wjb/append-capfs
               #'cape-dabbrev
               #'cape-file
               #'cape-emoji
               (cape-company-to-capf #'company-files)
               #'cape-file))
  (add-hook 'org-mode-hook #'wjb/cape-org-mode)

  ;; Programming modes: closer to previous company stacked backends.
  (add-hook 'prog-mode-hook
            (lambda ()
              (wjb/append-capfs
               #'cape-dabbrev
               #'cape-file
               #'cape-keyword)))

  ;; Match previous per-mode min prefix (company-minimum-prefix-length 3 in prog).
  (defun wjb/set-corfu-minimum-prefix-length ()
    (setq-local corfu-auto-prefix 3))
  (add-hook 'prog-mode-hook #'wjb/set-corfu-minimum-prefix-length)
  (add-hook 'restclient-mode-hook #'wjb/set-corfu-minimum-prefix-length)
  :config
  ;; Bridge company-only backends (e.g. tide) into CAPF if/when they load.
  (with-eval-after-load 'tide
    (add-hook 'tide-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions
                          (cape-company-to-capf #'company-tide) nil t)))))

;; Icons in the margin (rough analogue to richer company tooltips).
(use-package kind-icon
  :when wjb/using-corfu
  :after corfu
  :load-path "elisp/kind-icon"
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons (featurep 'nerd-icons))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Make TAB attempt completion after indent (nice with Corfu).
(setq tab-always-indent 'complete
      completion-cycle-threshold 3)

;; Preserve existing Org pcomplete override; Corfu will display its results.
;; (org-mode-hook already sets completion-at-point-functions for pcomplete.)

;; ======================
;; End Corfu trial block
;; ======================



;; web-mode

(use-package web-mode
  :load-path "elisp/web-mode"
  :mode "\\.html?\\'"
  "\\.hbs\\'"
  "\\.ejs\\'"
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-enable-current-element-highlight t)
  ;; TODO: web-mode-enable-auto-quoting only in html, not in JSX
  (setq web-mode-auto-close-style 2
        web-mode-auto-quote-style 1)    ;; uses double quotes

  (require 'setup-webmode)

  ;; experimental:
  (when wjb/using-company
    (require 'company-web-html)
    (require 'company-web-jade)
    (defun wjb/web-mode-company ()
      (set (make-local-variable 'company-backends)
           '((company-web-html :with company-dabbrev-code company-keywords)))
      (company-mode t))
    (add-hook 'web-mode-hook #'wjb/web-mode-company)))


;; folding

(use-package bicycle
  :disabled ;; couldnt get it to work
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-return] . bicycle-cycle)
              ;; ([S-tab] . bicycle-cycle-global)
              ))

;; better than vimish-fold
(use-package yafolding
  :config
  (add-hook 'prog-mode-hook #'yafolding-mode))

(use-package vimish-fold
  :disabled
  :config
  (vimish-fold-global-mode 0)
  ;; TODO: make this only true in vimish-fold key map
  (global-set-key (kbd "C-c `") #'vimish-fold-toggle)
  (global-set-key (kbd "C-c ~") #'vimish-fold))


;; rust

;; very helpful guide: https://robert.kra.hn/posts/rust-emacs-setup/
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  )

;; json

(use-package json-reformat
  :disabled)

;; jsons-print-path
(use-package json-snatcher
  :disabled)


;; js

;; Projects that use direnv files with layout: node don't actually need this.
(use-package nvm
  :config
  (nvm-use "v22.12.0")

  ;; HACK to avoid running this in magit buffers. Unclear if still needed.
  (defun nvm-use-for-buffer ()
    "Activate Node based on an .nvmrc for the current file.
If buffer is not visiting a file, do nothing."
    (when (or buffer-file-name (string-match "\`\*magit" (buffer-name)))
      (condition-case err
          (nvm-use-for buffer-file-name)
        (error (message "%s" err)))))

  (add-hook 'js-base-mode-hook #'nvm-use-for-buffer -99)
  (add-hook 'js2-mode-hook #'nvm-use-for-buffer -99)
  (add-hook 'js2-minor-mode-hook #'nvm-use-for-buffer -99)
  (add-hook 'yml-mode-hook #'nvm-use-for-buffer -99)
  (add-hook 'shell-script-mode-hook #'nvm-use-for-buffer -99)
  (add-hook 'projectile-after-switch-project-hook #'nvm-use-for-buffer -99)
)

(eval-when-compile (require 'cl))
(defvar preferred-javascript-indent-level 2)


;; Mode mappings
;; - old
;;   - js -> js2-mode.
;;   - ts-> typescript-ts-mode.
;;   - jsx -> rjsx-mode. not many left in neodarwin.
;;   - tsx -> tsx-ts-mode.
;;   - minified -> fundamental-mode.
;; - new
;;   - js -> js2-mode.
;;   - ts-> typescript-ts-mode. (or try jtsx-typescript-mode.)
;;   - jsx -> jtsx-mode.
;;   - tsx -> jtsx-mode.
;;   - minified -> fundamental.

(require 'setup-js-mode)
(require 'setup-ts-mode)
;; (require 'setup-rjsx-mode)
(require 'setup-jtsx-mode)
(require 'setup-eglot)
(require 'setup-jtsx-mode)

;; TODO ensure these come after flycheck, otherwise checkers may not be set up correctly
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'typescript-mode '(require 'setup-js2-mode))
(eval-after-load 'typescript-ts-mode '(require 'setup-js2-mode))

;; Prettier.
;;
(use-package prettier-js
  :hook ((js-base-mode . prettier-js-mode)
         (typescript-ts-base-mode . prettier-js-mode))
  :config
  (diminish 'prettier-js-mode)
  (setq prettier-js-width-mode 'fill)
  (setq-local prettier-js-args
        '("--single-quote"
          "--trailing-comma"
          "es5")))

(use-package indium
  :disabled
  :commands (indium-interaction-mode indium-connect)
  ;; :init
  ;; because indium-interaction-mode is in some dir-locals files so it will be
  ;; activated when those files load as part of the saved desktop.
  ;; (autoload 'indium-interaction-mode "indium-interaction-mode" nil t)
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode)
  (add-hook 'typescript-mode-hook #'indium-interaction-mode) ;; experimental
  (add-hook 'typescript-ts-mode-hook #'indium-interaction-mode)

  (setq indium-chrome-use-temporary-profile nil
        indium-client-debug nil ;; t
        ;; indium-chrome-executable (indium-chrome--default-executable)
        indium-chrome-executable "/Applications/Google Chrome Beta Debugger.app/Contents/MacOS/Google Chrome Beta Debugger")
  )

(use-package js-comint
  :disabled
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
  :config
  (require 'setup-coffee)
  (setq coffee-tab-width 2))


;; jest

;; unbind
;; (fmakunbound 'jest-minor-mode)
;; (makunbound 'jest-minor-mode)
;; (makunbound 'jest-minor-mode-map)

;; (makunbound 'jest-compile-command)
;; (fmakunbound 'jest-compile-command)
;; (makunbound 'jest-repeat-compile-command)
;; (fmakunbound 'jest-repeat-compile-command)

;; (makunbound 'jest-compile-function)

;; jest-mode is derived from comint:
;; (define-derived-mode jest-mode
;; comint-mode "jest"
;; "Major mode for jest sessions (derived from comint-mode)."
;; (compilation-setup t))
;;
;; - in buffers as specified by dir-locals (js, jsx, etc):  run jest-minor-mode so that compile and recompile run jest, but not g
;; - in *jest* buffers: run jest-mode. compile and recompile run jest, and g is bound to recompile.
;; - in *grep* buffers: nothing jest. g re-runs grep.
(use-package jest
  :after (js2-mode exec-path-from-shell)
  :hook ((js2-mode . jest-minor-mode)
         (typescript-ts-mode . jest-minor-mode)
         (tsx-ts-mode . jest-minor-mode)
         (jtsx-jsx-mode . jest-minor-mode)
         (jtsx-tsx-mode . jest-minor-mode)
         (jtsx-typescript-mode . jest-minor-mode)
         (typescript-mode . jest-minor-mode))
  :load-path "elisp/emacs-jest"
  :bind (
         :map jest-mode-map
         ([remap compile] . jest-popup)
         ([remap recompile] . jest-repeat)
         ;; ("u" . self-insert-command)
         ;; ("u". comint/send-custom-input)
         )
  :config
  (setq jest-pdb-track nil)
  (add-hook 'jest-mode-hook #'compilation-minor-mode)

  ;; compilation messes with Enter and regular keys. It binds Enter to
  ;; goto-error and unbinds some keys. C-q h comint-send-input works. I want to
  ;; disable compilation-minor-mode if at end of buffer. Or, I want to use Enter
  ;; for comint-send-input if at end of buffer. Or, see
  ;; https://endlessparentheses.com/provide-input-to-the-compilation-buffer.html,
  ;; which I am doing further down in this file (see endless/send-self).
  )

;; (package-generate-autoloads "jest" "~/.emacs.d/elisp/emacs-jest/")

;; Not sure which is preferable to use. shell-minor seems to not have
;; as many key bindings I want, however, it allows sending input into the
;; buffer.
;; (remove-hook 'jest-mode-hook #'compilation-shell-minor-mode)

;; (defun jest-minor-inhibit-self ()
;;   "Add this hook to modes that should not use jest-minor but otherwise would."
;;   (add-hook 'after-change-major-mode-hook
;;             (lambda () (jest-minor-mode 0))
;;             :append :local))

;; (add-hook 'grep-mode-hook 'jest-minor-inhibit-self)

;; (lambda ()
;;   (interactive)
;;   (call-interactively (symbol-value
;;                        'jest-compile-command)))

;; change it
;; (setq jest-compile-function #'jest-file-dwim)
;; (use-package jest-minor-mode
;;   :load-path "setup-lisp/jest-minor-mode"
;;   :after jest
;;   :hook js2-mode)


;; compilation / comint / shell

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

(defun endless/send-input (input &optional nl)
  "Send INPUT to the current process.
Interactively also sends a terminating newline."
  (interactive "MInput: \nd")
  (let ((string (concat input (if nl "\n"))))
    ;; This is just for visual feedback.
    (let ((inhibit-read-only t))
      (insert-before-markers string))
    ;; This is the important part.
    (process-send-string
     (get-buffer-process (current-buffer))
     string)))

;; shell is derived from comint: (define-derived-mode shell-mode comint-mode "Shell"
;; (use-package shell
;;   :after comint
;;   :config
;;   ;; Fix junk characters in shell-mode. This doesn't work to do ANSI color in
;;   ;; compilation mode, though. Maybe compilation mode doesn't use comint-mode, or
;;   ;; only sort of uses it?
;;   (add-hook 'shell-mode-hook
;;             'ansi-color-for-comint-mode-on)
;;   ;; want this for shell but not for compilation
;;   (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;   (setq comint-scroll-show-maximum-output nil)

;;   (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
;;   (define-key comint-mode-map (kbd "<down>") 'comint-next-input))

(defun endless/send-self ()
  "Send the pressed key to the current process."
  (interactive)
  (endless/send-input
   (apply #'string
          (append (this-command-keys-vector) nil))))

;; - Enter is for compile-goto-error, unless I want to rebind it to something else
;;   - Maybe I could somehow remap so that pressing C-j or C-m tells Jest to do Enter?
;; - p and t are for typing in patterns, so they don't work
;; - a w f o are for changing watch mode
;; - r is for re-running
;; - TODO don't echo the key I press
(dolist (key '("\C-d" "\C-j" "a" "w" "f" "o" "r" "u"))
  (define-key compilation-mode-map key
    #'endless/send-self)
  ;; I have jest set to run compilation-minor-mode, so this is the map that is active in my jest buffers:
  (define-key compilation-minor-mode-map key
    #'endless/send-self)
  ;; (define-key global-map key
  ;;   #'endless/send-self)
  )
;; (unbind-key "\C-c" compilation-minor-mode-map)

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

(defvar wjb/last-jest-buffer nil
  "The last jest buffer.")

;; based on https://github.com/bhollis/dotfiles/blob/86a1c854050a9ac1e5a205471802373328ee0b4f/emacs.d/init.el#L378
;; comint mode is interactive, compilation-shell-minor-mode runs compilation using shell-mode which is comint-mode, so it is interactive.
;; C-c RET starts compilation, keys don't do anything
;; C-u C-c RET starts comint with compilation-minor-mode, keys are sent -- BUT it's not doing anything for Jest, which probably isn't listening for input? maybe because terminfo is set to dumb? TODO: experiment with other values of TERM
;;
;; this actually works for sending keys to a comint compilation buffer, however,
;; you can't type in test name patterns, and there were some other problems too:
;; from http://endlessparentheses.com/provide-input-to-the-compilation-buffer.html
;;
(use-package compile
  :config
  (unbind-key (kbd "C-o") compilation-minor-mode-map)
  (unbind-key (kbd "C-o") compilation-mode-map)
  (setq compilation-scroll-output t
        ;; Set to "dumb" to not get color codes.
        ;; ansi and xterm-256colors both will get movement/scrolling codes from jest output, not just colors, and I think comint/shell-mode can't handle them
        ;; TODO: this could be set in dir-locals.
        comint-terminfo-terminal "dumb"
        ;; comint-terminfo-terminal "ansi"
        ;; comint-terminfo-terminal "xterm-256colors"
        ;;
        ;; https://stackoverflow.com/a/42467888/599258 talks about dumb-emacs-ansi
        ;; comint-terminfo-terminal "dumb-emacs-ansi"
        ;;
        ;; From https://unix.stackexchange.com/a/237947/14423:
        ;;
        ;; compilation-environment nil ;; default
        ;; This tells Jest (and other ncurses programs) they can use color codes
        ;; but not movement codes.
        ;; TODO: this could be set in dir-locals.
        ;; Related: node 12 changes for color detection and handling dumb terminals:
        ;; - https://github.com/nodejs/node/pull/26261
        ;; - https://github.com/nodejs/node/pull/26247
        ;; - https://github.com/nodejs/node/pull/26485
        ;;   - adds FORCE_COLOR and NO_COLOR
        ;;   - very helpful test suite: https://github.com/nodejs/node/blob/master/test/pseudo-tty/test-tty-color-support.js
        ;;   - tty implementation: https://github.com/nodejs/node/blob/master/lib/internal/tty.js
        ;; - interesting: https://no-color.org
        ;; - https://linux.die.net/man/1/dircolors
        compilation-environment '("TERM=dumb" "COLORTERM=1")

        comint-prompt-read-only nil
        comint-scroll-to-bottom-on-input t
        compilation-ask-about-save nil
        comint-prompt-regexp "^" ;; default
        comint-use-prompt-regexp nil ;; default
        ;; Don't save *anything*
        compilation-save-buffers-predicate '(lambda () nil)
        ;; compilation-scroll-output 'first-error
        )
  (make-variable-buffer-local 'comint-prompt-read-only)
  (setq-default comint-prompt-read-only nil)

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

  ;; vitest:

  ;;  FAIL  test/controllers/document-transcription.test.js > POST /document-transcription > should 200 for multipage PDF <= 30 pages
  ;; Error: expected 200 "OK", got 500 "Internal Server Error"
  ;;  ❯ test/controllers/document-transcription.test.js:480:8

  ;; different format when in a function, may includes frame.method, see https://github.com/vitest-dev/vitest/blob/main/packages/vitest/src/node/error.ts#L234 :
  ;;  ❯ Object.send test/middleware/error-handler.test.js:59:45

  ;; "❯ ([^:]+):([^:]+):([^:]+)"

  ;; I know this one works:
  ;; (defvar vitest-error-regexp "^ ❯ \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")
  (defvar vitest-error-regexp "^ ❯ \\(?:[^ ]+ \\)?\\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$")

  ;; But this is simpler, but I need to test it.
  ;; from https://github.com/flocks/dotfiles/blob/fc9548aaeecdc5bcdde3c4236efa20c1e8627fdd/emacs/.emacs.d/ft/ft-compile.el#L12
  ;; (setq vitest-error-regexp "^ ❯ \s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")

  (add-to-list 'compilation-error-regexp-alist-alist `(vitest ,vitest-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'vitest)

  ;; Make *compilation* buffer use visual-line-mode
  ;; TODO: make a key binding for turning vlmode on and off
  (add-hook 'compilation-mode-hook
            (lambda () (visual-line-mode 1)))

  (add-hook 'compilation-minor-mode-hook
            (lambda () (visual-line-mode 1)))

  ;; Handle ANSI color in compilation buffers.
  ;; First, some setup:
  (make-variable-buffer-local 'comint-output-filter-functions)
  (add-hook 'compilation-mode-hook
            (lambda () (setq comint-output-filter-functions
                        (remove 'ansi-color-process-output comint-output-filter-functions))))

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
  (setq compilation-environment '("TERM=xterm-256color")) ;; default nil
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
  (add-hook 'compilation-start-hook #'xterm-color-compilation-start-hook)
  (setq xterm-color-debug nil)

  ;; reference: https://github.com/atomontage/xterm-color/issues/28#issuecomment-487251253
  (defun xristos/font-lock-function (_)
    nil)

  (defun xristos/disable-font-lock ()
    (font-lock-mode -1)
    (make-local-variable 'font-lock-function)
    (setq font-lock-function 'xristos/font-lock-function))

  (defun xristos/shell-mode-hook ()
    (unless (eq major-mode 'aider-comint-mode)
      (xristos/disable-font-lock)
      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
    ;; TODO: The following depends on font-locking, it's better to write
    ;; my own mode for similar functionality at some point.
    ;; (compilation-shell-minor-mode 1)
    )

  ;; xristos does this for shell mode:
  ;; (add-hook 'shell-mode-hook 'xristos/shell-mode-hook)

  ;; but I'm going to try keeping font lock for shell mode, and only disable for comint (which I use for compilation)
  (add-hook 'comint-mode-hook 'xristos/shell-mode-hook)

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
  ;; (require 'ansi-color)
  ;; (defun colorize-compilation-buffer ()
  ;;   (read-only-mode 1)
  ;;   (ansi-color-apply-on-region compilation-filter-start (point))
  ;;   (read-only-mode -1))
  ;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (defun wjb/switch-to-project-jest-buffer ()
    "Switch to a project's jest buffer, falling back to last jest buffer.*"
    ;; needs to be updated for monorepos: cannot rely on projectile project name
    (interactive)
    (let* ((projectile-current-project-name (projectile-default-project-name (projectile-project-root)))
           (buffer-name (format "*jest*<%s>" projectile-current-project-name)))
      (if (buffer-live-p (get-buffer buffer-name))
          (switch-to-buffer buffer-name)
        (if (buffer-live-p wjb/last-jest-buffer)
            (switch-to-buffer wjb/last-jest-buffer)
          (user-error "No jest buffer")))))

  (defun wjb/switch-to-last-compilation-buffer ()
    "Switch to last compilation buffer, falling back to *compilation*."
    (interactive)
    (if (and wjb/last-compilation-buffer (buffer-live-p (wjb/last-compilation-buffer)))
        (switch-to-buffer wjb/last-compilation-buffer)
      (funcall-interactively #'wjb/switch-to-compilation-buffer)))

  (defun buffers-with-compilation-minor-mode ()
    (let ((buffers '()))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p compilation-minor-mode)
            (push (buffer-name buffer) buffers))))
      buffers))

  (defun buffers-for-jest ()
    (let ((jest-buffers '()))
      (dolist (buffer (buffer-list))
        (let ((buffer-name (buffer-name buffer)))
          (when (string-match-p "*jest*" buffer-name)
            (push buffer-name jest-buffers))))
      jest-buffers))

  (defun most-recently-visited-buffer (buffer-list)
    (car (sort (mapcar #'get-buffer (remove nil buffer-list))
               (lambda (b1 b2)
                 (time-less-p (buffer-local-value 'buffer-display-time b2)
                              (buffer-local-value 'buffer-display-time b1))))))

  (defun wjb/switch-to-compilation-buffer ()
    "Switch to *compilation*"
    (interactive)
    (let ((comp-buffer-name (most-recently-visited-buffer (buffers-with-compilation-minor-mode))))
      (if (buffer-live-p (get-buffer comp-buffer-name))
          (switch-to-buffer comp-buffer-name)
        (message "no compilation buffer"))))

  (defun wjb/switch-to-last-grep-buffer ()
    "Switch to last grep buffer."
    (interactive)
    (when wjb/last-grep-buffer
      (switch-to-buffer wjb/last-grep-buffer)))

  ;; based on purcell
  ;; (defadvice compilation-start (after wjb/save-compilation-buffer activate)
  ;;   "Save the compilation buffer to find it later."
  ;;   (let ((buf-name (buffer-name next-error-last-buffer)))
  ;;     (when (s-contains? "compil" buf-name t)
  ;;       (setq wjb/last-compilation-buffer next-error-last-buffer))
  ;;     (when (s-contains? "grep" buf-name t)
  ;;       (setq wjb/last-grep-buffer next-error-last-buffer))))

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

(use-package sane-term
  :disabled
  :commands (sane-term sane-term-create)
  :bind (("C-c s" . sane-term)
         ("C-c S" . sane-term-create)))

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below).
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y")
              (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))


;; vterm

(use-package vterm
  :bind
  ("H-`" . wjb/vterm-dwim)
  :config
  (defun toggle-vterm-copy-mode-cursor ()
    (if vterm-copy-mode
        (progn
          (setq last-color (face-background 'cursor))
          (set-cursor-color "red"))
      (progn
        (set-cursor-color last-color))))
  (add-hook 'vterm-copy-mode-hook #'toggle-vterm-copy-mode-cursor)

  (setq vterm-kill-buffer-on-exit t)
  (push "C-M-o" vterm-keymap-exceptions)
  (push "C-o" vterm-keymap-exceptions)
  (push "C-u" vterm-keymap-exceptions)
  ;; (push (kbd "C-<space>") vterm-keymap-exceptions)
  (vterm--exclude-keys vterm-mode-map vterm-keymap-exceptions)
  ;; hack: exclude will overwrite these, so they need to be re-defined. Would
  ;; be better if vterm defined them in a defun.
  ;; this may not be needed anymore -- need to try without it
  (define-key vterm-mode-map [tab]                       #'vterm-send-tab)
  (define-key vterm-mode-map (kbd "TAB")                 #'vterm-send-tab)
  (define-key vterm-mode-map [backtab]                   #'vterm--self-insert)
  (define-key vterm-mode-map [backspace]                 #'vterm-send-backspace)
  (define-key vterm-mode-map (kbd "DEL")                 #'vterm-send-backspace)
  (define-key vterm-mode-map [M-backspace]               #'vterm-send-meta-backspace)
  (define-key vterm-mode-map (kbd "M-DEL")               #'vterm-send-meta-backspace)
  (define-key vterm-mode-map [return]                    #'vterm-send-return)
  (define-key vterm-mode-map (kbd "RET")                 #'vterm-send-return)
  (define-key vterm-mode-map [left]                      #'vterm-send-left)
  (define-key vterm-mode-map [right]                     #'vterm-send-right)
  (define-key vterm-mode-map [up]                        #'vterm-send-up)
  (define-key vterm-mode-map [down]                      #'vterm-send-down)
  (define-key vterm-mode-map [prior]                     #'vterm-send-prior)
  (define-key vterm-mode-map [next]                      #'vterm-send-next)
  (define-key vterm-mode-map [home]                      #'vterm--self-insert)
  (define-key vterm-mode-map [end]                       #'vterm--self-insert)
  (define-key vterm-mode-map [escape]                    #'vterm--self-insert)
  (define-key vterm-mode-map [remap yank]                #'vterm-yank)
  (define-key vterm-mode-map [remap yank-pop]            #'vterm-yank-pop)
  (define-key vterm-mode-map [remap mouse-yank-primary]  #'vterm-yank-primary)
  (define-key vterm-mode-map (kbd "C-SPC")               #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-_")                 #'vterm-undo)
  (define-key vterm-mode-map (kbd "M-.")                 #'vterm-send-meta-dot)
  (define-key vterm-mode-map (kbd "M-,")                 #'vterm-send-meta-comma)
  (define-key vterm-mode-map (kbd "C-c C-y")             #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-c C-c")             #'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "C-c C-l")             #'vterm-clear-scrollback)
  (define-key vterm-mode-map [remap self-insert-command] #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-c C-t")             #'vterm-copy-mode)

  (defvar wjb/tmux-in-vterm nil)
  (make-variable-buffer-local 'wjb/tmux-in-vterm)

  (defun set-tmux-in-vterm (arg)
    ;; (message (format "set-tmux-in-vterm: %s" arg))
    (setq wjb/tmux-in-vterm (if (s-equals? arg "0") nil t)))
  (push '("set-tmux-in-vterm" set-tmux-in-vterm) vterm-eval-cmds)

  (defun wjb/vterm-maybe-send-C-v ()
    "Send C-v if tmux is on, otherwise don't send and do scroll-up-command instead."
    (interactive)
    (if wjb/tmux-in-vterm
        (call-interactively #'vterm-send-C-v)
      (call-interactively #'scroll-up-command)))

  (define-key vterm-mode-map (kbd "C-v")             #'wjb/vterm-maybe-send-C-v)

  (defun vterm-send-close-square-bracket ()
    "Sends `C-]' to libvterm."
    (interactive)
    (vterm-send-key "]" nil nil t))
  (define-key vterm-mode-map (kbd "C-]") #'vterm-send-close-square-bracket))

(add-hook 'vterm-mode-hook #'compilation-shell-minor-mode)

(defun wjb/vterm-dwim (&optional argument)
  "Invoke `vterm' according to context and current location.

With a \\[universal-argument] prefix or if no project is found, force a new
buffer to be created in place.

If existing, pop to it. Otherwise create a new buffer with a unique name at the project
root."
  (interactive "P")
  (if (or argument (not (projectile-project-root)))
      (vterm)
    (let* ((project (projectile-acquire-root))
           (buffer (format "*vterm %s*" (projectile-project-name project))))
      (if (buffer-live-p (get-buffer buffer))
          (pop-to-buffer buffer)
        (projectile-with-default-dir project
          (unless (require 'vterm nil :noerror)
            (error "Package 'vterm' not found"))
          (vterm buffer))))))
(global-set-key (kbd "H-`") #'wjb/vterm-dwim)


;; lsp

;; (require 'setup-lsp)
(use-package lsp-mode
  :disabled
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((js-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after (lsp-mode)
  )

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :after (lsp-mode)
  :config
  ;; (dap-auto-configure-mode)
  (setq dap-print-io nil)

  (dap-ui-mode -1)
  ;; enables mouse hover support
  (dap-tooltip-mode -1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode -1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode -1)

  (dap-register-debug-template
   "sd-playground"
   (list :type "node"
         :request "attach"
         :port 9329
         :program "__ignored"
         :localRoot (home-subdir "scm/sd/sd-playground/")
         :remoteRoot "/usr/src/app"
         :name "sd-playground"))

  (dap-register-debug-template
   "indium-ts-sourcemap"
   (list :type "node"
         :request "attach"
         :port 9229
         :program "__ignored"
         :name "indium-ts-sourcemap1"))

  (require 'dap-node)
  (dap-node-setup)
  )


;; visual-regexp
(use-package visual-regexp
  :bind (("C-c q" . vr/query-replace))
  ;; (define-key global-map (kbd "C-c r") 'vr/replace)
  ;; if you use multiple-cursors, this is for you:
  ;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)
  )

;; also see visual-regexp-on-steroids


;; multiple-cursors
;;
;; See: https://github.com/magnars/multiple-cursors.el
;;
(use-package multiple-cursors
  :bind (:map global-map
              ;; ("C-x t" . 'set-rectangular-region-anchor) ;
              ("C->" . 'mc/mark-next-like-this)
              ("C-<" . 'mc/mark-previous-like-this)
              ("C-*" . 'mc/mark-all-like-this)
              ("C-M-<" . 'mc/edit-beginnings-of-lines)
              ("C-M->" . 'mc/edit-ends-of-lines)))

(use-package mc-extras
  :after multiple-cursors
  :config
  (define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)
  (define-key mc/keymap (kbd "C-. C-o") 'mc/remove-cursors-on-blank-lines))


;; navigation / editing / formatting

(use-package avy)

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind (:map global-map
              ("C-=" . 'er/expand-region))
  :config
  (setq expand-region-subword-enabled t
        expand-region-smart-cursor t
        expand-region-autocopy-register "e"))

(use-package easy-kill
  :config
  (setq easy-kill-try-things '(url email sexp)
        easy-kill-alist '((?w word           " ")
                          (?s sexp           "\n")
                          (?l list           "\n")
                          (?f filename       "\n")
                          (?d defun          "\n\n")
                          (?D defun-name     " ")
                          (?L line           "\n")
                          (?b buffer-file-name)))
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package copy-as-format
  :config
  (global-set-key (kbd "C-c w s") 'copy-as-format-slack)
  (global-set-key (kbd "C-c w g") 'copy-as-format-github))

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
;;
(use-package symbol-overlay
  :after restclient
  :bind (:map prog-mode-map
              ;; ("M-i" . 'symbol-overlay-put) ;; not using
              ("M-n" . 'symbol-overlay-jump-next)
              ("M-p" . 'symbol-overlay-jump-prev)
              ("<f7>" .  'symbol-overlay-mode)
              ("<f8>" .  'symbol-overlay-remove-all))
  :bind (:map restclient-mode-map
              ("M-n" . 'symbol-overlay-jump-next)
              ("M-p" . 'symbol-overlay-jump-prev))
  :bind (:map text-mode-map
              ("M-n" . 'symbol-overlay-jump-next)
              ("M-p" . 'symbol-overlay-jump-prev)))

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

(use-package hungry-delete
  :disabled
  :diminish
  :config
  (setq hungry-delete-chars-to-skip " \t"
        hungry-delete-except-modes '(help-mode minibuffer-inactive-mode calc-mode org-mode))

  ;; TODO: turn off in some modes:
  ;; - org-mode?
  ;; - whitespace sensitive modes?
  (global-hungry-delete-mode))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))


;; ace

(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x l" . ace-window))
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-ignore-current nil
        aw-ignored-buffers '(dirtree-mode)
        aw-ignore-on t
        aw-keys '(?1 ?2 ?3 ?4)))

(use-package ace-jump-helm-line
  :disabled
  :config
  (setq ace-jump-helm-line-idle-delay 3
        ace-jump-helm-line-style 'pre
        ;; ace-jump-helm-line-style 'de-bruijn

        ;; Select by default
        ace-jump-helm-line-default-action 'select
        ;; Set the move-only and persistent keys
        ace-jump-helm-line-select-key ?s ;; this line is not needed
        ace-jump-helm-line-move-only-key ?m
        ace-jump-helm-line-persistent-key ?p)
  (ace-jump-helm-line-idle-exec-add 'helm-mini))


;; appearance

(use-package beacon
  :diminish
  :config
  (setq beacon-blink-duration 0.1
        beacon-blink-when-point-moves-vertically 20
        beacon-blink-when-window-scrolls t)
  ;; TODO don't use beacon-mode in shell-mode
  (beacon-mode 1))

;; Use `page-break-lines-mode' to enable the mode in specific buffers, or
;; customize `page-break-lines-modes' and enable the mode globally with
;; `global-page-break-lines-mode'.
;;
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; TODO(nativecomp): why doesn't tab-bar show?
;; X minibuffer-local-must-match-filename-map
;; not real fullscreen?
(setq tab-bar-show t)

;; from https://protesilaos.com/dotemacs/
;;
;; from https://www.reddit.com/r/emacs/comments/fkqj9v/emacs_27_tabbarmode_for_macos_show_in_minibuffer/ :
;; Generally speaking you can use (assoc 'name (tab-bar--current-tab)) to get the current tab name.
(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints t)

  (tab-bar-mode 1)
  (tab-bar-history-mode -1)

  (defun prot/icomplete-tab-bar-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (icomplete-vertical-do ()
                                    (tab-bar-switch-to-tab
                                     (completing-read "Select tab: " tabs nil t)))))))

  :bind (("M-1" . prot/icomplete-tab-bar-tab-dwim)
         ("H-t" . prot/icomplete-tab-bar-tab-dwim)
         ("M-1" . tab-bar-select-tab)
         ("M-2" . tab-bar-select-tab)
         ("M-3" . tab-bar-select-tab)
         ("M-4" . tab-bar-select-tab)
         ("H-1" . tab-bar-select-tab)
         ("H-2" . tab-bar-select-tab)
         ("H-3" . tab-bar-select-tab)
         ("H-4" . tab-bar-select-tab)
         ("M-0" . tab-switcher)))

(use-package rainbow-delimiters
  :defer 5
  :init
  (add-hook 'json-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'json-ts-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :init
  ;; (add-hook 'emacs-lisp-mode-hook 'rainbow-mode) ;; conflicts with paren-face
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'css-ts-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)
  (add-hook 'typescript-base-mode-hook 'rainbow-mode)
  (add-hook 'js-base-mode-hook 'rainbow-mode)
  (add-hook 'conf-mode-hook 'rainbow-mode)
  (add-hook 'help-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

(use-package centered-cursor-mode
  :config
  (setq ccm-recenter-at-end-of-file t)
  (defun wjb/set-ccm-vpos-init ()
    (setq ccm-vpos-init (wjb/calculate-ccm-vpos-init)))
  (defun wjb/calculate-ccm-vpos-init ()
    (round (* (ccm-visible-text-lines) .38)))
  (setq-default ccm-vpos-init (wjb/set-ccm-vpos-init))

  (add-hook 'woman-mode-hook #'centered-cursor-mode)
  (add-hook 'woman-mode-hook #'wjb/set-ccm-vpos-init)
  (add-hook 'text-mode-hook #'centered-cursor-mode)
  (add-hook 'text-mode-hook #'wjb/set-ccm-vpos-init))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  ;; (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode 1)
  ;; setting the solaire faces is in wjb/customize-appearance.
  )


;; startup

(defun wjb/emacs-startup-hook ()
  (setq source-directory (expand-file-name "scm/vendor/emacs-mac" wjb/home-directory)
        find-function-C-source-directory (expand-file-name "scm/vendor/emacs-mac/src" wjb/home-directory))
  ;; first, appearance defines some defuns that are used by a treemacs hook
  (require 'appearance)
  ;; loads and starts treemacs, runs treemacs hooks
  (treemacs-select-window)

  ;; display-fill-column-indicator is builtin. visual-fill-column-mode does something different.
  (global-display-fill-column-indicator-mode)
  (add-hook 'vterm-mode-hook (lambda ()
                               (display-fill-column-indicator-mode -1)))

  ;; load theme and customize appearance
  (progn
    (change-theme 'modus-vivendi t)
    (solaire-global-mode -1)
    (setq wjb/dark t)
    (wjb/customize-appearance))
  )
(add-hook 'emacs-startup-hook #'wjb/emacs-startup-hook)

(use-package envrc
  :hook (after-init . envrc-global-mode))

;; (defun wjb/run-once-when-idle (fun)
;;   "Run a command every once in a while, if possible when emacs is idle."
;;   (defun wjb/generate-idle-callback (fun)
;;     (defun ()
;;         (call fun)
;;       (remove-hook 'auto-save-hook thing)))
;;   ;; TODO: need unique name for thing
;;   (setq thing (wjb/generate-idle-callback fun))
;;   (add-hook 'auto-save-hook thing))

;; TODO: am I handling safe-local-variable-values in a sensible way?
;; look at purcell, etc.



;; dir-locals

;; https://emacs.stackexchange.com/a/5531/2163
(defvar walk-dir-locals-upward nil
  "If non-nil, evaluate .dir-locals.el files starting in the
  current directory and going up. Otherwise they will be
  evaluated from the top down to the current directory.")

(defadvice hack-dir-local-variables (around walk-dir-locals-file activate)
  (let* ((dir-locals-list (list dir-locals-file))
         (walk-dir-locals-file (first dir-locals-list)))
    (while (file-readable-p (concat "../" walk-dir-locals-file))
      (progn
        (setq walk-dir-locals-file (concat "../" walk-dir-locals-file))
        (add-to-list 'dir-locals-list walk-dir-locals-file
                     walk-dir-locals-upward)
        ))
    (dolist (file dir-locals-list)
      (let ((dir-locals-file (expand-file-name file)))
        ad-do-it
        ))))


;; tree-sitter (builtin)
(use-package treesit
  :config

  ;; Note: I'm Using treesit-auto for this.
  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  ;; (dolist (mapping '(
  ;;                    (python-mode . python-ts-mode)
  ;;                    (css-mode . css-ts-mode)
  ;;                    ;; (typescript-mode . typescript-ts-mode)
  ;;                    (typescript-mode . tsx-ts-mode)
  ;;                    ;; (js-mode . js-ts-mode)
  ;;                    (js-json-mode . json-ts-mode)
  ;;                    ;; (yaml-mode . yaml-ts-mode)
  ;;                    ))
  ;;   (add-to-list 'major-mode-remap-alist mapping))
)

;; Remove langs from this list that don't have ts modes yet (html, markdown) or I don't want to use their ts mode.
;; Removed javascript because js2-mode has a lot of good stuff: js2-refactor, jest-mode, key bindings for tide...
(setq wjb/treesit-auto-langs '(awk bash bibtex c c-sharp clojure cmake commonlisp cpp css dart dockerfile elixir go gomod heex java json julia kotlin latex lua make proto python r ruby rust toml tsx typescript typst verilog vhdl yaml))

(use-package treesit-auto
  :after treesit
  :init
  (setq treesit-auto-langs wjb/treesit-auto-langs)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs wjb/treesit-auto-langs)
  :config
  (treesit-auto-add-to-auto-mode-alist treesit-auto-langs)
  (global-treesit-auto-mode))

(use-package combobulate
  ;; :disabled
  :preface
  ;; (setq combobulate-key-prefix "C-c .")
  (setq combobulate-key-prefix "M-c") ;; or M-j
  :config
  (setf (cadr (assoc 'javascript combobulate-registered-languages-alist))
        (append (cadr (assoc 'javascript combobulate-registered-languages-alist))
                '(jtsx-jsx-mode)))
  (setf (cadr (assoc 'tsx combobulate-registered-languages-alist))
        (append (cadr (assoc 'tsx combobulate-registered-languages-alist))
                '(jtsx-tsx-mode)))
  (setf (cadr (assoc 'typescript combobulate-registered-languages-alist))
        (append (cadr (assoc 'typescript combobulate-registered-languages-alist))
                '(jtsx-typescript-mode)))

  :hook ((prog-mode . combobulate-mode))
  :load-path ("elisp/combobulate"))


;; key-bindings. Must be after defuns. Should be near the end, to avoid being overwritten.

(require 'key-bindings)

;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)
(define-key endless/toggle-map "a" #'wjb/customize-appearance)
(define-key endless/toggle-map "m" #'toggle-frame-maximized)
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "t" #'treemacs-select-window)
(define-key endless/toggle-map "D" #'toggle-window-dedicated)

;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

;; From http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(define-key endless/toggle-map "n"
  #'narrow-or-widen-dwim)

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(defun joaot/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))
(define-key process-menu-mode-map (kbd "C-k") 'joaot/delete-process-at-point)

;; moved to scm/wjb/wjb-org-static-blog
;; (require 'wjb-org-static-blog)



(use-package shell-maker)
(use-package chatgpt-shell
  :after shell-maker
  :custom
  (
   ;; (comint-use-prompt-regexp t) ;; trying to get prompt to have a different face. But I should do this in a hook, not set it generally.
   (chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))))

(use-package dall-e-shell
  :after shell-maker
  :custom
  ((dall-e-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))))

(require 'setup-copilot)
(require 'setup-gptel)
(require 'setup-aider)

(use-package 'mediainfo-mode
  :load-path ("elisp/mediainfo-mode"))

(provide 'main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main.el ends here
