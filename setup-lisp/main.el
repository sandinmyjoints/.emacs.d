;;; -*- lexical-binding: t no-byte-compile: t -*-
;;; main.el --- Emacs configuration.
;;
;; Filename: init.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Thu Oct  2 08:04:34 2014 (-0700)
;; Version:
;; Package-Requires: ((emacs "24.3"))
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

;; Base packages.
;;
;; Lists.
(use-package dash
  :ensure t)
;; Sequences.
(use-package seq
  :ensure t)
;; Strings.
(use-package s
  :ensure t)
;; Hash table.
(use-package ht
  :ensure t)
;; Filesystem.
(use-package f
  :ensure t)
;; Hashtables.
(use-package ht
  :ensure t)
;; Alists.
(use-package asoc
  :defer 1
  :load-path "elisp/asoc.el")

(use-package simple
  :config
  ;; TODO: only use these in modes where it makes sense. Org is not one of
  ;; them. From https://www.reddit.com/r/emacs/comments/e1uyvk/weekly_tipstricketc_thread/f93y0qg?utm_source=share&utm_medium=web2x
  ;;
  ;; Do word wrapping at fill column in visual-line-mode.
  (remove-hook 'visual-line-mode-hook #'visual-fill-column-mode)

  ;; Preserve indents when wrapping lines in visual-line-mode.
  (remove-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(when is-mac (require 'setup-mac))

;; ========================================
;; Machine-local custom configuration.
;; ========================================

(load custom-file t t)

;; ========================================
;; Helper defuns.
;; ========================================

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

(defun wjb/smart-magit-status ()
  (interactive)
  (if (window-dedicated-p (get-buffer-window))
      (progn
        (other-window 1)
        (call-interactively #'magit-status))
    (call-interactively #'magit-status)))

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

(use-package smart-mode-line
  ;; :after minions
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
  (setq minions-direct '(flycheck-mode))
  (minions-mode 1))

(require 'setup-grep)

(use-package wgrep
  :defer t
  :config
  (setq wgrep-enable-key "w"))

(require 'sane-defaults)

(require 'defuns)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package simple
  :diminish auto-fill-function)

(use-package abbrev
  :defer 4
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Abbrevs.html#Editing-Abbrevs
  ;; (list-abbrevs)
  :diminish abbrev-mode)

(use-package server
  :defer 4
  :config
  (unless (server-running-p)
    (message "Starting server...")
    (server-start)))

(use-package prog-mode
  :config
  (defun auto-fill-comments ()
    "Automatically fill comments, but nothing else"
    (setq-local comment-auto-fill-only-comments t)
    (setq truncate-lines nil))
  (add-hook 'prog-mode-hook #'auto-fill-comments)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Text and fill modes.
(defun wjb/soft-wrap-text ()
  "Soft wrap: sets fill-column to 10000. Doesn't auto-fill;
instead, wraps at screen edge, thanks to visual-line-mode."
  (set-fill-column 10000)
  (auto-fill-mode -1)
  (visual-line-mode 1))

(defun wjb/hard-wrap-text ()
  "Hard wrap: sets fill-column to 80 and auto-fills."
  ;; C-x f is set-fill-column
  (set-fill-column 78)
  (auto-fill-mode 1)
  (visual-line-mode -1))

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
  :defer t
  :config
  (defun wjb/olivetti ()
    "Turn on settings for writing prose."
    (interactive)
    (gfm-mode)
    (olivetti-mode))

  (setq-default olivetti-body-width 80)
  (add-hook 'olivetti-mode-hook #'wjb/soft-wrap-text))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(when (require 'so-long nil :noerror)
  (so-long-enable))

(use-package vlf
  :defer 6
  ;; put this in vlf-setup.el, L104:
  ;; ((string-equal filename "TAGS")
  ;;  (let ((large-file-warning-threshold nil))
  ;;    (ad-do-it)))
  :config
  (require 'vlf-setup))

(use-package flycheck
  :defer 5
  :init
  ;; This turns on Flycheck globally in only these modes. Others can be turned on
  ;; per-buffer.
  (defvar flycheck-global-modes
    '(js2-mode
      js2-jsx-mode
      rjsx-mode
      json-mode
      coffee-mode
      css-mode
      less-css-mode
      sql-mode
      emacs-lisp-mode
      sh-mode
      yaml-mode
      python-mode
      perl-mode
      perl6-mode))
  ;; (setq flycheck-global-modes
  ;;       '(not org-mode text-mode conf-mode restclient-mode))
  :config
  (setq-default flycheck-display-errors-delay 0.2
                flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                flycheck-disabled-checkers '(javascript-jshint html-tidy emacs-lisp-checkdoc)
                flycheck-temp-prefix ".flycheck"
                flycheck-navigation-minimum-level 'warning)
  ;; see https://github.com/flycheck/flycheck/issues/186#issuecomment-32773904
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-pycompile 'python-pylint)

  ;; too many typescript errors, and complains about missing definitions
  ;; files. And can it find anything that eslint can't?
  ;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide)
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide)

  (add-to-list 'safe-local-variable-values '(flycheck-javascript-eslint-executable . "eslint_d"))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (require 'setup-flycheck)
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

(use-package pos-tip
  :load-path "elisp/pos-tip")

(use-package flycheck-pos-tip
  :after (flycheck pos-tip)
  :load-path "elisp/flycheck-pos-tip"
  :config
  (setq flycheck-pos-tip-timeout -1
        flycheck-pos-tip-max-width 120)
  (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

(use-package flycheck-status-emoji
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-status-emoji-mode))

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2)
  (defun wjb/css-mode-hook ()
    (setq company-backends wjb/company-backends-css))
  (add-hook 'css-mode-hook #'wjb/css-mode-hook)
)

(use-package elisp-mode
  :mode "abbrev_defs"
  :config
  (diminish 'lisp-interaction-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  ;; (add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 70)))
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header))

(use-package eldoc
  :after elisp-mode
  :diminish eldoc-mode
  :config
  (diminish 'eldoc-mode))

(autoload 'auto-make-header "header2")

(use-package comment-dwim-2
  ;; Default for builtin comment-line is C-x C-;
  :bind (("M-;" . comment-dwim-2)))

(use-package beacon
  :diminish
  :defer 1
  :config
  (setq beacon-blink-duration 0.1)
  ;; TODO don't use beacon-mode in shell-mode
  (beacon-mode 1))

(use-package dired
  :defer 1
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

  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)

  ;; bsd ls vs. gls: this is written for bsd, but gls is probably
  ;; better
  ;;
  (setq dired-listing-switches "-lahF"
        dired-dwim-target t
        dired-recursive-copies 'always))

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
        ediff-split-window-function 'split-window-horizontally))

(use-package help-mode
  :init
  (add-hook 'help-mode-hook 'visual-line-mode)
  :diminish visual-line-mode)

(use-package helpful
  :defer 4
  :after help-mode
  :config
  (add-hook 'helpful-mode-hook 'visual-line-mode)

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
  :config
  (defface org-checkbox-done-text
    '((t (:strike-through t :slant italic :weight light) ))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   ;; from https://blog.jft.rocks/emacs/unicode-for-orgmode-checkboxes.html
   ;; TODO: it is striking through the newline at the end of the line
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  (setq org-src-fontify-natively t
        org-directory "~/notes"
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
        org-return-follows-link t)

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
    (set-fill-column 80)

    (setq-local company-backends wjb/company-backends-org)
    (setq-local completion-at-point-functions '(pcomplete-completions-at-point))

    (hungry-delete-mode -1)
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

    (local-set-key (kbd "<S-up>") 'outline-previous-visible-heading)
    (local-set-key (kbd "<S-down>") 'outline-next-visible-heading))

  (add-hook 'org-mode-hook #'wjb/org-mode-hook t)

  (global-set-key (kbd "H-c") #'org-capture)

  (require 'org-tempo))

(use-package ox-reveal
  :defer 5
  :after org)

(use-package org-src
  :defer 5
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
     (ein . t)
     (restclient . t))))

(use-package ob
  :disabled
  :after org
  :config
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

  ;; Load ODT backend to allow for exporting to open document format.
(use-package ox-odt
  :defer 5
  :after org)
(use-package ox-gfm
  :defer 5
  :after org)
(use-package ox-slack
  :defer 5
  :after org)

;; Use `page-break-lines-mode' to enable the mode in specific buffers,
;; or customize `page-break-lines-modes' and enable the mode globally with
;; `global-page-break-lines-mode'.
;;
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; Fix sql-prompt-regexp: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27586
(use-package sql
  :defer t
  :after page-break-lines
  :config
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat)
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
  (push 'sql-mode page-break-lines-modes)

  (setq-default sql-input-ring-file-name
                (expand-file-name ".sqli_history" user-emacs-directory)
                sql-product 'mysql)

  (add-to-list 'sql-mysql-login-params '(port :default 3311))
  ;; (push "" sql-mysql-options)

  (defun sanityinc/fix-postgres-prompt-regexp ()
    "Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22596.
Fix for the above hasn't been released as of Emacs 25.2."
    (when (or (eq sql-product 'postgres) (eq sql-product 'mysql))
      (setq-local sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
      (setq-local sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "))
    (setq comint-scroll-to-bottom-on-output t))

  (add-hook 'sql-interactive-mode-hook 'sanityinc/fix-postgres-prompt-regexp)

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

  ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
  (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
    (unless (eq 'oracle sql-product)
      (sql-product-font-lock nil nil)))
  (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode))

(use-package sqlformat
  :after sql
  :config
  ;; needs sqlparse package, which can be gotten with homebrew
  ;; (add-hook 'sql-mode-hook 'sqlformat-on-save-mode) ;; this was getting annoying
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))

(use-package ghub
  :defer t)

;; Magit.
(use-package magit
  :bind (("C-x g" . wjb/smart-magit-status))
  :config
  (setq ghub-use-workaround-for-emacs-bug nil
        magit-last-seen-setup-instructions "1.4.0"
        magit-diff-auto-show '(stage-all log-oneline log-follow log-select blame-follow)
        magit-status-expand-stashes nil
        magit-commit-show-diff nil
        magit-revert-buffers 1 ;; important for not slowing down everything
        ;; magit-completing-read-function 'magit-ido-completing-read
        magit-completing-read-function 'ivy-completing-read
        magit-push-always-verify nil
        magit-revision-insert-related-refs nil
        magit-branch-read-upstream-first nil)
        ;; experimental, see https://magit.vc/manual/magit/The-Branch-Popup.html
        magit-branch-prefer-remote-upstream '(master)
        ;; experimental:
        magit-process-connection-type nil
  (autoload 'magit-log "magit"))

;; Experiment, might want to do this for everything:
(use-package setup-magit
  :after magit)

(use-package forge
  :disabled
  :after magit)

(use-package github-review
  :after magit
  :disabled
  :config
  (setq github-review-fetch-top-level-and-review-comments t))

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
  ;; :disabled
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

(use-package elpy
  :disabled
  :config
  (elpy-enable)
  (setq elpy-modules (-remove-item 'elpy-module-flymake elpy-modules)))

(use-package smart-dash
  :disabled
  :hook (python-mode . smart-dash-mode))

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

  (add-hook 'python-mode-hook (lambda ()
                                (hack-local-variables)
                                (setq fill-column 79)
                                (set-face-background 'highlight-indentation-face "#111")
                                (pyvenv-tracking-mode)
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
  ;; (add-hook 'emacs-lisp-mode-hook 'rainbow-mode) ;; conflicts with paren-face
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)
  (add-hook 'conf-mode-hook 'rainbow-mode)
  (add-hook 'help-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

(use-package tsv-mode
  :defer t
  :disabled
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

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)

  (defun wjb/switch-to-vterm ()
    (interactive)
    (push-mark)
    (projectile-run-vterm))
  (define-key projectile-mode-map (kbd "H-g") #'wjb/switch-to-vterm)
  (define-key projectile-mode-map (kbd "C-M-g") #'wjb/switch-to-vterm)

  (counsel-projectile-mode))

(use-package counsel-css
  :after (counsel)
  :config
  (add-hook 'css-mode-hook 'counsel-css-imenu-setup))

;; (use-package ido
;;   :disabled
;;   :config
;;   (require 'setup-ido))

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

;; (defadvice ido-imenu (before push-mark activate)
;;   (push-mark))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(use-package imenu-anywhere)

(use-package flx)

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

;; switching/finding/opening/running things
;; - C-o = helm-mini -> buffers, recent files, bookmarks, more? (cf M-o)
;; - C-x b = switch buffer (among open buffers)
;;   - C-x C-b = was ibuffer, now helm-mini
;;   - H-x b = helm-buffers-list
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
  :defer
  :diminish
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer) ;; Use C-M-j to call ivy-immediate-done to create new buffer
  (global-set-key (kbd "C-x b") #'wjb/smart-counsel-switch-buffer) ;; giving this a try
  (global-set-key (kbd "H-0 f") 'counsel-find-file)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
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
        ivy-re-builders-alist '((swiper . ivy--regex-ignore-order)
                                (swiper-isearch . ivy--regex-ignore-order)
                                (counsel-projectile-switch-project . ivy--regex-ignore-order)
                                (ivy-switch-buffer . ivy--regex-fuzzy)
                                (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package posframe
  :config
  (setq posframe-arghandler #'wjb/posframe-arghandler)
  (defun wjb/posframe-arghandler (buffer-or-name arg-name value)
    ;; see
    ;; https://github.com/tumashu/posframe/blob/bfd2e55219e0911980f4ea97b5995ce8553dce60/posframe.el#L439
    ;; for a list of parameters
    (let ((info '(
                  :min-width 80
                  :min-height 10
                  :internal-border-width 2
                  :internal-border-color "#000"
                  :left-fringe 4
                  :right-fringe 4
                  :font "Fira Code-14")))
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
        ivy-display-function #'ivy-posframe-display-at-frame-above-center
        ;; for some reason this has to be changed to take effect
        ivy-posframe-border-width 2
        ivy-posframe-parameters
        '((left-fringe . 4)
          (right-fringe . 4)))

  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-above-center)))
  (ivy-posframe-mode 1))

;; uses hydra, hydra-posframe
(require 'services)

(use-package which-key-posframe
  :defer 4
  :config
  (which-key-posframe-mode))

(use-package ivy-rich
  :config
  :disabled
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)

  ;; see https://www.reddit.com/r/emacs/comments/b5n1yh/weekly_tipstricketc_thread/ejems0p/
  (defun ivy-rich-branch-candidate (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize (replace-regexp-in-string abbreviated-home-dir "~/" (file-name-directory (directory-file-name candidate))) 'face 'font-lock-doc-face)
                (propertize (file-name-nondirectory (directory-file-name candidate)) 'face 'success)))))

  (defun ivy-rich-branch (candidate)
    (let* ((candidate (expand-file-name candidate ivy--directory))
           (default-directory candidate))
      (if (or (not (magit-git-repo-p candidate)) (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s"
                (propertize (if (magit-rebase-in-progress-p) "REBASE" (or (magit-get-current-branch) "*NONE*")) 'face 'warning)))))

  (defun ivy-rich-count (candidate)
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate) (not (magit-git-repo-p candidate)))
          ""
        (format "%s" (length (projectile-project-buffers (projectile-ensure-project candidate)))))))

  (defun projectile-compilation-buffers (&optional project)
    "Get a list of a project's compilation buffers.
If PROJECT is not specified the command acts on the current project."
    (let* ((project-root (or project (projectile-project-root)))
           (buffer-list (mapcar #'process-buffer compilation-in-progress))
           (all-buffers (cl-remove-if-not
                         (lambda (buffer)
                           (projectile-project-buffer-p buffer project-root))
                         buffer-list)))
      (if projectile-buffers-filter-function
          (funcall projectile-buffers-filter-function all-buffers)
        all-buffers)))

  (defun ivy-rich-compiling (candidate)
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate) (not (magit-git-repo-p candidate)))
          ""
        (if (projectile-compilation-buffers candidate)
            "compiling"
          ""))))

  ;; wjb: this one is actually somewhat useful, but slow
  (plist-put ivy-rich--display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate (:width 45))
                ;; (ivy-rich-count (:width 3 :align left))
                (ivy-rich-branch (:width 30))
                (ivy-rich-compiling))))
  (ivy-rich-set-display-transformer)

  (defun ivy-rich-file-info (candidate)
    "read file information, try to deal with tramp"
    (if (file-remote-p (concat ivy--directory candidate))
        "     tramp method"
      (let* ((candidate (expand-file-name candidate ivy--directory))
             (attrs (file-attributes candidate))
             (type (nth 0 attrs))
             (size (nth 7 attrs))
             (modes (nth 8 attrs))
             (user (user-login-name (nth 2 attrs)))
             (group (nth 3 attrs))
             (info ""))
        (cond
         ((eq attrs nil)
          (setq info "nofile"))
         ((stringp type) ;; file is just an symbolic link
          (setq info (concat "     ->  " (expand-file-name type ivy--directory))))
         (t (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
                ""
              (setq info
                    (cond ((eq type t) ;; directory
                           "    dir")
                          (t
                           (cond ((> size 1000000) (format "%6.1fM" (/ size 1000000.0)))
                                 ((> size 1000) (format "%6.1fk" (/ size 1000.0)))
                                 (t (format "%6dB" size))))))
              (setq info (format "%s  %s  %s" info modes user))))
         info ))))

  ;; wjb: gratuitous!
  (setq ivy-rich-display-transformers-list
        (plist-put
         ivy-rich-display-transformers-list 'counsel-find-file
         '(:columns
           ((ivy-read-file-transformer    (:width 0.4))
            (ivy-rich-file-info               (:width 20 :face font-lock-doc-face))
            ;; (ivy-rich-file-last-modified-time (:width 25 :face font-lock-doc-face))
            ))))
  (ivy-rich-set-display-transformer)
  )

(use-package counsel
  :defer t
  :config
  (ivy-configure 'counsel-M-x
    :initial-input ""
    :display-transformer-fn #'counsel-M-x-transformer)

  (setq counsel-find-file-at-point t
        counsel-preselect-current-file t
        counsel-yank-pop-preselect-last t)
  ;; from https://emacs.stackexchange.com/a/33706/2163
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
        (alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    ;; TODO: Would like to do this for find-alternate-file as well, what map is active in that?
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))

;; TODO: C-g when helm-mini is showing actually quits
(use-package helm
  :defer t
  :bind (("M-o" . helm-browse-project)
         ("C-x C-b" . helm-buffers-list)
         ("C-o" . helm-mini)
         ("C-x C-o" . helm-mini)) ; Clobbers delete-blank-lines.
  :config
  ;; (require 'helm-config)

  ;; (global-set-key (kbd "M-o") #'helm-browse-project)
  ;; (global-set-key (kbd "C-o") #'helm-mini)  ;; within helm-mini, helm-mini again jumps to next section -- nice!
  ;; (global-set-key (kbd "C-x C-b") #'helm-buffers-list) ;; clobbers ibuffer
  ;; (global-set-key (kbd "H-x b") #'helm-buffers-list)
  ;; (global-set-key (kbd "H-o") #'helm-browse-project)

  ;; useful commands, but probably shouldn't be bound globally:
  ;; (global-set-key (kbd "C-'") 'helm-mark-all)
  ;; (global-set-key (kbd "C-\"") 'helm-ff-run-marked-files-in-dired)

  (require 'helm-dired-recent-dirs)
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
                                    ;; helm-source-mac-spotlight
                                    helm-source-locate
                                    helm-source-file-cache
                                    ;; helm-source-files-in-current-dir
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
   helm-ff-skip-boring-files t
   helm-buffer-max-length 24
   helm-buffer-skip-remote-checking t
   helm-buffers-end-truncated-string "…"
   helm-echo-input-in-header-line t
   helm-buffer--pretty-names '((dired-mode . "Dired")
                               (lisp-interaction-mode . "Lisp Inter")
                               (magit-process-mode . "Magit proc")
                               (rest-client-mode . "REST")
                               (shell-script-mode . "Shell"))
   )
  (when is-mac
    (setq helm-locate-fuzzy-match nil
          helm-locate-command "mdfind -name %s %s"))
  )

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
        ace-jump-helm-line-persistent-key ?p
        )
  (ace-jump-helm-line-idle-exec-add 'helm-mini)
)

(use-package helm-xref
  :after helm
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs
        helm-xref-candidate-formatting-function 'wjb/helm-xref-format-candidate-long
        helm-xref-input "!test ")

  ;; see https://github.com/brotzeit/helm-xref/issues/19
  (defun wjb/helm-xref-format-candidate-long (file line summary)
    "Build long form of candidate format with FILE, LINE, and SUMMARY."

    (setq modified-file (s-replace (projectile-project-root) "" file))
    (setq modified-file (s-replace-regexp abbreviated-home-dir "" modified-file))
    (setq modified-file (s-replace "scm/" "" modified-file))
    (setq modified-file (s-replace "sd/" "" modified-file))
    (concat
     ;; (propertize file 'font-lock-face 'helm-xref-file-name)
     (propertize modified-file 'font-lock-face 'helm-xref-file-name)
     (when (string= "integer" (type-of line))
       (concat
        ":"
        (propertize (int-to-string line)
                    'font-lock-face 'helm-xref-line-number)))
     ":"
     summary)))

(use-package helm-aws
  :load-path "elisp/helm-aws"
  :defer 6
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
  (use-package semantic/symref/grep
    :config
    (add-to-list 'semantic-symref-filepattern-alist '(js2-mode "*.js" "*.jsx"))
    (add-to-list 'semantic-symref-filepattern-alist '(coffee-mode "*.coffee"))
    (add-to-list 'semantic-symref-filepattern-alist '(helpful-mode "*"))
    (add-to-list 'semantic-symref-filepattern-alist '(sql-mode "*.sql"))
    (add-to-list 'semantic-symref-filepattern-alist '(org-mode "*.org")))

  ;; (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-selector 'helm)
  (unbind-key "C-M-p" dumb-jump-mode-map)
  (setq dumb-jump-force-searcher 'rg)
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
  :defer t
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back))
  :config
  (smart-jump-setup-default-registers)
  ;; this binds to M-. and M-, in prog-mode-map:
  (smart-jump-bind-jump-keys 'prog-mode)
  ;; heuristic is used to know whether the jump succeeded or not.
  ;; error means it failed if an error was signaled.
  ;; point means it failed if point is the same after the jump as before.
  (smart-jump-register :modes 'js2-mode
                       :jump-fn 'js2-jump-to-definition
                       :should-jump t
                       :heuristic 'point
                       :async nil
                       :order 1)
  (smart-jump-typescript-mode-register 'js2-mode)
  (smart-jump-register :modes 'js2-mode
                       :jump-fn 'counsel-etags-find-tag-at-point
                       :should-jump t
                       :heuristic 'error
                       :async nil
                       :order 0)
    )

;; counsel-etags-scan-code
(use-package counsel-etags
  :defer t
  ;; :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  ;; this is how smart-jump heuristic error works:
  (defun counsel-etags-grep (&optional default-keyword hint root)
    (error "Signaling error instead of grepping"))

  (setq counsel-etags-update-interval 60
        counsel-etags-find-program "gfind"
        counsel-etags-grep-program "ggrep"
        counsel-etags-tags-program "ctags -e -L")
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-directories "dist")
  (add-to-list 'counsel-etags-ignore-directories "local_notes")
  (add-to-list 'counsel-etags-ignore-filenames "*.org"))

(require 'setup-tramp)

(require 'mode-mappings)

;; Lua mode.
(use-package lua
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package gitignore-mode
  :defer t
  :mode "global.gitignore")

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'json-mode-hook #'rainbow-delimiters-mode))

(use-package date-at-point)

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
  (add-hook 'magit-status-mode-hook #'wjb/disable-show-paren-mode)
  (show-paren-mode 1))

;; Dims parens in certain modes.
(use-package paren-face
  :defer 1
  :config
  (add-to-list 'paren-face-modes 'js-mode 'js2-mode)
  (global-paren-face-mode))

(use-package restclient
  :defer t
  :mode
  ("\\.rest\\'" . restclient-mode)
  ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify))
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

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(use-package expand-region
  :defer t
  :bind (:map global-map
              ("C-=" . 'er/expand-region)))

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

;; (require 'setup-dirtree)
;; (with-eval-after-load 'dirtree
;;   (progn
;;     ;; Free up for helm-mini.
;;     (unbind-key (kbd "C-o")  dirtree-mode-map)
;;     (bind-key (kbd "<return>") 'dirtree-display dirtree-mode-map)))

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

(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 3
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil ;; TODO t
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    'on-distance
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      nil
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          ;; 55 is good for widescreen. (treemacs-set-width 45) is good for laptop.
          treemacs-width                         55)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 18)
    (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (treemacs-git-mode -1)
    (add-hook 'treemacs-mode-hook 'hidden-mode-line-mode)
    (defun wjb/treemacs-hook ()
      ;; Preserve indents when wrapping lines in visual-line-mode.
      (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode nil t)
      (set-face-attribute 'treemacs-root-face nil :height 1.0 :underline nil)
      (setq-local cursor-type 'box))
    (add-hook 'treemacs-mode-hook #'wjb/treemacs-hook)
    ;; for this to work with visual fill, treemacs tags would need to be able
    ;; to handle wrapped lines
    ;; (add-hook 'treemacs-mode-hook #'visual-line-mode t)

    (treemacs-map-icons-with-auto-mode-alist
     '(".less")
     '(less-css-mode . (treemacs-get-icon-value "css")))

  (defun wjb/treemacs-ignore-compiled-files (filename filepath)
    (or
     (s-equals? (file-name-extension filename) "elc")
     (s-equals? (file-name-extension filename) "pyc")))
  (push #'wjb/treemacs-ignore-compiled-files treemacs-ignored-file-predicates)
  )

  :bind
  (:map global-map
        ("H-a"       . treemacs-select-window)
        ("C-c d"     . treemacs-add-project-to-workspace)
        ;; ("C-x t 1"   . treemacs-delete-other-windows)
        ;; ("C-x t t"   . treemacs)
        ;; ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag)
        )
  (:map treemacs-mode-map
        ("e" . treemacs-TAB-action)
        ("j" . treemacs-next-neighbour)
        ("k" . treemacs-previous-neighbour)
        )
  )

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (require 'lisp-stuff)

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

(use-package org-pivotal
  :load-path "elisp/org-pivotal"
  :defer 1)

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
              ("<f8>" .  'symbol-overlay-remove-all))
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

(use-package docker
  :defer t
  :bind (:map wjb-map
              ("d" . docker))
  :config
  (setq docker-container-ls-arguments ""
        docker-container-default-sort-key ""))

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
  (defun wjb/disable-yas-minor-mode ()
    (yas-minor-mode -1))
  (add-hook 'term-mode-hook #'wjb/disable-yas-minor-mode))

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
  :diminish
  :ensure t
  :config
  (beginend-global-mode)
  (diminish 'beginend-global-mode)
  (diminish 'beginend-prog-mode))

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
  :disabled
  :commands (sane-term sane-term-create)
  :bind (("C-c s" . sane-term)
         ("C-c S" . sane-term-create))
  :ensure t
  :defer t)

(use-package pcomplete)

;; - a simple list gathers from all.
;; - a list with :with only gathers if the(/a?) backend before :with succeeds at the prefix command (more about with: https://github.com/company-mode/company-mode/issues/79)
;; - TODO: use delete-consecutive-dups and company-transformers to remove duplicates (see https://github.com/company-mode/company-mode/issues/528)

;; If the group contains keyword ':with', the backends listed after this
;; keyword are ignored for the purpose of the 'prefix' command.
;; but I'm not sure whether the first one doesn't return prefix, the :with ones will be or not be called either?

;; :with means completions unconditionally; whereas the default is to only use them if
;; they return the same prefix as the first defined checker in the group

;; elisp-completion-at-point
;; C-M-i is completion-at-point. How is it configured? maybe assume it is configred will for each mode.


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

;; commands:
;; company-complete
;; company-complete-common-or-cycle
;; company-other-backend
;; company-diag

;; - company-yasnippet -- specific binding for this
;; - company-shell
;; - company-web

(use-package company
  :defer t
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 4)
  (company-show-numbers nil)
  (company-tooltip-align-annotations 't)
  (company-dabbrev-downcase nil)

  :config
  (global-company-mode t)
  (make-variable-buffer-local 'company-backends)

  (setq company-selection-wrap-around t)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)

  (defun wjb/set-company-minimum-prefix-length ()
    (setq-local company-minimum-prefix-length 3))
  (add-hook 'prog-mode-hook #'wjb/set-company-minimum-prefix-length)
  (add-hook 'restclient-mode-hook #'wjb/set-company-minimum-prefix-length)

  ;; trial:
  (company-statistics-mode -1)
  (company-quickhelp-mode -1)

  (global-set-key (kbd "H-0 y") #'company-yasnippet)
  (global-set-key (kbd "C-c y") #'company-yasnippet)
  (global-set-key (kbd "C-c C-y") #'company-yasnippet))

;; trial:
(use-package company-flx
  :disabled
  :after company
  :config
  (company-flx-mode -1))

(use-package company-buffer-line
  :after company
  :commands (company-same-mode-buffer-lines)
  :bind ("C-x C-l" . company-same-mode-buffer-lines))

(use-package company-emoji
  :after company)

(use-package company-restclient
  :hook restclient-mode
  :after (company restclient))

(use-package company-ctags
  :after company
  :config
  (company-ctags-auto-setup)
  ;; rjsx-mode descends from js2-mode so I think this will cover both:
  (push 'js2-mode company-ctags-modes))

(defvar wjb/company-backends-original
  '(company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-dabbrev)
  "Original value of company-backends, fwiw.")

(defun wjb/company-backends-generic ()
  "Try some backend orderings."
  ;; mode-specific, smart
  (let (zing (list))
    (dolist
        ;; last ends up first
        (backend '(company-clang company-xcode company-cmake company-capf company-shell company-restclient company-css company-tide) zing)
      (if (equal list 'company-capf)
        (push
         (list backend 'company-dabbrev-code :with 'company-dabbrev 'company-emoji 'company-keywords)
         zing)

        (push
         (list backend :with 'company-dabbrev-code 'company-keywords)
         zing)))

    ;; generic
    (setq zing (append zing '(company-files)))
    ;; fallback backends -- likely to return in almost all cases
    (setq zing (append zing
               '(
                 ;; code
                 (company-dabbrev-code company-gtags company-ctags company-keywords)
                 ;; text
                 (company-emoji company-dabbrev)
                 )
               ))
    (setq-default company-backends zing)))

(wjb/company-backends-generic)

;; I think this ordering is good with manual cycling
;; prog:
;; (mode-primary :with :separate company-ctags company-capf company-keywords company-dabbrev-code)
;; text:
;; (company-capf company-keywords company-dabbrev-code company-dabbrev)
;; end:
;; (company-emoji company-capf company-dabbrev-code company-ctags company-keywords)
;; (company-files company-emoji company-dabbrev)

;; (setq completion-at-point-functions 'elisp-completion-at-point)

;; - tags-completion-at-point-function doesn't seem to work with company, and
;;   company has etags and ctags backends already, so it's unnecessary.
;; - pcomplete-completions-at-point seems to work only in org-mode, otherwise complains about pcomplete-here.

;; (setq completion-at-point-functions '())
;; (remove-hook completion-at-point-functions 'tags-completion-at-point-function)
;; (add-hook completion-at-point-functions #'pcomplete-completions-at-point)

(defvar wjb/company-backends-js)
(setq wjb/company-backends-js
  '((company-tide :with :separate company-ctags company-capf company-keywords company-dabbrev-code)
    (company-ctags company-capf company-keywords company-dabbrev-code company-dabbrev)))

(defvar wjb/company-backends-org)
;; todo get company-capf working: pcomplete-completions-at-point. Maybe I need
;; to teach pcomplete what to do?
(setq wjb/company-backends-org
      '(company-emoji company-files company-capf company-dabbrev-code company-dabbrev))
(setq wjb/company-backends-md wjb/company-backends-org)

(defvar wjb/company-backends-css)
(setq wjb/company-backends-css
      '((company-css :with :separate company-capf company-keywords company-dabbrev-code)
        (company-ctags company-capf company-keywords company-dabbrev-code company-dabbrev)))

(defvar wjb/company-backends-el)
(setq wjb/company-backends-el
      '((company-capf :with :separate company-keywords company-dabbrev-code)
        (company-ctags company-keywords company-dabbrev-code company-dabbrev)))

(use-package compdef
  :disabled
  :load-path "elisp/compdef"
  :config

  (compdef
   :modes #'org-mode
   :company '(company-dabbrev company-capf company-emoji)
   :capf '(#'tags-completion-at-point-function)))(require 'key-bindings)

(use-package web-mode
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
  (require 'company-web-html)
  (require 'company-web-jade)
  (defun wjb/web-mode-company ()
    (set (make-local-variable 'company-backends)
         '((company-web-html :with company-dabbrev-code company-gtags company-ctags company-keywords)))
    (company-mode t))
  (add-hook 'web-mode-hook #'wjb/web-mode-company))

(require 'setup-word)

(require 'setup-markdown)

;; Generate a TOC from a markdown file: M-x markdown-toc-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x markdown-toc-generate-toc
(use-package markdown-toc
  :defer t)

(use-package shell-script-mode
  :defer t
  :mode "\\.bash*")

(use-package elisp-demos
  :config
  ;; this is for normal help:
  ;; (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; this is specific to helpful:
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; see https://www.emacswiki.org/emacs/Edit_with_Emacs
(use-package edit-server
  :disabled
  :defer 6
  :config
  (setq edit-server-new-frame nil)
  (defun wjb/save-edit-server () (kill-ring-save (point-min) (point-max)))
  (add-hook 'edit-server-done-hook #'wjb/save-edit-server)
  (add-hook 'edit-server-start-hook #'gfm-mode)
  (edit-server-start))

(use-package atomic-chrome
  :disabled
  :defer 5
  :config
  (atomic-chrome-start-server))

(use-package google-this
  :defer t
  ;; C-c / n|SPC|l
  :diminish google-this-mode
  :config
  (google-this-mode 1))

;; better than yafolding
(use-package origami
  :defer t
  :config
  (define-key origami-mode-map (kbd "C-<return>") #'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "M-<return>") #'origami-show-only-node)
  (define-key origami-mode-map (kbd "H-<return>") #'origami-toggle-all-nodes)

  (add-hook 'prog-mode-hook #'origami-mode))

;; better than vimish-fold
(use-package yafolding
  :disabled
  :config
  (add-hook 'prog-mode-hook #'yafolding-mode))

(use-package vimish-fold
  :disabled
  :config
  (vimish-fold-global-mode 0)
  ;; TODO: make this only true in vimish-fold key map
  (global-set-key (kbd "C-c `") #'vimish-fold-toggle)
  (global-set-key (kbd "C-c ~") #'vimish-fold))

(use-package npm-mode
  :commands (npm npm-mode)
  :load-path "elisp/npm-mode"
  :diminish
  ;; Prefer dir locals activation: https://github.com/mojochao/npm-mode#project-activation
  ;; :config
  ;; (npm-global-mode)
  )

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
(use-package shell
  :after comint
  :config
  ;; Fix junk characters in shell-mode. This doesn't work to do ANSI color in
  ;; compilation mode, though. Maybe compilation mode doesn't use comint-mode, or
  ;; only sort of uses it?
  (add-hook 'shell-mode-hook
            'ansi-color-for-comint-mode-on)
  ;; want this for shell but not for compilation
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (setq comint-scroll-show-maximum-output nil)

  (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<down>") 'comint-next-input))

(defun endless/send-self ()
  "Send the pressed key to the current process."
  (interactive)
  (endless/send-input
   (apply #'string
          (append (this-command-keys-vector) nil))))

;; - Enter is for compile-goto-error, unless I want to rebind it to something else
;;   - Maybe I could somehow remap so that pressing C-j or C-m tells Jest to do Enter?
;; - p and t are for typing in patterns, so they don't workm
;; - a w f o are for changing watch mode
;; - TODO don't echo the key I press
(dolist (key '("\C-d" "\C-j" "a" "w" "f" "o"))
  (define-key compilation-mode-map key
    #'endless/send-self)
  ;; I have jest set to run compilation-minor-mode, so this is the map that is active in my jest buffers:
  (define-key compilation-minor-mode-map key
    #'endless/send-self)
  ;; (define-key global-map key
  ;;   #'endless/send-self)
  )
  ;; (unbind-key "\C-c" compilation-minor-mode-map)

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

  ;; Make *compilation* buffer use visual-line-mode
  ;; TODO: make a key binding for turning vlmode on and off
  (add-hook 'compilation-mode-hook
            (lambda () (visual-line-mode 1)
              (npm-mode 1)))

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
    (xristos/disable-font-lock)
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)

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
    "Switch to a project's jest buffer, falling back to last compilation buffer.*"
    (interactive)
    (let* ((projectile-current-project-name (projectile-default-project-name (projectile-project-root)))
           (buffer-name (format "*jest*<%s>" projectile-current-project-name)))
      (if (buffer-live-p (get-buffer buffer-name))
          (switch-to-buffer buffer-name)
        (funcall-interactively #'wjb/switch-to-last-compilation-buffer))))

  (defun wjb/switch-to-last-compilation-buffer ()
    "Switch to last compilation buffer, falling back to *compilation*."
    (interactive)
    (if (and wjb/last-compilation-buffer (buffer-live-p (wjb/last-compilation-buffer)))
        (switch-to-buffer wjb/last-compilation-buffer)
      (funcall-interactively #'wjb/switch-to-compilation-buffer)))

  (defun wjb/switch-to-compilation-buffer ()
    "Switch to *compilation*"
    (interactive)
    (let ((comp-buffer-name "*compilation*"))
      (if (buffer-live-p (get-buffer comp-buffer-name))
          (switch-to-buffer comp-buffer-name)
        (message "no compilation buffer"))))

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
  :load-path "elisp/emacs-jest"
  :bind (
         :map jest-mode-map
         ([remap compile] . jest-popup)
         ([remap recompile] . jest-repeat)

         ;; TODO: none of this seems to be needed anymore. Delete it.
         ;; ("g" . jest-repeat)
         ;; ("M-n" . compilation-next-error)
         ;; ("M-p" . compilation-previous-error)
         ;; ("C-c RET" . jest-popup)
         ;; ("C-c <C-return>" . jest-repeat)
         ;; :map compilation-minor-mode-map
         ;; ;; TODO not sure both of these are required -- not sure what the
         ;; ;; defaults are for this map
         ;; ("C-c RET" . jest-popup)
         ;; ("C-c C-<return>" . jest-repeat)
         ;; ([remap compile] . jest-popup)
         ;; ([remap recompile] . jest-repeat)
         )
  :config
  (setq jest-pdb-track nil)
  ;; Not sure which is preferable to use. shell-minor seems to not have
  ;; as many key bindings I want, however, it allows sending input into the
  ;; buffer.
  ;; (remove-hook 'jest-mode-hook #'compilation-shell-minor-mode)
  (add-hook 'jest-mode-hook #'compilation-minor-mode))

(defcustom jest-compile-function 'jest-popup
  "Command to run when compile and friends are called."
  :group 'jest
  :type 'function)

;; change
;; (setq jest-compile-function #'jest-popup)
;; (setq jest-compile-function #'jest-file-dwim)

(defun jest-compile-command ()
  (interactive)
  (call-interactively (symbol-value 'jest-compile-function)))

(defun jest-minor-inhibit-self ()
  "Add this hook to modes that should not use jest-minor but otherwise would."
  (add-hook 'after-change-major-mode-hook
            (lambda () (jest-minor-mode 0))
            :append :local))

(add-hook 'grep-mode-hook 'jest-minor-inhibit-self)

;; I have been activating this via dir-locals, though that also turns it on for
;; other kinds of buffers (non-JS), like grep, which is annoying because its bindings shadow recompile.
;;;###autoload
(define-minor-mode jest-minor-mode
  "Minor mode to run jest-mode commands for compile and friends."
  :lighter " Jest Minor"
  :keymap (let ((jest-minor-mode-keymap (make-sparse-keymap)))
            (define-key jest-minor-mode-keymap [remap compile] 'jest-compile-command)
            (define-key jest-minor-mode-keymap [remap recompile] 'jest-repeat)
            (define-key jest-minor-mode-keymap [remap projectile-test-project] 'jest-compile-command)
            (define-key jest-minor-mode-keymap (kbd "C-c ;") 'jest-file-dwim)
            jest-minor-mode-keymap))

(diminish 'jest-minor-mode)

;; unbind
;; (fmakunbound 'jest-minor-mode)
;; (makunbound 'jest-minor-mode)
;; (makunbound 'jest-minor-mode-map)
;; (makunbound 'jest-compile-command)
;; (makunbound 'jest-compile-function)

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

  (dolist (hook '(web-mode-hook))
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

(use-package eglot
  :defer t
  :config
  ;; TODO: find a language server that actually works with JSX
  ;; (add-to-list 'eglot-server-programs '(rjsx-mode . ("typescript-language-server" "--stdio")))
  )

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
  :commands company-lsp)

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
        hungry-delete-except-modes '(help-mode minibuffer-inactive-mode calc-mode org-mode))

  ;; TODO: turn off in some modes:
  ;; - org-mode?
  ;; - whitespace sensitive modes?
  (global-hungry-delete-mode))

(use-package nginx-mode
  :defer
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
  (global-set-key "\M-{" #'paredit-wrap-curly)
  )

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

(use-package zoom
  :config
  (global-set-key (kbd "C-x +") 'zoom))

(use-package eyebrowse
  :after zoom
  :init
  ;; (setq eyebrowse-keymap-prefix (kbd "C-c C-z"))
  (setq eyebrowse-keymap-prefix (kbd "H-w"))
  :config
  (define-key eyebrowse-mode-map (kbd "H-w n") 'eyebrowse-next-window-config)
  (define-key eyebrowse-mode-map (kbd "H-w p") 'eyebrowse-prev-window-config)
  (define-key eyebrowse-mode-map (kbd "H-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "H-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "H-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "H-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "H-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-'") 'eyebrowse-last-window-config)
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        eyebrowse-mode-line-style 'always)
  (set-face-attribute 'eyebrowse-mode-line-active nil :foreground "#9ccc65")

  ;; ;; zoom uses advice-add after
  ;; ;;   (advice-add #'select-window :after #'zoom--handler)

  ;; (defun wjb/turn-off-zoom-mode (slot)
  ;;   "This is zoom--off but without the final cleanup it does."
  ;;   ;; (message "turning off zoom mode")
  ;;   "Disable hooks and advices and evenly balance the windows."
  ;;   ;; unregister the zoom handler
  ;;   (remove-function pre-redisplay-function #'zoom--handler)
  ;;   ;; enable mouse resizing
  ;;   (advice-remove #'mouse-drag-mode-line #'ignore)
  ;;   (advice-remove #'mouse-drag-vertical-line #'ignore)
  ;;   (advice-remove #'mouse-drag-header-line #'ignore)
  ;;   (setq zoom-mode nil))

  ;; (advice-add #'eyebrowse-switch-to-window-config :before #'wjb/turn-off-zoom-mode)

  ;; (defun wjb/post-ebhook ()
  ;;   (let* ((current-slot (eyebrowse--get 'current-slot))
  ;;          (window-configs (eyebrowse--get 'window-configs))
  ;;          (current-tag (nth 2 (assoc current-slot window-configs))))
  ;;     ;; (message (format "switched to: %s" current-tag))
  ;;     (cond ((equal current-tag "sql") (progn
  ;;                                        (zoom-mode)))
  ;;           ((equal current-tag "rest") (progn
  ;;                                         (zoom-mode)))
  ;;           ;; (t (progn
  ;;           ;; (message "leaving zoom deactivated")))
  ;;           )))

  ;; (add-hook 'eyebrowse-post-window-switch-hook #'wjb/post-ebhook)


  ;; (setq zoom-ignored-buffer-names '("*HTTP Response*"))
  ;; (setq zoom-ignored-buffer-name-regexps '())

  ;; TODO my hack
  (defun zoom--resize-one-dimension (size-hint-cons horizontal)
    "Resize one dimension of the selected window according to the user preference.

Argument SIZE-HINT-CONS is the size hint provided by the user.

Argument HORIZONTAL determines whether the window should be
resized horizontally or vertically."
    (let* ((size-hint
            (if horizontal (car size-hint-cons) (cdr size-hint-cons)))
           (frame-size
            (if horizontal (frame-width) (frame-height)))
           ;; use the total size (including fringes, scroll bars, etc.) for ratios
           ;; and the body size for absolute values
           (window-size
            (if (floatp size-hint)
                (if horizontal (window-total-width) (window-total-height))
              (if horizontal (window-body-width) (window-body-height))))
           ;; either use an absolute value or a ratio
           (min-window-size
            (if (floatp size-hint) (round (* size-hint frame-size)) size-hint))
           ;; do not shrink the window if it is already large enough
           ;; (desired-delta (max (- min-window-size window-size) 0))
           ;; HACK: DO shrink the window if by >1!
           (desired-delta (- min-window-size window-size))
           ;; fall back to the maximum available if the windows are too small
           (delta (window-resizable nil desired-delta horizontal)))
        ;; actually resize the window
        (window-resize nil delta horizontal)))

  ;; half-width windows
  ;; (setq zoom-size '(0.5 . 1.0))

  (defun zoom-sizer ()
    (let* ((buf-name (buffer-name)))
      (cond ((s-equals? buf-name "*scratch*") '(1.0 . 1.0))
            ;; due to rounding(?), 0.6 and 0.4 produce different adjustments
            ;; from balanced (evenly split) windows. These values produce
            ;; identical adjustments.
            ((s-equals? buf-name "*HTTP Response*") '(0.39 . 1.0))
            ((s-ends-with? ".rest" buf-name) '(0.62 . 1.0))

            ((s-equals? buf-name "*SQL*") '(0.44 . 1.0))
            ((s-ends-with? ".sql" buf-name) '(0.57 . 1.0))

            ((s-starts-with? "*" buf-name) '(0.4 . 1.0))
            (t '(1.0 . 1.0)))
      ))

  (setq zoom-size #'zoom-sizer)

  (eyebrowse-mode t))

(use-package eyezoom
  :load-path "elisp/eyezoom"
  :after (eyebrowse zoom)
  :config
  (setq eyezoom-tags-that-zoom '("sql" "rest")))

(defhydra hydra-eyebrowse (:color blue)
  "
  ^Window configs^
  ^
  %s(eyebrowse-mode-line-indicator)
  ^
  ^Modify^                          ^Switch
  -------------------------------------------------------------
  _,_ left window config            _0_ switch to window config
  _._ right window config           _1_ switch to window config
  ↦ previous window config  ^^        ...
  _r_ename current window config    _9_ switch to window config
  _c_reate new window config
  _C_lose current window config
  ^^
  "
  ("," eyebrowse-prev-window-config nil)
  ("." eyebrowse-next-window-config nil)
  ("<tab>" eyebrowse-last-window-config nil)
  ("r" eyebrowse-rename-window-config nil)
  ("c" eyebrowse-create-window-config nil)
  ("C" eyebrowse-close-window-config nil)
  ("0" eyebrowse-switch-to-window-config-0 nil)
  ("1" eyebrowse-switch-to-window-config-1 nil)
  ("2" eyebrowse-switch-to-window-config-2 nil)
  ("3" eyebrowse-switch-to-window-config-3 nil)
  ("4" eyebrowse-switch-to-window-config-4 nil)
  ("5" eyebrowse-switch-to-window-config-5 nil)
  ("6" eyebrowse-switch-to-window-config-6 nil)
  ("7" eyebrowse-switch-to-window-config-7 nil)
  ("8" eyebrowse-switch-to-window-config-8 nil)
  ("9" eyebrowse-switch-to-window-config-9 nil)
  )

(define-key global-map (kbd "M-0") 'hydra-eyebrowse/body)

(use-package pcre2el
  :commands reb-change-syntax)

;; TODO this messes with file paths (.., /) in Python, it should not apply
;; within strings
(use-package electric-operator
  :defer t
  :hook
  ((coffee-mode python-mode) . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'python-mode
                                        (cons "." nil)) ;; doesnt work as intended
  (electric-operator-add-rules-for-mode 'python-mode
                                        (cons "/" nil))
  (setq electric-operator-enable-in-docs t))

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

(setq term-suppress-hard-newline t)

;; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below).
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y")
              (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))

(require 'appearance)

;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "t" #'endless/toggle-theme)
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

(defvar initial-file (expand-file-name "init.el" user-emacs-directory))

(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

(use-package hi-lock
  :diminish)

(require 'wjb)

(defun wjb/after-init-hook ()
  (setq source-directory "/Users/william/scm/vendor/emacs-mac"
        find-function-C-source-directory "/Users/william/scm/vendor/emacs-mac/src")
  (treemacs))
(add-hook 'after-init-hook #'wjb/after-init-hook)

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package vterm
  :defer t
  ;; TODO: switch to melpa.
  :load-path "elisp/emacs-libvterm"
  :config
  (push "C-o" vterm-keymap-exceptions)
  (push "C-u" vterm-keymap-exceptions)
  ;; (push (kbd "C-<space>") vterm-keymap-exceptions)
  (vterm--exclude-keys vterm-keymap-exceptions)
  ;; hack: exclude will overwrite these, so they need to be re-defined. Would
  ;; be better if vterm defined them in a defun.
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
  (define-key vterm-mode-map (kbd "C-_")                 #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-/")                 #'vterm-undo)
  (define-key vterm-mode-map (kbd "M-.")                 #'vterm-send-meta-dot)
  (define-key vterm-mode-map (kbd "M-,")                 #'vterm-send-meta-comma)
  (define-key vterm-mode-map (kbd "C-c C-y")             #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-c C-c")             #'vterm-send-ctrl-c)
  (define-key vterm-mode-map (kbd "C-c C-l")             #'vterm-clear-scrollback)
  (define-key vterm-mode-map [remap self-insert-command] #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-c C-t")             #'vterm-copy-mode)

  (defun vterm-send-close-square-bracket ()
    "Sends `C-]' to libvterm."
    (interactive)
    (vterm-send-key "]" nil nil t))
  (define-key vterm-mode-map (kbd "C-]") #'vterm-send-close-square-bracket))

(add-hook 'vterm-mode-hook #'compilation-shell-minor-mode)

;; TODO: am I handling safe-local-variable-values in a sensible way?
;; look at purcell, etc.

;; TODO: Byte-recompile site-lisp-dir during some idle time after startup.
;; (byte-recompile-directory site-lisp-dir 0)
;; (byte-recompile-directory "/Users/william/.emacs.d/elpa" 0)
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

(advice-remove #'copy-to-register nil)
;; This doesn't seem to work bc copy-to-register must be moving things around
;; (advice-add #'copy-to-register :after (lambda (REGISTER START END &optional DELETE-FLAG REGION) (exchange-point-and-mark)))

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

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

(use-package indium
  :commands (indium-interaction-mode indium-connect)
  :config
  (setq indium-chrome-use-temporary-profile nil
        indium-client-debug nil ;; t
        indium-chrome-executable "/Applications/Google Chrome Beta Debugger.app/Contents/MacOS/Google Chrome Beta Debugger")
        ;; indium-chrome-executable (indium-chrome--default-executable)
)

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (set-face-background 'solaire-default-face
                       (color-darken-name
                        (face-attribute 'default :background) 4))
  (set-face-foreground 'solaire-default-face
                       (color-lighten-name
                        (face-attribute 'default :foreground) 3)))

(provide 'main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main.el ends here
