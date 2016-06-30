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

;; ========================================
;; Definitions.
;; ========================================

;; Set Emacs Lisp directory.
(setq site-lisp-dir
      (expand-file-name "elisp" user-emacs-directory))

;; Directories to open in dirtree on start. TODO This should be in custom.el,
;; but probably need to move when dirtree starts up to happen following init.el
;; being processed because custom.el isn't loaded until the very end.
;; TODO: Would be nice to start these closed instead of expanded.
(setq initial-dirs-to-open '())

;; An initial file to open if it exists.
(setq initial-file (expand-file-name "init.el"
                                     user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Set file containing machine-local customized settings.
(setq custom-file (expand-file-name "custom.el"
                                    user-emacs-directory))

;; Set custom markers.
;; Args:
;; 1. Marker.
;; 2. Register to store.
;; 3. Key bindings to set/clear marker.
;; 4. Insert/remove marker from current buffer?
;;
(setq wjb-custom-markers
      '(("NNN" ?n "" t)
        ;("MMM" ?m "" t)
        ("Server" ?s "" nil)
        ("Quiz View" ?q "" nil)
        ("Client" ?c "" nil)))

(setq wjb-test-config-buffer "test.coffee")

;; Custom grep-find via find-in-project.
(setq find-in-project-default-dir ".")

(set-register ?t "TODO ")
(set-register ?h "TODO HERE: ")

;; ========================================
;; Set up load-path.
;; ========================================
(add-to-list 'load-path site-lisp-dir t)
(add-to-list 'load-path (expand-file-name "setup-lisp" user-emacs-directory) t)
;(add-to-list 'load-path user-emacs-directory t) ;; Probably not needed, but
;commented out for now until known for sure.

;; Add all subdirs of site-lisp-dir.
(let ((default-directory site-lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))

;; ========================================
;; Settings.
;; ========================================

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
(require 'server)
(unless (server-running-p)
  (server-start))

;; Save desktop.
(desktop-save-mode 1)
(setq desktop-restore-eager 32)

;; Allow the very useful set-goal-column.
(put 'set-goal-column 'disabled nil)

;; 24-hour time.
(setq display-time-24hr-format t)

;; Tramp.
(setq tramp-default-method "ssh")

;; Sane backup files.
;; See: http://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   `(("." . ,(expand-file-name
              (concat user-emacs-directory "backups"))))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Keep auto-save files out of the filesystem.
;; See: http://emacswiki.org/emacs/AutoSave
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable interlocking files (interferes with watches).
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html
;; http://stackoverflow.com/questions/5738170/
(setq create-lockfiles nil)

;; Set PAGER and EDITOR so git doesn't complain: "terminal is not
;; fully functional".
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

;; Allow downcasing regions.
(put 'downcase-region 'disabled nil)

;; Keyboard for Macs.
(setq-default mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(require 'setup-grep)

;; Workaround for a bug in emacs' http fetching. See:
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-12/msg00196.html
(setq url-http-attempt-keepalives nil)

;; From purcell.
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; ========================================
;; Machine-local custom configuration.
;; ========================================

(load custom-file t t)

;; ========================================
;; Appearance.
;; ========================================

(require 'appearance)

;; ========================================
;; Package management.
;; ========================================

(require 'setup-package)

;; For Emacs Lisp not available as submodule or package (e.g., windata.el).
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/"))

;; ========================================
;; Sane defaults.
;; ========================================

;; From:
;; https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el
(require 'sane-defaults)

;; ========================================
;; Custom defuns.
;; ========================================

(require 'defuns)

;; ========================================
;; Misc.
;; ========================================

(require 'setup-flycheck)

;; Set shortcuts to clear custom markers. Requires lexical binding.
(dolist (marker-data wjb-custom-markers)
        (let ((marker (pop marker-data))
              (marker-register (pop marker-data))
              (marker-key (pop marker-data))
              (handle-in-current-buffer (pop marker-data)))
          (progn
            (set-register marker-register marker)
            (global-set-key marker-key (lambda (arg)
                                          (interactive "P")
                                          (wjb-toggle-marker arg marker handle-in-current-buffer))))))

;; Open grep results in the same frame. See:
;; http://stackoverflow.com/questions/2299133/emacs-grep-find-link-in-same-window/2299261#2299261
(eval-after-load "compile"
  '(defun compilation-goto-locus (msg mk end-mk)
     "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non-nil, mark is set there
and overlay is highlighted between MK and END-MK."
     ;; Show compilation buffer in other window, scrolled to this error.
     (let* ((from-compilation-buffer (eq (window-buffer (selected-window))
                                         (marker-buffer msg)))
            ;; Use an existing window if it is in a visible frame.
            (pre-existing (get-buffer-window (marker-buffer msg) 0))
            (w (if (and from-compilation-buffer pre-existing)
                   ;; Calling display-buffer here may end up (partly) hiding
                   ;; the error location if the two buffers are in two
                   ;; different frames.  So don't do it if it's not necessary.
                   pre-existing
                 (let ((display-buffer-reuse-frames t)
                       (pop-up-windows t))
                   ;; Pop up a window.
                   (display-buffer (marker-buffer msg)))))
            (highlight-regexp (with-current-buffer (marker-buffer msg)
                                ;; also do this while we change buffer
                                (compilation-set-window w msg)
                                compilation-highlight-regexp)))
       ;; Ideally, the window-size should be passed to `display-buffer' (via
       ;; something like special-display-buffer) so it's only used when
       ;; creating a new window.
       (unless pre-existing (compilation-set-window-height w))

       (switch-to-buffer (marker-buffer mk))

       ;; If narrowing gets in the way of going to the right place, widen.
       (unless (eq (goto-char mk) (point))
         (widen)
         (goto-char mk))
       (if end-mk
           (push-mark end-mk t)
         (if mark-active (setq mark-active)))
       ;; If hideshow got in the way of
       ;; seeing the right place, open permanently.
       (dolist (ov (overlays-at (point)))
         (when (eq 'hs (overlay-get ov 'invisible))
           (delete-overlay ov)
           (goto-char mk)))

       (when highlight-regexp
         (if (timerp next-error-highlight-timer)
             (cancel-timer next-error-highlight-timer))
         (unless compilation-highlight-overlay
           (setq compilation-highlight-overlay
                 (make-overlay (point-min) (point-min)))
           (overlay-put compilation-highlight-overlay 'face 'next-error))
         (with-current-buffer (marker-buffer mk)
           (save-excursion
             (if end-mk (goto-char end-mk) (end-of-line))
             (let ((end (point)))
               (if mk (goto-char mk) (beginning-of-line))
               (if (and (stringp highlight-regexp)
                        (re-search-forward highlight-regexp end t))
                   (progn
                     (goto-char (match-beginning 0))
                     (move-overlay compilation-highlight-overlay
                                   (match-beginning 0) (match-end 0)
                                   (current-buffer)))
                 (move-overlay compilation-highlight-overlay
                               (point) end (current-buffer)))
               (if (or (eq next-error-highlight t)
                       (numberp next-error-highlight))
                   ;; We want highlighting: delete overlay on next input.
                   (add-hook 'pre-command-hook
                             'compilation-goto-locus-delete-o)
                 ;; We don't want highlighting: delete overlay now.
                 (delete-overlay compilation-highlight-overlay))
               ;; We want highlighting for a limited time:
               ;; set up a timer to delete it.
               (when (numberp next-error-highlight)
                 (setq next-error-highlight-timer
                       (run-at-time next-error-highlight nil
                                    'compilation-goto-locus-delete-o)))))))
       (when (and (eq next-error-highlight 'fringe-arrow))
         ;; We want a fringe arrow (instead of highlighting).
         (setq next-error-overlay-arrow-position
               (copy-marker (line-beginning-position)))))))

;; ========================================
;; Some hooks.
;; ========================================

;;(add-hook 'coffee-mode-hook 'smart-indent-rigidly-mode) ;; clobbers TAB for yasnippet/expand

;; Force 2-space indentation in css-mode.
(add-hook 'css-mode-hook
          (function
           (lambda ()
             (setq css-indent-offset 2))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (abbrev-mode 1))))

;; ========================================
;; Require/autoload and config packages.
;; ========================================

;; ibuffer.
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Smex.
(when (require 'smex nil t)
  (smex-initialize))

;; Ido.
(when (require 'ido nil t)
  (eval-after-load 'ido '(require 'setup-ido)))

;; Imenu.
(when (require 'imenu nil t)
  (autoload 'idomenu "idomenu" nil t))

(defadvice ido-imenu (before push-mark activate)
    (push-mark))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; Dired.
;; dired-jump is useful.
(require 'dired-x)

(when (require 'dired+ nil t)
  (eval-after-load 'dired+ '(require 'setup-dired+)))

;; Org-mode.
(require 'org-install)
(eval-after-load 'org '(require 'setup-org))

;; Magit.
(autoload 'magit-status "magit")
(autoload 'magit-log "magit")
(eval-after-load 'magit '(require 'setup-magit))

;; Python.
(require 'setup-python)

;; Shell.
;(eval-after-load 'shell '(require 'setup-shell))

;(require 'setup-hippie)

;; Yasnippet.
;(require 'setup-yasnippet)
;; Work-around for tab complaining when yas is active in ansi-term. See:
;; https://github.com/capitaomorte/yasnippet/issues/289
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;; Rainbow mode.
(when (require 'rainbow-mode nil t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

(require 'setup-projectile)

;(require 'setup-perspective)
;(require 'setup-ffip)
;(require 'setup-html-mode)
;(require 'setup-paredit)

;; Anything.
;(require 'helm-config)
;(helm-mode 1)

(defun mine-goto-symbol-at-point ()
  "Will navigate to the symbol at the current point of the cursor"
  (interactive)
  (ido-goto-symbol (thing-at-point 'symbol)))

;; Map files to modes
(require 'mode-mappings)

;; Lua mode.
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Text and fill modes.
(defun textful-settings ()
        (goto-address-mode 1)
        (auto-fill-mode 1)
        (set-fill-column 80))

(add-hook 'markdown-mode-hook 'textful-settings)
(add-hook 'rst-mode-hook 'textful-settings)
(add-hook 'text-mode-hook 'textful-settings)

(add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 80)))
(add-hook 'js2-mode-hook '(lambda () (set-fill-column 80)))

;; Paired tick is useful in some modes.
;; TODO: This throws errors if run here. Probably needs to go into mode hooks.
;; (modify-syntax-entry ?\` "$" markdown-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" text-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" rst-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" org-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" coffee-mode-syntax-table)

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
  ;; (add-hook 'dired-mode-hook 'turn-off-fci)
  ;; (add-hook 'edit-server-start-hook 'turn-off-fci)
  ;; (add-hook 'edit-server-start-hook 'my-edit-server-hook)
  ;; (add-hook 'edit-server-edit-mode-hook 'my-edit-server-hook)
  )

(require-package 'nvm)
(require 'nvm)

;; js2-mode
(eval-after-load 'js2-mode '(require 'setup-js2-mode))

;; json
(require 'json nil t)

;; Smart-tab. See: https://raw.github.com/genehack/smart-tab/master/smart-tab.el
(when (require 'smart-tab nil t)
  (global-smart-tab-mode 1))

(when (require 'anzu-mode nil t)
  (global-anzu-mode 1))

;; multiple-cursors.
;;
;; See: https://github.com/magnars/multiple-cursors.el
;;

(global-set-key (kbd "C-x t") 'set-rectangular-region-anchor)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-M->") 'mc/edit-ends-of-lines)

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; smart-forward
;; See: https://github.com/magnars/smart-forward.el
(when (require 'smart-forward nil t)
  (global-set-key (kbd "M-<up>") 'smart-up)
  (global-set-key (kbd "M-<down>") 'smart-down)
  (global-set-key (kbd "M-<left>") 'smart-backward)
  (global-set-key (kbd "M-<right>") 'smart-forward))

;; Scheme.
;; (setq scheme-program-name
;;       "/usr/local/bin/racket")
(setq scheme-program-name
      "/Applications/MIT-GNUScheme.app/Contents/Resources/mit-scheme")
;; Common Lisp.
;; Inferior Lisp.
(set-variable 'inferior-lisp-program "/usr/local/bin/clisp")

;; SLIME.
(add-to-list 'load-path "~/scm/slime/")
;; Autoload on use. See: http://ambience.info.ucl.ac.be/slime.html
(when (load "slime-autoloads" t)
  (setq slime-auto-connect 'always)
  (slime-setup '(slime-fancy inferior-slime)))
;(require 'slime-autoloads)
;(slime-setup '(slime-fancy))

;; EPG.
(require 'epa-file)
(epa-file-enable)

;; Edit-server for Edit-in-Emacs Chrome extension.
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start)
  (autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
  (autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer))

;; Yasnippet.
(when (require 'yasnippet nil t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/yasnippet-coffee-script-snippets/" t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/js-snippets" t)
  (yas-global-mode 1))

;; Linum: put spaces around line numbers.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; RVM.
(when (require 'rvm nil t)
  (rvm-use-default)) ;; use rvm's default ruby for the current Emacs session

;; From http://emacs.stackexchange.com/a/11064
(defun my-keyboard-quit-advice (fn &rest args)
  (let ((region-was-active (region-active-p)))
    (unwind-protect
         (apply fn args)
      (when region-was-active
        (activate-mark t)))))

(advice-add 'keyboard-quit :around #'my-keyboard-quit-advice)

;; ========================================
;; Key bindings.
;; ========================================

(require 'key-bindings)

;; ========================================
;; Final.
;; ========================================

;; Byte-recompile site-lisp-dir.
(byte-recompile-directory site-lisp-dir 0)

(when is-mac (require 'setup-mac))

(eval-after-load 'smart-parens-mode '(require 'setup-smartparens))
(eval-after-load 'coffee-mode '(require 'setup-coffee))

;; Load something that might be useful.
(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

;; Open up some dirs in dirtree if it's available.
(defun do-setup-dirtree ()
  (when (and (require 'tree-mode nil t) (require 'dirtree nil t))
    (let ((dirtree-buffer "*dirtree*"))
      (dolist (dir initial-dirs-to-open)
        (when (file-accessible-directory-p dir)
          (dirtree dir dirtree-buffer)))
      ;; Dedicate window and resize.
      (let ((window (get-buffer-window dirtree-buffer)))
        (set-window-fringes window 0 0 nil)
        (set-window-dedicated-p window t)
        ;; TODO: Resize more intelligently.
        (adjust-window-trailing-edge window -5 t)
        (cd "~/scm/sd")))))

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
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(add-hook 'dirtree-mode-hook 'hidden-mode-line-mode)

;(add-hook 'after-init-hook (lambda () (do-setup-dirtree)))
;(do-setup-dirtree)

;; Paired tick is useful in some modes.
;; TODO: Probably Can't run these until the mode has been loaded or something.
;; TODO: Could use smartparens for this instead.
;; (modify-syntax-entry ?\` "$" markdown-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" text-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" rst-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" org-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" coffee-mode-syntax-table)

(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)

(require 'discover)
(global-discover-mode 1)

(add-hook 'after-init-hook 'sml/setup)
(eval-after-load 'smart-mode-line (lambda () (load "setup-modeline")))

(require 'setup-ediff)
(require 'setup-docker)
(require 'setup-webmode)

(setq aw-keys '(?a ?f ?j ?l))

(provide 'init)

;(do-setup-dirtree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
