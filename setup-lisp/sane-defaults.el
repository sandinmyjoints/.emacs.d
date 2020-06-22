;;; sane-defaults.el --- Set up some useful defaults.
;;
;; Filename: sane-defaults.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Dec 31 17:03:23 2014 (-0800)
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
;; Derived from: https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el
;;
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

(setq-default bidi-display-reordering 'left-to-right)

(setq inhibit-startup-echo-area-message "william")

;; Turn on/off display stuff.
;;
(setq-default visible-bell t
              font-lock-maximum-decoration t
              truncate-partial-width-windows nil)

;; Prevent vulnerability. See https://bugzilla.redhat.com/show_bug.cgi?id=1490409.
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; Workaround for a bug in emacs' http fetching. Maybe not needed anymore. See:
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-12/msg00196.html
;;(defvar url-http-attempt-keepalives nil)

;; Allow the very useful set-goal-column.
(put 'set-goal-column 'disabled nil)

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

;; Allow casing regions.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Mode-line display-time formatting.
(defvar display-time-24hr-format t)
(defvar display-time-default-load-average nil)
(defun wjb/display-time ()
  (setq display-time-24hr-format t)
  (setq display-time-default-load-average nil))
(add-hook 'display-time-hook #'wjb/display-time)

;; Allow pasting selection outside of Emacs/kill to system clipboard.
(defvar x-select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(set-language-environment "UTF-8")

;; Don't want to see garbage in term-mode.
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Always display line and column numbers
(setq line-number-mode t
      column-number-mode t)

;; Lines should be 80 characters wide, not 72. However, with the font size I'm
;; using, on my laptop screen, there are 78 columns. And this builds in a little
;; wiggle room.
(setq-default fill-column 78)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Set up 2-space tabs.
;; See: http://stackoverflow.com/a/1819405/599258
(setq-default tab-width 2)
;;(setq indent-line-function 'insert-tab)
;; TODO: check if this is what I really want in all modes:
(setq indent-line-function 'indent-relative-maybe)

;; Show empty lines after buffer end?
(setq-default indicate-empty-lines nil
              indicate-buffer-boundaries nil)

;; Don't break lines for me, please
;; what is relationship of this to visual-line-mode? and truncate-partial-width-windows?
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Easily navigate sillycased words
(use-package subword
  :diminish
  :config
  (global-subword-mode 1))

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
  :defer 1
  :config
  (setq uniquify-buffer-name-style 'post-forward))

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;;; Fewer new frames and pop-up windows.
(setq pop-up-frames nil)
(setq pop-up-windows nil)

;;; Never split windows for me, split-window-sensibly.
(setq split-height-threshold nil)
(setq split-width-threshold nil)

(defadvice split-window-vertically
    (after my-window-splitting-advice first () activate)
    (set-window-buffer (next-window) (other-buffer)))

;;; Default for new buffers.
(when (require 'markdown-mode nil t)
  (setq-default major-mode 'fundamental-mode))

;;; Avoid backslash madness.
(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

;;; Emacs 24 and up.
(when (>= emacs-major-version 24)
  ;; Pair parens and other delimiters.
  ;; Turn off electric-pair-mode if using smart-parens.
  (electric-pair-mode)
  ;; Scroll up without warning the first time.
  (setq scroll-error-top-bottom t))

(setq ispell-dictionary "american")

;; Blink forever.
(setq blink-cursor-blinks 0)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "saved-places" user-emacs-directory))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; Use human readable Size column instead of original one.
(eval-after-load 'ibuffer
  '(progn (define-ibuffer-column size-h
            (:name "Size" :inline t)
            (cond
             ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
             ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
             (t (format "%8d" (buffer-size)))))

          ;; ;; Modify the default ibuffer-formats.
          (setq ibuffer-formats
                '((mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size-h 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        filename-and-process)))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

;; Narrow to region is useful.
(put 'narrow-to-region 'disabled nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; This unfolds all before saving, so that deleting trailing whitespace works as
;; expected.
;;
;; (add-hook 'before-save-hook (lambda ()
;;                               (if (fboundp 'yafolding-show-all)
;;                                   (yafolding-show-all))
;;                               (delete-trailing-whitespace)))

;; This attempts to only delete-trailing-whitespace if there aren't any folds.
;; Not sure it's bug-free, though.
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (and
;;              (fboundp 'yafolding-get-overlays)
;;              (not (yafolding-get-overlays (point-min) (point-max)))
;;              ;; TODO: don't delete trailing whitespace in .diff or .patch files
;;              (delete-trailing-whitespace))))

(defalias 'exit-emacs 'save-buffers-kill-terminal)

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)

;; Based on:
;; - https://github.com/lewang/flx#gc-optimization
;; - http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold wjb/gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

(setq message-log-max t
      suggest-key-bindings nil
      large-file-warning-threshold 25000000
      text-quoting-style 'straight
      search-default-mode #'character-fold-to-regexp
      auto-window-vscroll nil
      compile-command "npm test"
      compilation-always-kill t
      recenter-positions '(0.33 top bottom)
      ediff-window-setup-function 'ediff-setup-windows-plain
      revert-without-query '("TAGS"))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(if (fboundp 'global-prettify-symbols-mode)
    (progn  (global-prettify-symbols-mode 1)
            (setq prettify-symbols-unprettify-at-point 'right-edge)))

(defalias 'apply-kbd-macro-to-region-lines 'apply-macro-to-region-lines)

;; C-] is abort-recursive-edit

;; From: https://stackoverflow.com/a/39672208/599258
(defun cancel-minibuffer-first (sub-read &rest args)
    (let ((active (active-minibuffer-window)))
        (if active
                (progn
                    ;; we have to trampoline, since we're IN the minibuffer right now.
                    (apply 'run-at-time 0 nil sub-read args)
                    (abort-recursive-edit))
            (apply sub-read args))))

(advice-add 'read-from-minibuffer :around #'cancel-minibuffer-first)

;; from https://superuser.com/a/132454/93702
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)

(setq tab-always-indent 'complete)

(minibuffer-depth-indicate-mode 1)
(setq enable-recursive-minibuffers t)

;; The visible bell is usually fine, but still horrid in certain terminals.
;; We can make a nicer version. See http://pragmaticemacs.com/emacs/using-a-visible-bell-in-emacs/
;; and
;; (defun wjb/visible-bell ()
;;   "A friendlier visual bell effect (invert)."
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.05 nil #'invert-face 'mode-line))

(defun wjb/visible-bell ()
  "A friendlier visual bell effect (alarm color)."
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit keyboard-escape-quit))
    (let ((orig-bg (face-background 'mode-line))
          (wjb/alarm-color "#F2804F"))
      (set-face-background 'mode-line wjb/alarm-color)
      (run-with-idle-timer 0.1 nil
                           (lambda (bg) (set-face-background 'mode-line bg))
                           orig-bg))))

(define-minor-mode wjb/visible-bell-mode
  "Use `wjb/visible-bell' as the `ring-bell-function'."
  :global t
  (let ((this 'wjb/visible-bell-mode))
    (if wjb/visible-bell-mode
        (progn
          (put this 'visible-bell-backup visible-bell)
          (put this 'ring-bell-function-backup ring-bell-function)
          (setq visible-bell nil
                ring-bell-function #'wjb/visible-bell))
      ;; Restore the original values when disabling.
      (setq visible-bell (get this 'visible-bell-backup)
            ring-bell-function (get this 'ring-bell-function-backup)))))

(setq visible-bell t)
(wjb/visible-bell-mode 1)

(setq tags-add-tables t)

(defvar desktop-restore-eager 16)
(desktop-save-mode 1)
;; (setq desktop-restore-frames nil)

(setq register-preview-delay nil
      confirm-kill-processes nil)

;; emacs 27
(setq auto-save-no-message t
      tooltip-resize-echo-area t
      ;; I think I want this:
      switch-to-buffer-obey-display-actions t
      ;; this is actual buffer-local:
      display-fill-column-indicator t)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; for communicating with subprocesses
(setq read-process-output-max (* 1024 1024))

(setq term-suppress-hard-newline t)

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

(setq display-line-numbers-type nil)

(provide 'sane-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sane-defaults.el ends here
