;;; sane-defaults.el --- Set up some useful defaults.
;;
;; Filename: sane-defaults.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Dec 31 17:03:23 2014 (-0800)
;; Version:
;; Package-Requires: ((emacs "27.1"))
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

(defvar wjb/home (expand-file-name "~"))

(setq user-full-name "William Bert"
      user-mail-address "william.bert@gmail.com")


;; Buffer display, frame management, and window splitting.

;;; Fewer new frames and pop-up windows.
(setq pop-up-frames nil)
(setq pop-up-windows nil)

;;; Discourage split-window-sensibly from splitting windows.
(setq split-height-threshold nil) ;; will still fall back to splitting
                                  ;; vertically if other windows in frame are
                                  ;; dedicated
(setq split-width-threshold nil)

;; prevents all splitting, but then restclient (and others?) will pop a new frame instead...
;; (setq split-window-preferred-function nil)
(setq split-window-preferred-function #'split-window-sensibly) ;; ...so go with the default.

;; Display a different buffer when splitting window vertically.
(defadvice split-window-vertically
    (after my-window-splitting-advice first () activate)
    (set-window-buffer (next-window) (other-buffer)))

;; From https://www.reddit.com/r/emacs/comments/80pd2q/anyone_could_help_me_with_window_management/dux9cme/
;; also potentially useful: https://emacs.stackexchange.com/a/338/2163
(setq display-buffer-alist
      ;; Let popup buffers pop up.
      '(("\*.*popup\*" . (display-buffer-pop-up-window))
        ("\*helm-imenu\*" . (display-buffer-pop-up-window))
        ;; Catchall: always allow same window, which is the one reusable window.
        (".*" .
         ;; (display-buffer-use-some-window .
         ;; (display-buffer-reuse-window .
         (display-buffer-same-window .
                                         '((inhibit-same-window . nil)
                                           (inhibit-switch-frame . t))))
        ))

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)



(setq-default bidi-display-reordering 'left-to-right)

(setq diff-font-lock-prettify t
      isearch-lazy-count t
      inhibit-startup-echo-area-message t)

;; Turn on/off display stuff.
;;
(setq-default visible-bell t
              font-lock-maximum-decoration t
              truncate-partial-width-windows nil)

;; Don't break lines for me, please
;; what is relationship of this to visual-line-mode? and truncate-partial-width-windows?
;; use toggle-truncate-lines (C-x t l) to enable/disable
(setq-default truncate-lines t)

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

;; Lines should be 80 characters wide, not 72.
(setq-default fill-column 80)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Set up 2-space tabs.
;; See: http://stackoverflow.com/a/1819405/599258
(setq-default tab-width 2)

(setq-default indent-line-function 'insert-tab)
;; TODO: check if this is what I really want in all modes:
;; (setq-default indent-line-function 'indent-relative-maybe)

;; Show empty lines after buffer end?
(setq-default indicate-empty-lines nil
              indicate-buffer-boundaries nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Easily navigate sillycased words
(use-package subword
  :diminish
  :config
  (global-subword-mode 1))

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
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

;;; Default for new buffers.
(setq-default major-mode 'fundamental-mode)

;;; Avoid backslash madness.
(use-package re-builder
  :defer 5
  :config
  (setq reb-re-syntax 'string))

;;; Emacs 24 and up.
(when (>= emacs-major-version 24)
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

;; Narrow to region is useful.
(put 'narrow-to-region 'disabled nil)

;; Shift-arrow moves around windows, by default, which is fine.
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

(require 'wjb-byte-compile)
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


;; Minibuffer and recursive editing.
;;
;; C-] is abort-recursive-edit.

;; From https://www.reddit.com/r/emacs/comments/qqnjse/comment/hk1zm4z/?utm_source=share&utm_medium=web2x&context=3
(use-package emacs
  :custom
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer"))

(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode 1))

;; From: https://stackoverflow.com/a/39672208/599258
(defun cancel-minibuffer-first (sub-read &rest args)
    (let ((active (active-minibuffer-window)))
        (if active
                (progn
                    ;; we have to trampoline, since we're IN the minibuffer right now.
                    (apply 'run-at-time 0 nil sub-read args)
                    (abort-recursive-edit))
            (apply sub-read args))))

;; (advice-add 'read-from-minibuffer :around #'cancel-minibuffer-first)
(advice-remove 'read-from-minibuffer #'cancel-minibuffer-first)

;; from https://superuser.com/a/132454/93702
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(setq tab-always-indent 'complete)

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
(setq desktop-lazy-verbose t
      desktop-lazy-idle-delay 8)
(desktop-save-mode -1)
;; (setq desktop-restore-frames nil)

(setq register-preview-delay nil
      confirm-kill-processes nil)
(setq-default display-line-numbers-widen t)

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

(setq term-suppress-hard-newline t
      display-line-numbers-type nil)

(require 'wjb-byte-compile)
(advice-add 'keyboard-quit :around #'my-keyboard-quit-advice)
;; (define-key minibuffer-local-map "\C-g" 'minibuffer-keyboard-quit)

(setq warning-minimum-level :emergency)

;; allow remembering risky variables. from https://emacs.stackexchange.com/a/44604/2163
(defun risky-local-variable-p (sym &optional _ignored) nil)

;; conflicts with easy-kill
;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (message "Single line killed")
;;      (list (line-beginning-position)
;; 	         (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defface kmacro-modeline '() "Face when kmacro is active")
(set-face-attribute 'kmacro-modeline nil
                    :background "Firebrick"
                    :box `(:line-width -1 :color "salmon" :style released-button))

(defun ad-kmacro-change-modebar ()
  "Remap the mode-line face with our custom face"
  (add-to-list 'face-remapping-alist '(mode-line . kmacro-modeline)))

(defun ad-kmacro-restore-modebar ()
  "Restore the mode-line face"
  (setf face-remapping-alist
        (assoc-delete-all 'mode-line face-remapping-alist)))

(defadvice kmacro-start-macro (before kmacro-hl-modeline activate)
  "Alters `kmacro-start-macro' so it highlights the modeline when
  recording begins."
  (ad-kmacro-change-modebar))

(defadvice kmacro-keyboard-quit (before kmacro-rem-hl-modeline activate)
  "Alters `kmacro-keyboard-quit' so it highlights the modeline when
  recording begins."
  (ad-kmacro-restore-modebar))

(defadvice kmacro-end-macro (before kmacro-rem-hl-modeline activate)
  "Alters `kmacro-end-macro' so it highlights the modeline when
  recording begins."
  (ad-kmacro-restore-modebar))

(setq max-specpdl-size 32767)
(setq max-lisp-eval-depth 16000)
(setq ffap-machine-p-known 'accept)

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

;; Emacs 28
(setq help-enable-symbol-autoload t
      describe-bindings-outline t
      save-interprogram-paste-before-kill t
      isearch-repeat-on-direction-change t
      next-error-message-highlight t
      xref-show-definitions-function #'xref-show-definitions-completing-read
      eldoc-echo-area-display-truncation-message nil)

(setq use-dialog-box nil)

(provide 'sane-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sane-defaults.el ends here
