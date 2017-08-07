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

;; Allow pasting selection outside of Emacs/kill to system clipboard.
(setq x-select-enable-clipboard t)

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

;; Fix junk characters in shell-mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

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
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(setq-default fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Set up 2-space tabs.
;; See: http://stackoverflow.com/a/1819405/599258
(setq-default tab-width 2)
;(setq indent-line-function 'insert-tab)
(setq indent-line-function 'indent-relative-maybe)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
;(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

;;; Visible bell.
(setq-default visible-bell t)

;;; Fewer pop-up windows.
(setq pop-up-windows nil)

;;; Default for new buffers.
(when (require 'markdown-mode nil t)
  (setq-default major-mode 'fundamental-mode))

;;; Avoid backslash madness.
(setq reb-re-syntax 'string)

(put 'upcase-region 'disabled nil)

;;; Emacs 24 and up.
(when (>= emacs-major-version 24)
  ;; Pair parens and other delimiters.
  ;;(electric-pair-mode t) ;; try using smart-parens
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

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

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

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(display-time-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(after-load 'autopair (diminish 'autopair-mode))
(after-load 'auto-complete (diminish 'auto-complete-mode))
(after-load 'yasnippet (diminish 'yas-minor-mode))
(after-load 'eldoc (diminish 'eldoc-mode))
;;(after-load 'smartparens (diminish 'smartparens-mode))
(after-load 'rainbow-mode (diminish 'rainbow-mode))
(after-load 'smart-tab-mode (diminish 'smart-tab-mode))

(defalias 'exit-emacs 'save-buffers-kill-terminal)

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)

(defvar my/gc-cons-threshold 20000000)

;; Based on https://github.com/lewang/flx#gc-optimization
(setq gc-cons-threshold my/gc-cons-threshold)

;; Based on http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold my/gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

(setq message-log-max t)

(setq suggest-key-bindings nil)

(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

(if (fboundp 'global-prettify-symbols-mode)
    (progn  (global-prettify-symbols-mode 1)
            (setq prettify-symbols-unprettify-at-point 'right-edge)))

(setq text-quoting-style 'straight
      search-default-mode #'character-fold-to-regexp)

;; See http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(setq split-width-threshold 300)

(provide 'sane-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sane-defaults.el ends here
