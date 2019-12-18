;;; setup-smartparens.el --- Configure smartparens.
;;
;; Filename: setup-smartparens.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Mon Dec 29 00:06:00 2014 (-0500)
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


(require 'smartparens-config)

(smartparens-global-mode t)

;; Not sure if I want this or not.
(show-smartparens-global-mode t)

(dolist (mode '(dirtree-mode))
  (add-to-list 'sp-ignore-modes-list mode))

;; UPDATE: This variable has been removed. See
;; http://smartparens.readthedocs.io/en/latest/automatic-escaping.html for the
;; latest on how this stuff works.
;;
;; This does not appear to work -- it still escapes quotes in coffee-mode...
;; (dolist (mode '(coffee-mode shell-mode)) (add-to-list
;; 'sp-autoescape-string-quote-if-empty mode))

;; UPDATE: This seems to have been removed too.
;; ...so turn off all autoescaping.
;; (setq sp-autoescape-string-quote nil)

(sp-local-pair '(markdown-mode gfm-mode) "*" "*"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
;; to customize/view this list.
(mapc (lambda (mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
      sp--lisp-modes)

;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)

;; The difference between parens and quotes is that quotes smart and end with
;; the same char.

;; A string-like sexp is an expression where opening and closing delimeter is
;; the same sequence of characters. For example: *...*, $...$.

;; For coffee-mode (and possibly other modes):
;; * make sp-autoskip-opening-pair nil (the default) or t
;; * make sp-autoskip-closing-pair always (instead of default always-end) --
;;   seems likely it is unable to tell when it's at the end in quoted strings.
;; * and add coffee-mode to sp-navigate-consider-stringlike-sexp
;; to get the string-closing behavior I want.
;; This variable is buffer-local, so must be set by customize.
(setq sp-autoskip-closing-pair 'always)

;; If this is true (the default) then after typing delete or moving backwards in
;; a string, typing the closing delimeter will no longer jump to the end of the
;; string. I don't want that, so I turn it off.
(setq sp-cancel-autoskip-on-backward-movement nil)

;; Based off of https://github.com/Fuco1/smartparens/wiki/Example-configuration
;; but using H instead of C-M.
(define-key smartparens-mode-map (kbd "H-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "H-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "H-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "H-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "H-M-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "H-M-d") 'sp-end-of-sexp)

;; For some reason, any combos with H and e seem not to work. Why?
(define-key smartparens-mode-map (kbd "H-e") 'sp-up-sexp)
;; (define-key smartparens-mode-map (kbd "H-x") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd "H-)") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "H-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "H-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "H-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "H-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "H-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "H-w") 'sp-copy-sexp)

(provide 'setup-smartparens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-smartparens.el ends here
