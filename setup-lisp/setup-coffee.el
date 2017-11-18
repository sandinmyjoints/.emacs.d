;;; setup-coffee.el --- Configure coffee-mode.
;;
;; Filename: setup-coffee.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Dec 31 18:22:11 2014 (-0800)
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

;; Coffee-mode.
(autoload 'coffee-mode "coffee-mode" "Major mode for editing CoffeeScript.")
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(eval-after-load 'coffee-mode
  '(define-key coffee-mode-map [(control j)] 'coffee-newline-and-indent))

(defun coffee-tab-properly ()
  "Expand yasnippet and if it worked, don't tab after."
  (interactive)

  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command))))

(after-load 'coffee-mode
  (define-key coffee-mode-map (kbd "TAB") 'coffee-tab-properly)
  (define-key coffee-mode-map (kbd "C-c C-b") 'browse-at-remote)
  (define-key coffee-mode-map (kbd "C-c r l") 'remove-console-log-coffee)
  (define-key coffee-mode-map (kbd "H-c r l") 'remove-console-log-coffee)
  (define-key coffee-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-coffee))

(defvar coffee-indent-like-python-mode nil)

(provide 'setup-coffee)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-coffee.el ends here
