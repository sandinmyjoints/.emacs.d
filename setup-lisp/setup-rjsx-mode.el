;;; setup-rjsx-mode.el ---
;;
;; Filename: setup-rjsx-mode.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Fri Sep 26 12:22:29 2025 (-0400)
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

(use-package rjsx-mode)

;; Which mode(s) to use for JSX?
;; - could try js2-jsx-mode by itself
;; - web-mode + js2-jsx-mode is pretty good but has some quirks, and js2r doesn't work b/c it doesn't support the js2 parse tree.
;; - rsjx-mode works with js2r but it has had a tendency to hang when attributes are malformed.
;; - in emacs 27, js-mode with js2-minor-mode is recommended for JSX, but js2 doesn't work.
;;
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)) ;; rjsx can parse spread operator

;; from https://github.com/felipeochoa/rjsx-mode/issues/112#issuecomment-530497532
(defun +javascript-rjsx-electric-gt-a (n)
  (when (and (looking-back "<>")
             (looking-at-p "/>"))
    (save-excursion (insert "<"))))
(advice-add #'rjsx-electric-gt :after #'+javascript-rjsx-electric-gt-a)

(provide 'setup-rjsx-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-rjsx-mode.el ends here
