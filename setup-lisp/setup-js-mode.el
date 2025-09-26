;;; setup-js-mode.el ---
;;
;; Filename: setup-js-mode.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Fri Sep 26 12:35:30 2025 (-0400)
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

;; from https://github.com/redguardtoo/emacs.d/blob/def7e0496482e1830ff6d1182ff20b2a6fa68160/lisp/init-javascript.el#L66
(eval-after-load 'js-mode
  '(progn
     ;; experimental: make underscore be a symbol, part of a name
     (modify-syntax-entry ?- "_" js-mode-syntax-table)

     ;; '$' is part of variable name like '$item'
     (modify-syntax-entry ?$ "w" js-mode-syntax-table)))

(defun wjb/company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun wjb/js-mode-hook nil
  (make-local-variable 'company-transformers)
  (push 'wjb/company-transformer company-transformers)
  (setq-local prettify-symbols-alist nil)
  (setq-local fill-column 80))

(add-hook 'js-base-mode-hook 'wjb/js-mode-hook)

(provide 'setup-js-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-js-mode.el ends here
