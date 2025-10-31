;;; company-dabbrev-code-for-text.el --- code symbol company backend for text/org  -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Your Name Here
;; This file is NOT part of GNU Emacs (local extension).

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `company-dabbrev-code-for-text' is a `company-mode' backend intended for
;; use while editing text/org buffers (i.e. in `text-mode', `org-mode', and
;; modes derived from them).  It offers code symbol completions collected
;; from all buffers whose major mode is in `company-dabbrev-code-modes'.
;;
;; It reuses the configuration and filtering logic of `company-dabbrev-code',
;; including respect for:
;;   - `company-dabbrev-code-ignore-case'
;;   - `company-dabbrev-code-completion-styles'
;;   - `company-dabbrev-code-time-limit'
;;   - `company-dabbrev-code-everywhere' (controls skipping of comments/strings
;;      in the searched code buffers)
;;
;; Unlike `company-dabbrev-code', this backend never limits itself to the
;; current buffer (since the current buffer is not a code buffer), and always
;; searches all code-mode buffers (per `company-dabbrev-code-modes').

;;; Code:

(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)
(require 'cl-lib)

(defgroup company-dabbrev-code-for-text nil
  "Completion backend providing code symbols while in text/org buffers."
  :group 'company)

;;;###autoload
(defun company-dabbrev-code-for-text (command &optional arg &rest _ignored)
  "Company backend offering code symbols in text/org buffers.

COMMAND, ARG, and _IGNORED follow the `company-mode' backend protocol."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dabbrev-code-for-text))
    (prefix (when (or (derived-mode-p 'text-mode)
                      (eq major-mode 'org-mode))
              (or (company-grab-symbol) 'stop)))
    (candidates
     (let* ((case-fold-search company-dabbrev-code-ignore-case)
            (regexp (company-dabbrev-code--make-regexp arg)))
       (company-dabbrev-code--filter
        arg
        (company-cache-fetch
         'dabbrev-code-for-text-candidates
         (lambda ()
           (company-dabbrev--search
            regexp
            company-dabbrev-code-time-limit
            company-dabbrev-code-modes
            (not company-dabbrev-code-everywhere)))
         :expire t
         :check-tag regexp))))
    (kind 'text)
    (annotation "Dabbrev-code")
    (no-cache t)
    (ignore-case company-dabbrev-code-ignore-case)
    (match (when company-dabbrev-code-completion-styles
             (company--match-from-capf-face arg)))
    (duplicates t)))

(provide 'company-dabbrev-code-for-text)
;;; company-dabbrev-code-for-text.el ends here
