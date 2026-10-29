;;; kill-dollar-mode.el --- Remove leading $ from shell-script-like text  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 William Bert

;; Author: William Bert
;; Created: 13 Dec 2024

;; Keywords: convenience tools
;; URL: https://github.com/sandinmyjoints/kill-dollar-mode
;; Package-Version: 20241217.1947
;; Package-Revision: e51c076f9360
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; When killing from documentation code blocks, removes a leading
;; `$` and surrounding whitespace from text so that when yanked into a shell,
;; it can be executed.

;;; Code:

(defmacro kill-dollar-advice-inhibit (function &rest body)
  "Execute BODY with all advice temporarily disabled for FUNCTION."
  (declare (indent 1) (debug t))
  (let ((orig-fn (make-symbol "orig-fn")))
    `(let ((,orig-fn (symbol-function ',function)))
       (unwind-protect
           (progn
             (fset ',function (advice--cd*r (advice--symbol-function ',function)))
             ,@body)
         (fset ',function ,orig-fn)))))

(defvar kill-dollar-after-kill-new-hook nil
  "Hook run after text is added to the kill ring.")

(defadvice kill-new (after run-kill-dollar-after-kill-new-hook activate)
  "Run `kill-dollar-after-kill-new-hook` after text is added to the kill ring."
  (run-hooks 'kill-dollar-after-kill-new-hook))

(define-minor-mode kill-dollar-mode
  "Remove leading $ from lines when killing text in Org or Markdown code blocks."
  :lighter " Kill-$"
  :global nil

  (unless (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
    (user-error "Kill-Dollar mode works only in Org-mode and Markdown-mode."))

  (if kill-dollar-mode
      (add-hook 'kill-dollar-after-kill-new-hook #'kill-dollar-remove-dollar-on-kill nil t)
    (remove-hook 'kill-dollar-after-kill-new-hook #'kill-dollar-remove-dollar-on-kill t)))

(declare-function markdown-code-block-at-point-p "markdown-mode" (&optional pos))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))

(defun kill-dollar-remove-dollar-on-kill ()
  "Remove leading $ from each line of killed text.
Works inside org or markdown code blocks."
  (when (save-excursion
          (or
           (and (derived-mode-p 'org-mode)
                (eq (org-element-type (org-element-context)) 'src-block))
           (and (derived-mode-p 'markdown-mode)
                (markdown-code-block-at-point-p))))
      (let ((processed-text
             (with-temp-buffer
               (insert (current-kill 0))
               (goto-char (point-min))
               (while (not (eobp))
                 (when (looking-at "^\\s-*\\$\\s-*")
                   (replace-match "" nil nil))
                 (forward-line 1))
               (buffer-string))))
        (kill-dollar-advice-inhibit kill-new
          (kill-new processed-text)))))

(provide 'kill-dollar-mode)
;;; kill-dollar-mode.el ends here
