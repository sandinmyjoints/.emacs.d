;;; comment-dwim-2.el --- An all-in-one comment command to rule them all  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025  Rémy Ferré

;; Author: Rémy Ferré <dev@remyferre.net>
;; Package-Version: 20241219.1643
;; Package-Revision: 6ab75d0a690f
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/remyferre/comment-dwim-2
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; # Description
;;
;; This package provides a replacement for `comment-dwim' called
;; `comment-dwim-2', which includes more features and allows you to
;; comment / uncomment / insert comment / kill comment and indent
;; comment depending on the context.  The command can be repeated
;; several times to switch between the different possible behaviors.
;;
;; # Demonstration
;;
;; You can find several gifs showing how the command works on Github:
;; https://github.com/remyferre/comment-dwim-2
;;
;; # How to use
;;
;; You need to add your own key binding first, for instance:
;;
;;    (keymap-global-set "M-;" #'comment-dwim-2)
;;
;; # Customization
;;
;; ## Commenting region
;;
;; When commenting a region, `comment-dwim-2' will by default comment
;; the entirety of the lines that the region spans (i.e. a line will
;; be fully commented even if it is partly selected).  In Lisp modes,
;; however, `comment-dwim-2' will strictly comment the region as
;; commenting whole lines could easily lead to unbalanced parentheses.
;; You can customize this behavior.
;;
;; If you always want to fully comment lines (Lisp modes included),
;; add this to your configuration file:
;;
;;    (setopt comment-dwim-2-region-function
;;     #'comment-dwim-2-comment-or-uncomment-lines)
;;
;; If you only want to comment the selected region (like
;; `comment-dwim' does), add this:
;;
;;    (setopt comment-dwim-2-region-function
;;     #'comment-dwim-2-comment-or-uncomment-region)
;;
;; # Org-mode
;;
;; In `org-mode' code blocks, `comment-dwim-2' will work as expected.
;; When editing regular `org-mode' content, it will fallback to
;; `org-toggle-comment' instead.
;;
;; ## Behavior when command is repeated
;;
;; `comment-dwim-2' will by default try to kill any end-of-line
;; comments when repeated.  If you wish to reindent these comments
;; instead, add this to your configuration file:
;;
;;    (setopt comment-dwim-2-inline-comment-behavior 'reindent-comment)
;;
;; If you use this setting, you will still be able to kill comments by
;; calling `comment-dwim-2' with a prefix argument.

;;; Code:

(require 'cl-lib)
(declare-function org-in-src-block-p "org")
(declare-function org-toggle-comment "org")
(eval-when-compile
  ;; For macro `org-babel-do-in-edit-buffer'
  ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Compiling-Macros.html
  (require 'ob-core))
;; Dynamically bound variable used by `org-babel-do-in-edit-buffer'
(defvar org-src-window-setup)
;; Functions used by `org-babel-do-in-edit-buffer'
(declare-function org-edit-src-exit "org-src")
(declare-function org-edit-src-code "org-src")
(declare-function org-element-at-point "org-element")
(declare-function org-element-property "org-element")
(declare-function org-babel-where-is-src-block-head "ob-core")

(defgroup comment-dwim-2 nil
  "An all-in-one comment command to rule them all."
  :group 'convenience
  :group 'tools
  :prefix "comment-dwim-2-"
  :link '(url-link :tag "Repository on Github"
                   "https://github.com/remyferre/comment-dwim-2"))

;; Better follow Elisp coding conventions
(define-obsolete-variable-alias
  'comment-dwim-2--inline-comment-behavior
  'comment-dwim-2-inline-comment-behavior
  "1.5.0")

(defcustom comment-dwim-2-inline-comment-behavior 'kill-comment
  "Behavior of `comment-dwim-2' when repeated and at an inline comment.
Possible values are:

* \\='kill-comment     : Kill the inline comment (default)
* \\='reindent-comment : Reindent the inline comment

When a behavior is chosen, the other one is still made available
by calling `comment-dwim-2' with a prefix argument."
  :type '(choice (const :tag "Kill the inline comment" kill-comment)
                 (const :tag "Reindent the inline comment" reindent-comment))
  :group 'comment-dwim-2)

(defun comment-dwim-2--unexpected-value-error (var)
  "Signal a user error when variable VAR has an unexpected value."
  (user-error "`%s' got an unexpected value" (symbol-name var)))

(defun comment-dwim-2--inline-comment-handler ()
  "Function called by `comment-dwim-2' when repeated and at an inline comment.
The behavior depends on the value of `comment-dwim-2-inline-comment-behavior'"
  (cl-case comment-dwim-2-inline-comment-behavior
    (kill-comment     (comment-dwim-2--comment-kill))
    (reindent-comment (comment-indent))
    (t (comment-dwim-2--unexpected-value-error
        'comment-dwim-2-inline-comment-behavior))))

(defun comment-dwim-2--prefix-handler ()
  "Function called by `comment-dwim-2' when called with a prefix argument.
The behavior is the one not chosen by the user in
`comment-dwim-2-inline-comment-behavior' so it can still be
available."
  (cl-case comment-dwim-2-inline-comment-behavior
    (kill-comment     (comment-indent))
    (reindent-comment (comment-dwim-2--comment-kill))
    (t (comment-dwim-2--unexpected-value-error
        'comment-dwim-2-inline-comment-behavior))))

;; Better follow Elisp coding conventions
(define-obsolete-function-alias
  'cd2/comment-or-uncomment-region
  'comment-dwim-2-comment-or-uncomment-region
  "1.5.0")
(define-obsolete-function-alias
  'cd2/comment-or-uncomment-lines
  'comment-dwim-2-comment-or-uncomment-lines
  "1.5.0")
(define-obsolete-function-alias
  'cd2/comment-or-uncomment-lines-or-region-dwim
  'comment-dwim-2-comment-or-uncomment-lines-or-region-dwim
  "1.5.0")
(define-obsolete-variable-alias
  'cd2/region-command
  'comment-dwim-2-region-function
  "1.5.0")

(defun comment-dwim-2-comment-or-uncomment-region ()
  "Call `comment-or-uncomment-region' on current region."
  (comment-or-uncomment-region (region-beginning) (region-end)))

(defun comment-dwim-2-comment-or-uncomment-lines ()
  "Toggle commenting on all the lines that the region spans."
  (if (eq (line-number-at-pos (point))
          (line-number-at-pos (mark)))
      (comment-dwim-2-comment-or-uncomment-region)
    (comment-or-uncomment-region
     (save-excursion (goto-char (region-beginning)) (line-beginning-position))
     (if (= (region-end)
            (save-excursion (goto-char (region-end)) (line-beginning-position)))
         (- (region-end) 1)
       (save-excursion (goto-char (region-end)) (line-end-position))))))

(defun comment-dwim-2-comment-or-uncomment-lines-or-region-dwim ()
  "Toggle commenting on lines or region depending on the mode.

In most modes, this function will toggle commenting on all the lines
that the region spans.
In Lisp-derived modes, however, this function applies strictly to the
region, as commenting whole lines would often results with
unbalanced parentheses."
  (if (derived-mode-p 'lisp-data-mode 'clojure-mode 'racket-mode
                      'scheme-mode 'hy-mode)
      (comment-dwim-2-comment-or-uncomment-region)
    (comment-dwim-2-comment-or-uncomment-lines)))

(defcustom comment-dwim-2-region-function
  'comment-dwim-2-comment-or-uncomment-lines-or-region-dwim
  "Function used for commenting/uncommenting current region.
Possible values are:

* `comment-dwim-2-comment-or-uncomment-lines-or-region-dwim' (default)
* `comment-dwim-2-comment-or-uncomment-lines'
* `comment-dwim-2-comment-or-uncomment-region'"
  :type '(choice
          (function :tag "Comment region in lisp modes, lines otherwise."
                    comment-dwim-2-comment-or-uncomment-lines-or-region-dwim)
          (function :tag "Comment lines the region spans."
                    comment-dwim-2-comment-or-uncomment-lines)
          (function :tag "Comment region."
                    comment-dwim-2-comment-or-uncomment-region))
  :group 'comment-dwim-2)

(defun comment-dwim-2--empty-line-p ()
  "Return non-nil if current line contains only whitespace characters."
  (string-match "^[[:blank:]]*$"
                (buffer-substring (line-beginning-position)
                                  (line-end-position))))

(defun comment-dwim-2--fully-commented-line-p ()
  "Return non-nil if current line is commented from its beginning.
Whitespace characters at the beginning of the line are ignored."
  (and (not (comment-dwim-2--empty-line-p))
       (comment-only-p (save-excursion
                         (move-beginning-of-line 1)
                         (skip-chars-forward " \t")
                         (point))
                       (line-end-position))))

(defun comment-dwim-2--face-is-at-pos-p (pos faces)
  "Return non-nil if one of the given FACES is at POS."
  (let* ((face-property (get-text-property pos 'face))
         (faces-at-point (ensure-list face-property)))
    (cl-some (lambda (face) (member face faces-at-point)) faces)))

(defun comment-dwim-2--within-comment-p (pos)
  "Return non-nil if content at given position (POS) is within a comment."
  (comment-dwim-2--face-is-at-pos-p pos '(font-lock-comment-face
                                          font-lock-comment-delimiter-face)))

(defun comment-dwim-2--line-contains-comment-p ()
  "Return non-nil if current line contains a comment."
  (save-excursion
    (font-lock-fontify-region (line-beginning-position) (line-end-position)))
  (let ((eol (line-end-position)))
    (save-excursion
      (move-beginning-of-line 1)
      (while (and (/= (point) eol)
                  (not (comment-dwim-2--within-comment-p (point))))
        (forward-char))
      (comment-dwim-2--within-comment-p (point)))))

(defun comment-dwim-2--line-ends-with-multiline-string-p ()
  "Return t if current line ends inside a multiline string.
In such a case, adding an end-of-line comment is meaningless."
  (let ((bol  (line-beginning-position))
        (eol  (line-end-position))
        (bol2 (line-beginning-position 2)))
    (and
     ;; End of line have string face..
     (save-excursion
       (font-lock-fontify-region bol bol2)
       (comment-dwim-2--face-is-at-pos-p eol '(font-lock-string-face
                                               font-lock-doc-face)))
     ;; ..and next line contains a string which begins at the same position
     (= (elt (save-excursion (syntax-ppss eol )) 8)
        (elt (save-excursion (syntax-ppss bol2)) 8)))))

(defun comment-dwim-2--comment-kill ()
  "A clone of `comment-kill' which does not reindent the code."
  (comment-normalize-vars)
  (save-excursion
    (beginning-of-line)
    (let ((cs (comment-search-forward (line-end-position) t)))
      (when cs
        (goto-char cs)
        (skip-syntax-backward " ")
        (setq cs (point))
        (comment-forward)
        (kill-region cs (if (bolp) (1- (point)) (point)))))))

(defun comment-dwim-2--uncomment-line ()
  "Uncomment current line."
  (uncomment-region (line-beginning-position) (line-end-position)))

(defun comment-dwim-2--comment-line ()
  "Comment current line."
  ;; `comment-region' does not support empty lines, so we use
  ;; `comment-dwim' in such cases to comment the line
  (if (comment-dwim-2--empty-line-p)
      (comment-dwim nil)
    (comment-region (line-beginning-position) (line-end-position))))

(defun comment-dwim-2--prog (&optional arg)
  "`comment-dwim-2' handler for `prog-mode'."
  (if (use-region-p)
      (funcall comment-dwim-2-region-function)
    (if arg
        (comment-dwim-2--prefix-handler)
      (if (comment-dwim-2--fully-commented-line-p)
          (progn
            (comment-dwim-2--uncomment-line)
            (when (and (or (eq last-command 'comment-dwim-2)
                           (eq last-command 'org-comment-dwim-2))
                       (not (comment-dwim-2--empty-line-p))
                       (not (comment-dwim-2--line-ends-with-multiline-string-p))
                       (not (comment-dwim-2--fully-commented-line-p)))
              (if (comment-dwim-2--line-contains-comment-p)
                  (comment-dwim-2--inline-comment-handler)
                (comment-indent)))) ; Insert inline comment
        (if (and (comment-dwim-2--line-contains-comment-p)
                 (or (eq last-command 'comment-dwim-2)
                     (eq last-command 'org-comment-dwim-2)))
            (comment-dwim-2--inline-comment-handler)
          (comment-dwim-2--comment-line))))))

(defun comment-dwim-2--org (&optional arg)
  "`comment-dwim-2' handler for `org-mode'.

In code blocks, work as usual.  In regular `org-mode' content,
fallback to `org-toggle-comment' instead"
  (if (org-in-src-block-p t)
      (org-babel-do-in-edit-buffer
       (comment-dwim-2--prog arg))
    (org-toggle-comment)))

;;;###autoload
(defun comment-dwim-2 (&optional arg)
  "Call a comment command according to the context.

If the region is active, toggle commenting according to the value
of `comment-dwim-2-region-function'.
Else, the function applies to the current line and calls a
different function at each successive call.  The algorithm is:
* First  call : Toggle line commenting
* Second call : - Kill end-of-line comment if present (1)
                - Insert end-of-line comment otherwise
Given an prefix ARG, it reindents the inline comment instead (2).

You can also switch behaviors of (1) and (2) by setting
`comment-dwim-2-inline-comment-behavior' to \\='reindent-comment."
  (interactive "*P")
  (if (derived-mode-p 'org-mode)
      (comment-dwim-2--org arg)
    (comment-dwim-2--prog arg)))

;; `comment-dwim-2' now supports `org-mode' by default
(define-obsolete-function-alias 'org-comment-dwim-2 'comment-dwim-2 "1.5.0")

(provide 'comment-dwim-2)
;;; comment-dwim-2.el ends here
