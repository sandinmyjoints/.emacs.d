;;; setup-markdown.el --- Setup markdown the way I like.
;;
;; Filename: setup-markdown.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Mon Aug 24 17:48:09 2015 (-0700)
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

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Based on http://stackoverflow.com/a/1680994

;; https://github.com/michaelamie/markdown-mode-css/tree/master/css
;; also see scm/vendor/markdown-css-theme
;; also see https://markdowncss.github.io/
(setq markdown-css-paths '("/Users/william/markdown-css/github-rhio.css")
      markdown-list-indent-width 2
      markdown-asymmetric-header t
      markdown-fontify-code-blocks-natively t)

;; (setq markdown-css-paths '("github-rhio.css"))

(add-to-list 'markdown-code-lang-modes  '("json" . json-mode))
(add-to-list 'markdown-code-lang-modes  '("js" . js2-mode))
(add-to-list 'markdown-code-lang-modes  '("coffee" . coffee-mode))

(after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-b") 'browse-at-remote))

;; Useful:
;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)

;; from https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-markdown.el

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn
    ;; Seamless editing of Markdown tables (allowed in GFM) using `orgtbl-mode'
    ;; http://stackoverflow.com/a/20912535/1219634
    ;; https://gist.github.com/yryozo/5807243
    (defun orgtbl-to-gfm (table params)
      "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
      (let* ((alignment (mapconcat (lambda (x)
                                     (if x
                                         "|--:"
                                       "|---"))
                                   org-table-last-alignment ""))
             (params2 (list :splice t
                            :hline (concat alignment "|")
                            :lstart "| " :lend " |" :sep " | ")))
        (orgtbl-to-generic table (org-combine-plists params2 params))))
    (add-hook 'markdown-mode-hook #'orgtbl-mode)

    (bind-keys
     :map markdown-mode-map
      ;; Mimicking the org-export style bindings
      ("C-c C-e o" . markdown-preview)
      ("C-c C-e t". orgtbl-send-table))))

;; Example orgtbl template:
;;
;; <!--- BEGIN RECEIVE ORGTBL foo-tbl -->
;; | a | b |
;; |---|---|
;; | c | d |
;; <!--- END RECEIVE ORGTBL foo-tbl -->
;; <!---
;;  - Title row is needed.
;;  - Horizontal rule below title row is needed.
;;  - The table identifier 'foo-tbl' after SEND has to match with that in the
;;    BEGIN RECEIVE and END RECEIVE lines above.
;; #+orgtbl: SEND foo-tbl orgtbl-to-gfm
;; | a | b |
;; |---+---|
;; | c | d |
;; -->
;;
;; 1. Paste the above template in a `markdown-mode' buffer (without the elisp
;;    comment delimiters ";;").
;; 2. Rename 'foo-tbl' to whatever is more appropriate (optional).
;; 3. With point *inside* the 'SEND' table, call `orgtbl-send-table'.
;;
;;    Above is tested to work with `hugo' (which uses the BlackFriday markdown
;; parser).

(provide 'setup-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-markdown.el ends here
