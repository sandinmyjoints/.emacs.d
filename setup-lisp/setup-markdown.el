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

(provide 'setup-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-markdown.el ends here
