;;; mode-mappings.el ---
;;
;; Filename: mode-mappings.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Mon Nov 13 23:12:59 2017 (-0500)
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

(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

;; Jade and Stylus (sws = significant whitespace)
(autoload 'sws-mode "sws-mode")
(autoload 'jade-mode "jade-mode")
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))

(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))

;; nginx
(add-to-list 'auto-mode-alist '("\\neodarwin-site.erb\\'" . nginx-mode))

(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

;; Clojure
(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg\\'" . image-mode))

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
;(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; Handlebars mode.
;; (autoload 'handlebars-mode "handlebars-mode"
;;   "Major mode for editing Handlebars")
;; (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . js2-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

;; log mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;; php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.knot\\'" . knot-mode))

(add-to-list 'auto-mode-alist '("\\.min\\.js\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("-min\\.js\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("-min-async\\.js\\'" . fundamental-mode))

(provide 'mode-mappings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-mappings.el ends here
