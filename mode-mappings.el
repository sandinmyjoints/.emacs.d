;; Jade and Stylus (sws = significant whitespace)
(autoload 'sws-mode "sws-mode")
(autoload 'jade-mode "jade-mode")
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; Ruby
(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

;; Clojure
(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
;(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; Add buffer-local indicator for whether prog-mode-hook has run.
;; See:
;; http://yoo2080.wordpress.com/2012/03/15/js2-mode-setup-recommendation/
(defun my-set-pmh-ran ()
  (set (make-local-variable 'my-pmh-ran) t))

(add-hook 'prog-mode-hook 'my-set-pmh-ran)

;; Ensure js2-mode runs prog-mode-hook.
(add-hook 'js2-mode-hook 'my-run-pmh-if-not-ran)
(defun my-run-pmh-if-not-ran ()
  (unless (bound-and-true-p my-pmh-ran)
    (run-hooks 'prog-mode-hook)))

;; Handlebars mode.
(autoload 'handlebars-mode "handlebars-mode"
  "Major mode for editing Handlebars")
(add-to-list 'auto-mode-alist '("\\.handlebars$" . js2-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; log mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;; php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; less-css-mode
(autoload 'less-css-mode "less-css-mode" "Major mode for LESS CSS." )
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; Coffee-mode.
;;;
;; Want to change the regex when loading files from
;; fixtures directory. Some kind of hook or advice.
;; Or, could somehow fix the regex.
;;;
(autoload 'coffee-mode "coffee-mode" "Major mode for editing CoffeeScript.")
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  (set 'coffee-tab-width 2)
  (set-fill-column 80))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; TODO: write-file-functions is global, so this is a pretty dumb hook. Fix it.
(add-hook 'coffee-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; TODO: coffee-beginning-of-defun. Search backward to -> not in
;; string or comment.

;; Catch common typo.
(add-hook 'coffee-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (perform-replace "commong" "common" nil nil nil nil nil (point-min) (point-max)))))))


(provide 'mode-mappings)
