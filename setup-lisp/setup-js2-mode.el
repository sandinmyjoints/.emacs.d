;;; setup-js2-mode.el --- Set up js2-mode.
;;
;; Filename: setup-js2-mode.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Jun 29 16:39:41 2016 (-0700)
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

(use-package js2-mode)

(use-package js2-imenu-extras
  :config
  (progn
    (setq js2-imenu-enabled-frameworks '(jquery react))
    (js2-imenu-extras-setup)))

;; To jump back:
;; xref-pop-marker-stack

(use-package js2-refactor
  :diminish js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "H-c")

  ;; hack to my liking
  (defun wjb/js2r-log-this (arg)
    "Log of the node at point, adding a 'console.log()' statement.
Unless a prefix argument ARG, use JSON pretty-printing for logging."
    (interactive "P")
    (js2r--guard)
    (js2r--wait-for-parse
     (let* ((log-info (js2r--figure-out-what-to-log-where))
	          (stmt (car log-info))
	          (pos (cdr log-info)))
       (save-excursion
         (goto-char pos)
         (when (looking-at "[;{]")
	         (forward-char 1))
         (newline-and-indent)
         (unless arg
	         (progn (insert "console.log('DEBUG ' + '" stmt " = ');")
		              (newline-and-indent)
		              (insert "console.dir(" stmt ");"))
	         ;; (insert "console.log('DEBUG ' + '" stmt " = ', " stmt ");")
           (insert "")
           )))))
  (defalias 'js2r-log-this #'wjb/js2r-log-this)

  (defun wjb/js2r-debug-this ()
    "Debug the node at point, adding a 'debug()' statement."
    (interactive)
    (js2r--guard)
    (js2r--wait-for-parse
     (let* ((log-info (js2r--figure-out-what-to-log-where))
	          (stmt (car log-info))
	          (pos (cdr log-info)))
       (save-excursion
         (goto-char pos)
         (when (looking-at "[;{]")
	         (forward-char 1))
         (newline-and-indent)
         (insert "debug(" (js2r--wrap-text stmt " = %s") ", " stmt ");")))))
  (defalias 'js2r-debug-this #'wjb/js2r-debug-this)
  )

(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "H-0 n") 'js2-narrow-to-defun)
  (define-key js2-mode-map (kbd "H-0 h") 'js2-mode-toggle-hide-functions)
  ;; TODO js2-mode-show-all
  (define-key js2-mode-map [remap move-beginning-of-line] 'js2-beginning-of-line)
  (define-key js2-mode-map [remap move-end-of-line] 'js2-end-of-line)
  (define-key js2-mode-map [remap forward-sexp] 'js2-mode-forward-sexp)
  (define-key js2-mode-map (kbd "C-c C-o") 'js2-mode-toggle-element)

  ;; (define-key js2-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
  ;; replace with dumb-jump, or js2-mode-goto-definition, or xref-find-definitions
  (define-key js2-mode-map (kbd "M-.") 'smart-jump-go)
  (define-key js2-mode-map (kbd "M-,") 'smart-jump-back)
  ;; (define-key js2-mode-map (kbd "C-c ! .") 'wjb-find-js-definition)
  ;; (define-key js2-mode-map (kbd "C-c ! ,") 'wjb-return-from-js-definition)
  (define-key js2-mode-map (kbd "C-M-n") #'forward-paragraph)
  (define-key js2-mode-map (kbd "C-M-p") #'backward-paragraph)

  (define-key js2-refactor-mode-map (kbd "H-c r l") 'remove-console-log-js)
  (define-key js2-refactor-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-js)
  (define-key js2-refactor-mode-map (kbd "H-c m") 'wjb-mark-this-node)
  (define-key js2-refactor-mode-map (kbd "H-c k") 'wjb-kill-this-node)
  (define-key js2-refactor-mode-map (kbd "H-c r k") 'js2r-kill)

  ;; TODO: fix this
  (load-file "~/.emacs.d/elisp/js-doc/js-doc.el")

  (defun wjb/js2-mode-hook ()
    (define-key js2-mode-map "\C-c@" 'js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map (kbd "H-k") #'wjb-kill-this-node)
    (setq mode-name "JS2" company-backends wjb/company-backends-js)
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

  (add-hook 'js2-mode-hook #'wjb/js2-mode-hook)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  ;; This might slow things down when loading large files?
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  ;; TODO: make minor mode hook more like major mode hook
  (add-hook 'js2-minor-mode-hook #'js2-refactor-mode)
)

(setq-default js2-basic-offset 2)
(setq js2-dynamic-idle-timer-adjust 0
      ;; js2-dynamic-idle-timer-adjust 40000
      js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-basic-offset preferred-javascript-indent-level
      js2-indent-on-enter-key t
      js2-auto-indent-p t
      js2-bounce-indent-p t
      js2-enter-indents-newline nil
      js2-include-rhino-externs nil
      js2-include-gears-externs nil
      js2-idle-timer-delay 0.2 ;; default
      js2-mirror-mode t
      js2-strict-inconsistent-return-warning t
      js2-rebind-eol-bol-keys nil
      js2-concat-multiline-strings 'eol
      js-indent-level preferred-javascript-indent-level
      js2r-prefer-let-over-var t
      js-doc-mail-address "william.bert@gmail.com"
      js-doc-author (format "William Bert <%s>" js-doc-mail-address)
      js-doc-url "williambert.online"
      js-doc-license "MIT")

(add-to-list 'js2-global-externs
             '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "isNaN" "encodeURIComponent" "parseInt"))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

(require 'setup-rjsx-mode)

;; Disabled b/c not worth it to run yet another regex
;; (font-lock-add-keywords
;;  'js2-mode `(("\\(function\\) *("
;;               (0 (progn (compose-region (match-beginning 1)
;;                                         (match-end 1) "\u0192")
;;                         nil)))))

;; Use right arrow for return in one-line functions.
;; (font-lock-add-keywords
;;  'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
;;               (0 (progn (compose-region (match-beginning 1)
;;                                         (match-end 1) "\u2190")
;;                         nil)))))

;; Did this predate using gruvbox, or do I still want it?
;; (set-face-foreground 'js2-object-property "light goldenrod")

;; Only use if js2-highlight-vars-mode is installed.
;; TODO: diminish Js2-Highlight-Vars (indicator vars).
;; TODO: make it stop complaing about no syntax tree available.
;; (eval-after-load "js2-highlight-vars-autoloads"
;;   '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;; from https://github.com/redguardtoo/emacs.d/blob/def7e0496482e1830ff6d1182ff20b2a6fa68160/lisp/init-javascript.el#L66
(eval-after-load 'js-mode
  '(progn
     ;; experimental: make underscore be a symbol, part of a name
     (modify-syntax-entry ?- "_" js-mode-syntax-table)
     (modify-syntax-entry ?- "_" js2-mode-syntax-table)

     ;; '$' is part of variable name like '$item'
     (modify-syntax-entry ?$ "w" js-mode-syntax-table)))

;; Flycheck.
;;
(setq flycheck-eslint-args '("--no-color"))

;; Prettier.
;;
(use-package prettier-js
  :hook ((js-base-mode . prettier-js-mode)
         (typescript-ts-base-mode . prettier-js-mode))
  :config
  (diminish 'prettier-js-mode)
  (setq prettier-js-width-mode 'fill)
  (setq-local prettier-js-args
        '("--single-quote"
          "--trailing-comma"
          "es5")))

;; TODO convert to use-package :bind.
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-base-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-js))

;; always nil!
;; (with-eval-after-load 'tsx-ts-mode (message "loaded"))
;; (with-eval-after-load 'typescript-ts-base-mode (message "loaded"))

(defun wjb/ts-mode-hook ()
  ;; these really only need to be run once, but with-eval-after-load doesn't run
  ;; for tsx-ts-mode or typescript-ts-base-mode, so I'll put them into this hook.
  (define-key typescript-ts-base-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-js)
  (setq company-backends wjb/company-backends-ts))
(add-hook 'typescript-base-mode-hook #'wjb/ts-mode-hook)

(setq typescript-indent-level 2)

(defun wjb/company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun wjb/js-mode-hook nil
  (make-local-variable 'company-transformers)
  (push 'wjb/company-transformer company-transformers)
  (setq-local prettify-symbols-alist nil)
  (setq-local fill-column 80))

(add-hook 'js-base-mode-hook 'wjb/js-mode-hook)



;; Below based on https://github.com/js-emacs/js2-refactor.el/pull/118
(defun js2r--convert-string-delimiter (to-delim)
  "Convert the delimiters of string at point to a specified delimiter TO-DELIM."
  (let ((node (js2-node-at-point)))
    (when (js2-string-node-p node)
      (let* ((start (js2-node-abs-pos node))
             (end (+ start (js2-node-len node)))
             (prev-delim (char-after start)))
        (when (memq prev-delim '(?' ?\" ?`))
          (save-excursion
            (goto-char end) (delete-char -1) (insert to-delim)
            (goto-char start) (delete-char 1) (insert to-delim)
            (perform-replace to-delim (concat "\\" to-delim) nil nil nil nil nil (1+ start) (1- end))
            (perform-replace (concat "\\" (char-to-string prev-delim)) (char-to-string prev-delim) nil nil nil nil nil (1+ start) (1- end))))))))

(defun js2r-cycle-string-literal-type ()
  "Cycle: single -> template -> double -> single, etc."
  (interactive)
  (let ((node (js2-node-at-point)))
    (when (js2-string-node-p node)
      (let* ((start (js2-node-abs-pos node))
             (prev-delim (char-after start)))
        (pcase prev-delim
          (?' (js2r--convert-string-delimiter "`"))
          (?\" (js2r--convert-string-delimiter "'"))
          (?` (js2r--convert-string-delimiter "\"")))))))

(provide 'setup-js2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-js2-mode.el ends here
