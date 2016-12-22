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


(require-package 'json-mode)

(when (>= emacs-major-version 24)
  (require-package 'js2-mode)
  (require-package 'ac-js2)
  (require-package 'coffee-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)

(require-package 'js-comint)

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "H-c")

(after-load 'js2-mode
  (define-key js2-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
  (define-key js2-mode-map (kbd "H-r") 'js2r-rename-var)
  (define-key js2-mode-map (kbd "H-c r m") 'remove-console-log-js)
  )

;; Don't redefine C-a for me please, js2-mode
;(define-key js2-mode-map (kbd "C-a") nil)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; js2-mode
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  ;(add-hook 'js2-mode-hook #sanityinc/disable-js2-checks-if-flycheck-active)
  )

;; from http://emacs.stackexchange.com/a/21207
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (file-executable-p eslint)
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq js2-dynamic-idle-timer-adjust 40000)

(after-load 'js2-mode
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2"))))

(setq js2-use-font-lock-faces t
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
      )

(setq-default js2-global-externs
              '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

;; This might slow things down when loading large files?
;; (after-load 'js2-mode
;;   (js2-imenu-extras-setup))

;; Set js-mode's indent level (it seems to ignore tab-width).
(setq js-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

;; Javascript nests {} and () a lot, so I find this helpful
(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; js-doc
(setq js-doc-mail-address "william.bert@gmail.com"
      js-doc-author (format "William Bert <%s>" js-doc-mail-address)
      js-doc-url "williambert.online"
      js-doc-license "MIT")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-c@" 'js-doc-insert-function-doc)))

;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; TODO: write-file-functions is global, so this is a pretty dumb hook. Fix it.
(add-hook 'coffee-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(setq inferior-js-program-command "node --interactive")

;; Fix garbage in prompt: http://stackoverflow.com/questions/13862471
;;
(setenv "NODE_NO_READLINE" "1")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
                                        ;(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
                                        ;(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
                                        ;(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))

;;;;;;;;;
;; nvm ;;
;;;;;;;;;

(require-package 'nvm)
(require 'nvm)

(defun do-nvm-use (version)
  (interactive "sVersion: ")
  (nvm-use version)
  ;; exec-path-from-shell made a new login shell at startup and imported values,
  ;; including PATH to exec-path. But nvm-use does setenv "PATH". So we need to
  ;; update exec-path to the current PATH in the Emacs process.
  (exec-path-from-shell-copy-env "PATH")
  ;; TODO: Unset a Node. Remove node from PATH. Could use setenv with no
  ;; argument.
  )

(defun run-node (cwd)
  (interactive "DDirectory: ")
  (unless (executable-find "node")
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (pop-to-buffer (make-comint (format "node-repl-%s" cwd) "node" nil "--interactive"))))

(defalias 'node-repl 'run-node)

(defun run-coffee (cwd)
  (interactive "DDirectory: ")
  (unless (and (executable-find "node") (executable-find "coffee"))
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (call-interactively 'coffee-repl)))

(defun run-nesh (cwd)
  "Run Nesh."
  (interactive "DDirectory: ")
  (unless (and (executable-find "node") (executable-find "nesh"))
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (pop-to-buffer (make-comint (format "nesh-repl-%s" cwd) "nesh" nil "--interactive"))))

                                        ;(defalias 'coffee-repl 'run-coffee) ;; (Overwrites defun in coffee-mode.el.)

;; Needs Node to really honor NODE_NO_READLINE. See:
;; https://github.com/joyent/node/issues/5344
(defun run-coffee-someday (cwd)
  "Run Coffeescript."
  (interactive "DDirectory: ")
  (unless (and (executable-find "node") (executable-find "coffee"))
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (pop-to-buffer
     (apply 'make-comint (format "coffee-repl-%s" cwd)
            "env"
            nil
            "NODE_NO_READLINE=1"
            "coffee"
            (list "--interactive")))))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

;; (when (and (>= emacs-major-version 24) (featurep 'js2-mode))
;;   (require-package 'skewer-mode)
;;   (after-load 'skewer-mode
;;     (add-hook 'skewer-mode-hook
;;               (lambda () (inferior-js-keys-mode -1)))))


;; Use lambda for anonymous functions.
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions.
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u2190")
                        nil)))))

;; TODO: implement recommendations from
;; http://yoo2080.wordpress.com/2012/03/15/js2-mode-setup-recommendation/
(setq-default js2-basic-offset 2)

;; js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  "Tab properly."
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command)
      (if (looking-back "^\s+")
          (back-to-indentation)))))

(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)

(js2r-add-keybindings-with-prefix "C-c C-r")

(add-hook 'json-mode 'flymake-json-load)

(set-face-foreground 'js2-object-property "light goldenrod")

(provide 'setup-js2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-js2-mode.el ends here
