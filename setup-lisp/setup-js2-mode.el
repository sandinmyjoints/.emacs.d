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
  (require-package 'coffee-mode))

(require-package 'js-comint)

;; js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  "Tab properly."
  (interactive)

  ;; yas works differently now, this may be obsolete. See yas-fallback-behavior documentation.
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command)
      (if (looking-back "^\s+")
          (back-to-indentation)))))

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "H-c")
(js2r-add-keybindings-with-prefix "H-r")

;; Tern uses C-c C-r.
;; https://ternjs.net/doc/manual.html#emacs
;; (js2r-add-keybindings-with-prefix "C-c C-r")

(after-load 'js2-mode
  ;; (define-key js2-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)
  (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
  ;;(define-key js2-mode-map (kbd "H-r") 'js2r-rename-var) ;; H-c r v
  (define-key js2-refactor-mode-map (kbd "H-c r l") 'remove-console-log-js)
  (define-key js2-refactor-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-js)
  (define-key js2-refactor-mode-map (kbd "H-c m") 'wjb-mark-this-node)
  (define-key js2-refactor-mode-map (kbd "H-c k") 'wjb-kill-this-node)
  (define-key js2-refactor-mode-map (kbd "H-c r k") 'js2r-kill))


;; Don't redefine C-a for me please, js2-mode
;(define-key js2-mode-map (kbd "C-a") nil)

(eval-when-compile (require 'cl))
(defcustom preferred-javascript-mode
  (cl-first (cl-remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-javascript-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))

;; js2-mode
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-skip-preprocessor-directives t)
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
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/use-coffeelint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (coffeelint (and root
                          (expand-file-name "node_modules/coffeelint/bin/coffeelint"
                                            root))))
    (when (and coffeelint (file-executable-p coffeelint))
      (setq-local flycheck-coffee-coffeelint-executable coffeelint))))

(add-hook 'coffee-mode-hook #'my/use-coffeelint-from-node-modules)

(defun my/use-coffee-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (coffee (and root
                      (expand-file-name "node_modules/.bin/coffee"
                                        root))))
    (when (and coffee (file-executable-p coffee))
      (setq-local flycheck-coffee-executable coffee))))

(add-hook 'coffee-mode-hook #'my/use-coffee-from-node-modules)

;; (setq js2-dynamic-idle-timer-adjust 40000)
(setq js2-dynamic-idle-timer-adjust 0)

(after-load 'js2-mode
  (add-hook 'js2-mode-hook #'(lambda () (setq mode-name "JS2")))
  (add-hook 'js2-mode-hook #'(lambda () (electric-pair-mode 1)))) ;; maybe?

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

(add-to-list 'js2-global-externs
              '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "isNaN" "encodeURIComponent" "parseInt"))

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

(setq inferior-js-program-command "node")
(setq inferior-js-program-arguments '("--interactive"))
(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

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

;; run-js?
;; indium-run-node
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

;; Did this predate using gruvbox, or do I still want it?
;; (set-face-foreground 'js2-object-property "light goldenrod")

;; Only use if js2-highlight-vars-mode is installed.
;; TODO: diminish Js2-Highlight-Vars (indicator vars)
(eval-after-load "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))



;; js-comint
;; To set/change version of node js, run `js-select-node-version'
(autoload 'js-comint "js-select-node-version" "Add directory to tree view")
(autoload 'js-comint "run-js" "Add directory to tree view")

(setq js-use-nvm t)
(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)


;; xref
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


;; company and tern
(when (require 'company nil t)
  (require 'company-tern nil t)

  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)
                             (company-mode))))

;; Just placing this here for now. Company stuff should probably be in its own
;; file.
(add-to-list 'company-backends 'company-restclient)
(when (require 'company-emoji nil t)
  (add-to-list 'company-backends 'company-emoji))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)


;; prettier
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(defun my/use-prettier-if-in-node-modules ()
  "Use prettier-js-mode if prettier is found in this file's project's node_modules."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                      (expand-file-name "node_modules/prettier/bin/prettier.js"
                                        root))))
    (when (and prettier (file-executable-p prettier))
      (prettier-js-mode))))

(when (require 'prettier-js nil t)
  (add-hook 'js2-mode-hook #'my/use-prettier-if-in-node-modules)
  (setq prettier-js-width-mode 80)
  (setq prettier-js-args '(
                           "--single-quote"
                           "--trailing-comma"
                           "es5"
                           )))


;; indium
;; (when (require 'indium nil t)
;;   (add-hook 'js-mode-hook #'indium-interaction-mode))

;;(add-hook 'coffee-mode-hook 'smart-indent-rigidly-mode) ;; clobbers TAB for yasnippet/expand

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

(provide 'setup-js2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-js2-mode.el ends here
