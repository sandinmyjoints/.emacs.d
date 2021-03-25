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

(require-package 'js2-mode)

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

(after-load 'js2-mode
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
  ;;(add-hook 'js2-mode-hook #sanityinc/disable-js2-checks-if-flycheck-active)

  ;; TODO: fix this
  (load-file "~/.emacs.d/elisp/js-doc/js-doc.el")

  (defun wjb/js2-mode-hook ()
    (define-key js2-mode-map "\C-c@" 'js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map (kbd "H-k") #'wjb-kill-this-node)
    ;; (setq-local imenu-create-index-function 'js2-custom-imenu-make-index)
    (setq mode-name "JS2"
          company-backends wjb/company-backends-js)
    (electric-pair-mode 1)
    ;; (require 'smartparens-javascript)
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
  (add-hook 'js2-mode-hook #'wjb/js2-mode-hook)

  ;; not quite ready for prime time:
  ;; (add-hook 'js2-mode-hook #'tree-sitter-mode)
  ;; (add-hook 'js2-mode-hook #'tree-sitter-hl-mode)

  (add-hook 'js2-mode-hook #'js2-refactor-mode)

  ;; This might slow things down when loading large files?
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

  ;; TODO: make minor mode hook more like major mode hook
  (add-hook 'js2-minor-mode-hook #'js2-refactor-mode)

  ;; put towards the end so it runs early (hooks are added to
  ;; beginning of list). This hook only runs when a JS file is opened,
  ;; so TODO: give nvm more opportunities to switch to correct node.
  ;; - hook for switching buffers
  ;; - hook where projectile knows when project changes?
  (add-hook 'js2-mode-hook #'nvm-use-for-buffer)
  (add-hook 'js2-minor-mode-hook #'nvm-use-for-buffer)
  (add-hook 'yml-mode-hook #'nvm-use-for-buffer)
  (add-hook 'shell-script-mode-hook #'nvm-use-for-buffer)
  (add-hook 'projectile-after-switch-project-hook #'nvm-use-for-buffer)

  ;; HACK
  (defun nvm-use-for-buffer ()
    "Activate Node based on an .nvmrc for the current file.
If buffer is not visiting a file, do nothing."
    (when (or buffer-file-name (string-match "\`\*magit" (buffer-name)))
      (condition-case err
          (nvm-use-for buffer-file-name)
        (error (message "%s" err))))))

;; TODO: default nvm to be used at startup, before any project has been activated.

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

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

;; Which mode(s) to use for JSX?
;; - could try js2-jsx-mode by itself
;; - web-mode + js2-jsx-mode is pretty good but has some quirks, and js2r doesn't work b/c it doesn't support the js2 parse tree.
;; - rsjx-mode works with js2r but it has had a tendency to hang when attributes are malformed.
;; - in emacs 27, js-mode with js2-minor-mode is recommended for JSX, but js2 doesn't work.
;;
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)) ;; rjsx can parse spread operator
(add-to-list 'auto-mode-alist '("\\.min\\.js\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("-min\\.js\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("-min-async\\.js\\'" . fundamental-mode))

;; from https://github.com/felipeochoa/rjsx-mode/issues/112#issuecomment-530497532
(defun +javascript-rjsx-electric-gt-a (n)
  (when (and (looking-back "<>")
             (looking-at-p "/>"))
    (save-excursion (insert "<"))))
(advice-add #'rjsx-electric-gt :after #'+javascript-rjsx-electric-gt-a)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (cl-loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))

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

;;(setq add-node-modules-path-debug t)
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))
(eval-after-load 'js2-minor-mode
  '(add-hook 'js2-minor-mode-hook #'add-node-modules-path))

;; from https://github.com/redguardtoo/emacs.d/blob/def7e0496482e1830ff6d1182ff20b2a6fa68160/lisp/init-javascript.el#L66
(eval-after-load 'js-mode
  '(progn
     ;; experimental:
     (modify-syntax-entry ?- "_" js-mode-syntax-table)
     (modify-syntax-entry ?- "_" js2-mode-syntax-table)

     ;; '$' is part of variable name like '$item'
     (modify-syntax-entry ?$ "w" js-mode-syntax-table)))

;; The following defuns may be replaceable by
;; https://github.com/codesuki/add-node-modules-path

;; Flycheck.
;;
(make-variable-buffer-local 'flycheck-javascript-eslint-executable)
(setq flycheck-eslint-args '("--no-color"))

;; from http://emacs.stackexchange.com/a/21207
(defun my/use-eslint-from-node-modules ()
  ;; TODO: may need to add web-mode or some js-specific minor mode from it to
  ;; this to get proper eslint when using web-mode for jsx files
  ;; TODO: use flycheck-executable-find
  (when (derived-mode-p 'js-mode)
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root)))
           (eslint_d (executable-find "eslint_d")))
      ;; (message (format "vars: 1 %s 2 %s 3 %s" root eslint eslint_d))
      ;; eslint_d has stopped working in jsx files, so disabling it.
      (if (and nil (file-executable-p (format "%s" eslint_d)))
          (setq-local flycheck-javascript-eslint-executable eslint_d)
        (when (and eslint (file-executable-p (format "%s" eslint)))
          (setq-local flycheck-javascript-eslint-executable eslint))))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Prettier.
;;
;; TODO: This can probably be updated to work with
;; add-node-modules-to-path.
;; or even better would be: $ "$(npm bin)/prettier"
(defun my/use-prettier-if-in-node-modules ()
  "Use prettier-js-mode if prettier is found in this file's
project's node_modules. Use the prettier binary from this
project."
  (when (derived-mode-p 'js-mode)
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (prettier (and root
                          (expand-file-name "node_modules/prettier/bin/prettier.js"
                                            root)))
           (prettier2 (and root
                           (expand-file-name "node_modules/prettier/bin-prettier.js"
                                             root))))
      (when (and prettier (file-executable-p prettier))
        (setq prettier-js-command prettier)
        (prettier-js-mode))
      (when (and prettier2 (file-executable-p prettier2))
        (setq prettier-js-command prettier2)
        (prettier-js-mode)))))

(when (require 'prettier-js nil t)
  (diminish 'prettier-js-mode)
  (make-variable-buffer-local 'prettier-js-command)
  (add-hook 'js2-mode-hook #'my/use-prettier-if-in-node-modules)
  (add-hook 'js2-minor-mode-hook #'my/use-prettier-if-in-node-modules)
  (setq prettier-js-width-mode 'fill)
  (setq prettier-js-args
        '("--single-quote"
          "--trailing-comma"
          "es5")))

(use-package tide
  :demand
  ;; tide-mode binds these to tide defuns, but I've set up smart-jump to do the tide stuff plus some fallbacks
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . tide-references))
  :after (js2-mode company)
  :hook ((js2-mode . tide-setup) (typescript-mode . tide-setup))
  :config
  (setq tide-tsserver-start-method 'manual)
  (tide-setup)
  (setq tide-tsserver-start-method 'manual
        tide-disable-suggestions t ;; trying this out
        tide-native-json-parsing t
        tide-default-mode "JS"
        tide-hl-identifier-idle-time 0.1
        tide-filter-out-warning-completions t
        tide-sync-request-timeout 5
        tide-project-cleanup-delay (* 20 60)
        tide-server-max-response-length (* 10 256 1024))
  ;; tide places company-tide first :(
  (pop company-backends)
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

  ;; what might actually be helpful:
  ;; ;; (encoded-command (json-encode command)) ;; WJB
  ;; (encoded-command (json-serialize command))
  ;; and converting json-read-object in tide-decode-response
  )

(defun wjb/ts-mode-hook ()
  (setq company-backends wjb/company-backends-ts))
(add-hook 'typescript-mode-hook #'wjb/ts-mode-hook)

;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-tide 'javascript-eslint 'append)
(flycheck-add-next-checker 'jsx-tide 'javascript-eslint 'append)

(defun wjb/company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun wjb/js-mode-hook nil
  (make-local-variable 'company-transformers)
  (push 'wjb/company-transformer company-transformers)
  (setq-local prettify-symbols-alist nil)
  (setq-local fill-column 80))

(add-hook 'js-mode-hook 'wjb/js-mode-hook)

;; this imenu generic expression aims to exclude for, while, if when aims to match functions in
;; es6 js, e.g. ComponentDidMount(), render() function in React
;; https://emacs-china.org/t/topic/4538/7
(defun js-exception-imenu-generic-expression-regexp ()
  ;; (async)? xxx (e) { }
  (if (re-search-backward "^[ \t]*\(async\)?[ \t]*\([A-Za-z_$][A-Za-z0-9_$]+\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" nil t)
      (progn
        (if (member (match-string 2) '("for" "if" "while" "switch"))
            (js-exception-imenu-generic-expression-regexp)
          t))
    nil))

;; See https://github.com/mooz/js2-mode/issues/274 -- looks promising, but I haven't wired it up yet.

;; Below is from https://stackoverflow.com/a/21656063/599258
;; this looks promising, but I haven't wired it up yet.
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun wjb/js2-mode-hook-poc ()
  (add-to-list
   'imenu-generic-expression
   '("describe" "\s-*describe\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)))

(remove-hook 'js2-mode-hook #'wjb/js2-mode-hook-poc)

(defun js2-custom-imenu-make-index-poc ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\(\"\(.+\)\"" 1)))
    (imenu--generic-function '(
                               ("describe" "\s-*describe\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("describeX" "describe(\(.+\))" 1)
                               ))))

;; based on https://ztlevi.github.io/posts/Get%20your%20imenu%20ready%20for%20modern%20javascript/
(defun js2-custom-imenu-make-index-interactive ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\(\"\(.+\)\"" 1)))
    (imenu--generic-function '(
                               ("describe" "\s-*describe\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("describeX" "describe(\(.+\))" 1)
                               ("it" "\s-*it\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("test" "\s-*test\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("before" "\s-*before\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("beforeEach" "\s-*beforeEach\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("after" "\s-*after\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                               ("afterEach" "\s-*afterEach\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)

                               ("Class" "^[ \t]*[0-9a-zA-Z_$ ]*[ \t]*class[ \t]*\([a-zA-Z_$.]*\)" 1)
                               ("Class" "^[ \t]*\(var\|let\|const\)[ \t]*\([0-9a-zA-Z_$.]+\)[ \t]*=[ \t]*[a-zA-Z_$.]*.extend" 2)
                               ("Class" "^[ \t]*cc\.\(.+\)[ \t]*=[ \t]*cc\..+\.extend" 1)

                               ("Function" "\(async\)?[ \t]*function[ \t]+\([a-zA-Z0-9_$.]+\)[ \t]*(" 2) ;; (async)? function xxx (
                               ("Function" "^[ \t]*\([a-zA-Z0-9_$.]+\)[ \t]*:[ \t]*\(async\)?[ \t]*function[ \t]*(" 1) ;; xxx : (async)? function (
                               ("Function" "^[ \t]*\(export\)?[ \t]*\(var\|let\|const\)?[ \t]*\([a-zA-Z0-9_$.]+\)[ \t]*=[ \t]*\(async\)?[ \t]*function[ \t]*(" 3) ;; (export)? (var|let|const)? xxx = (async)? function (

                               ;; {{ es6 beginning
                               ("Function" js-exception-imenu-generic-expression-regexp 2) ;; (async)? xxx (e) { }
                               ("Function" "^[ \t]*\([A-Za-z_$][A-Za-z0-9_$.]*\)[ \t]*:[ \t]*\(async\)?[ \t]*(" 1) ;; xxx : (async)? (
                               ("Function" "^[ \t]*\(export\)?[ \t]*\(var\|let\|const\)?[ \t]*\([A-Za-z_$][A-Za-z0-9_$.]*\)[ \t]*=[ \t]*\(async\)?[ \t]*(" 3) ;; (export)? (var|let|const)? xxx = (async)? (
                               ("Function" "^[ \t]*\(export\)?[ \t]*\(var\|let\|const\)?[ \t]*\([A-Za-z_$][A-Za-z0-9_$.]*\)[ \t]*=[ \t]*\(async\)?[ \t]*[A-Za-z_$][A-Za-z0-9_$.]*[ \t]*=>" 3) ;; (export)? (var|let|const)? xxx = (async)? e =>
                               ;; }}
                               ))))

(defun js2-custom-imenu-make-index ()
  ;; (setq imenu-generic-expression '((nil "describe\(\"\(.+\)\"" 1)))
  (imenu--generic-function '(
                             ("describe" "\s-*describe\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                             ("it" "\s-*it\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                             ("test" "\s-*test\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                             ("before" "\s-*before\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                             ("beforeEach" "\s-*beforeEach\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                             ("after" "\s-*after\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)
                             ("afterEach" "\s-*afterEach\s-*(\s-*[\"']\(.+\)[\"']\s-*,.*" 1)

                             ("Class" "^[ \t]*[0-9a-zA-Z_$ ]*[ \t]*class[ \t]*\([a-zA-Z_$.]*\)" 1)
                             ("Class" "^[ \t]*\(var\|let\|const\)[ \t]*\([0-9a-zA-Z_$.]+\)[ \t]*=[ \t]*[a-zA-Z_$.]*.extend" 2)
                             ("Class" "^[ \t]*cc\.\(.+\)[ \t]*=[ \t]*cc\..+\.extend" 1)

                             ("Function" "\(async\)?[ \t]*function[ \t]+\([a-zA-Z0-9_$.]+\)[ \t]*(" 2) ;; (async)? function xxx (
                             ("Function" "^[ \t]*\([a-zA-Z0-9_$.]+\)[ \t]*:[ \t]*\(async\)?[ \t]*function[ \t]*(" 1) ;; xxx : (async)? function (
                             ("Function" "^[ \t]*\(export\)?[ \t]*\(var\|let\|const\)?[ \t]*\([a-zA-Z0-9_$.]+\)[ \t]*=[ \t]*\(async\)?[ \t]*function[ \t]*(" 3) ;; (export)? (var|let|const)? xxx = (async)? function (

                             ;; {{ es6 beginning
                             ("Function" js-exception-imenu-generic-expression-regexp 2) ;; (async)? xxx (e) { }
                             ("Function" "^[ \t]*\([A-Za-z_$][A-Za-z0-9_$.]*\)[ \t]*:[ \t]*\(async\)?[ \t]*(" 1) ;; xxx : (async)? (
                             ("Function" "^[ \t]*\(export\)?[ \t]*\(var\|let\|const\)?[ \t]*\([A-Za-z_$][A-Za-z0-9_$.]*\)[ \t]*=[ \t]*\(async\)?[ \t]*(" 3) ;; (export)? (var|let|const)? xxx = (async)? (
                             ("Function" "^[ \t]*\(export\)?[ \t]*\(var\|let\|const\)?[ \t]*\([A-Za-z_$][A-Za-z0-9_$.]*\)[ \t]*=[ \t]*\(async\)?[ \t]*[A-Za-z_$][A-Za-z0-9_$.]*[ \t]*=>" 3) ;; (export)? (var|let|const)? xxx = (async)? e =>
                             ;; }}
                             )))

;; following based on https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-javascript.el
(defvar js2-imenu-original-item-lines nil
  "List of line information of original imenu items.")

(defun js2-imenu--get-line-start-end (pos)
  (let* (b e)
    (save-excursion
      (goto-char pos)
      (setq b (line-beginning-position))
      (setq e (line-end-position)))
    (list b e)))

(defun js2-imenu--get-pos (item)
  (let* (val)
    (cond
     ((integerp item)
      (setq val item))

     ((markerp item)
      (setq val (marker-position item))))

    val))

(defun js2-imenu--get-extra-item-pos (item)
  (let* (val)
    (cond
     ((integerp item)
      (setq val item))

     ((markerp item)
      (setq val (marker-position item)))

     ;; plist
     ((and (listp item) (listp (cdr item)))
      (setq val (js2-imenu--get-extra-item-pos (cadr item))))

     ;; alist
     ((and (listp item) (not (listp (cdr item))))
      (setq val (js2-imenu--get-extra-item-pos (cdr item)))))

    val))

(defun js2-imenu--extract-line-info (item)
  "Recursively parse the original imenu items created by js2-mode.
The line numbers of items will be extracted."
  (let* (val)
    (if item
        (cond
         ;; Marker or line number
         ((setq val (js2-imenu--get-pos item))
          (push (js2-imenu--get-line-start-end val)
                js2-imenu-original-item-lines))

         ;; The item is Alist, example: (hello . 163)
         ((and (listp item) (not (listp (cdr item))))
          (setq val (js2-imenu--get-pos (cdr item)))
          (if val (push (js2-imenu--get-line-start-end val)
                        js2-imenu-original-item-lines)))

         ;; The item is a Plist
         ((and (listp item) (listp (cdr item)))
          (js2-imenu--extract-line-info (cadr item))
          (js2-imenu--extract-line-info (cdr item)))

         ;;Error handling
         (t (message "Impossible to here! item=%s" item))))))

(defun js2-imenu--item-exist (pos lines)
  "Try to detect does POS belong to some LINE"
  (let* (rlt)
    (dolist (line lines)
      (if (and (< pos (cadr line)) (>= pos (car line)))
          (setq rlt t)))
    rlt))

(defun js2-imenu--is-item-already-created (item)
  (unless (js2-imenu--item-exist
           (js2-imenu--get-extra-item-pos item)
           js2-imenu-original-item-lines)
    item))

(defun js2-imenu--check-single-item (r)
  (cond
   ((and (listp (cdr r)))
    (let (new-types)
      (setq new-types
            (delq nil (mapcar 'js2-imenu--is-item-already-created (cdr r))))
      (if new-types (setcdr r (delq nil new-types))
        (setq r nil))))
   (t (if (js2-imenu--item-exist (js2-imenu--get-extra-item-pos r)
                                 js2-imenu-original-item-lines)
          (setq r nil))))
  r)

(defun js2-imenu--remove-duplicate-items (extra-rlt)
  (delq nil (mapcar 'js2-imenu--check-single-item extra-rlt)))

(defun js2-imenu--merge-imenu-items (rlt extra-rlt)
  "RLT contains imenu items created from AST.
EXTRA-RLT contains items parsed with simple regex.
Merge RLT and EXTRA-RLT, items in RLT has *higher* priority."
  ;; Clear the lines.
  (set (make-variable-buffer-local 'js2-imenu-original-item-lines) nil)
  ;; Analyze the original imenu items created from AST,
  ;; I only care about line number.
  (dolist (item rlt)
    (js2-imenu--extract-line-info item)))

;; (defadvice js2-mode-create-imenu-index (around my-js2-mode-create-imenu-index activate)
;;   (let (rlt extra-rlt)
;;     ad-do-it
;;     (setq extra-rlt (js2-custom-imenu-make-index))
;;           ;; (save-excursion
;;           ;;   (imenu--generic-function js2-custom-imenu-make-index))
;;     (setq ad-return-value (js2-imenu--merge-imenu-items ad-return-value extra-rlt))
;;     ad-return-value))

(provide 'setup-js2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-js2-mode.el ends here
