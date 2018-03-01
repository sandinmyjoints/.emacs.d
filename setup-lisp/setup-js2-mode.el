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

;; To jump back:
;; xref-pop-marker-stack

;; Run each in succession.
;; How does each signal success or failure?
;; xref-find-definitions is best if it goes to a result
;; then tern-find-definition
;; then js2-jump-to-definition
;; then xref-find-definitions listing things
(defun wjb-find-js-definition ()
  (interactive)
  (point-to-register ?=)
  (let ((wjb-find-js-definition-initial (point-marker)))
    ;; (message "000")
    (tern-find-definition)
    ;; if point hasn't moved, tern didn't find it
    (if (equal (point-marker) wjb-find-js-definition-initial)
        (progn
          ;; (message "111")
          (condition-case nil
              (js2-jump-to-definition)
            ;; if js2-jump-to-definition errored or didn't move point, it didn't find it
            (error
             (progn
               ;; (message "222a")
               (xref-find-definitions (thing-at-point 'symbol)))))
          (if (equal (point-marker) wjb-find-js-definition-initial)
              (progn
                ;; (message "222b")
                (message (thing-at-point 'symbol))
                (xref-find-definitions (thing-at-point 'symbol))))))))

(defun wjb-return-from-js-definition ()
  (interactive)
  (jump-to-register ?=))


(use-package js2-refactor
  :config
  (js2r-add-keybindings-with-prefix "H-c")
  (js2r-add-keybindings-with-prefix "H-r"))

;; Company and Tern.

;; Last argument makes tern the last hook to run, which is good
;; because it needs to come after nvm-use-for-buffer.
(add-hook 'js2-mode-hook #'tern-mode t)

(when (require 'company nil t)
  (require 'company-tern nil t)
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda () (company-mode))))

;; Just placing this here for now. Company stuff should probably be in its own
;; file.
(add-to-list 'company-backends 'company-restclient)
(when (require 'company-emoji nil t)
  (add-to-list 'company-backends 'company-emoji))

;; Disable completion keybindings, as we use xref-js2 and
;; js2-jump-to-definition instead.
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(define-prefix-command 'tern-js2-map)
(define-key tern-js2-map (kbd "M-.") 'tern-find-definition)
(define-key tern-js2-map (kbd "M-,") 'tern-pop-find-definition)

(after-load 'js2-mode
  ;; (define-key js2-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key js2-mode-map (kbd "C-M-h") 'js2-mark-defun)
  (define-key js2-mode-map (kbd "H-t") 'tern-js2-map)
  (define-key js2-mode-map (kbd "C-c ! .") 'wjb-find-js-definition)
  (define-key js2-mode-map (kbd "C-c ! ,") 'wjb-return-from-js-definition)
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
  (add-hook 'js2-mode-hook #'(lambda () (define-key js2-mode-map "\C-c@" 'js-doc-insert-function-doc)))
  (add-hook 'js2-mode-hook #'(lambda () (setq mode-name "JS2")))
  (add-hook 'js2-mode-hook #'(lambda () (electric-pair-mode 1))) ;; maybe?
  (add-hook 'js2-mode-hook #'(lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (add-hook 'js2-mode-hook #'js2-refactor-mode)

  ;; This might slow things down when loading large files?
  ;; (add-hook 'js2-mode-hook  #'js2-imenu-extras-setup)

  ;; put towards the end so it runs early (hooks are added to
  ;; beginning of list)
  (add-hook 'js2-mode-hook #'nvm-use-for-buffer)
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
      js-doc-mail-address "william.bert@gmail.com"
      js-doc-author (format "William Bert <%s>" js-doc-mail-address)
      js-doc-url "williambert.online"
      js-doc-license "MIT")

(add-to-list 'js2-global-externs
              '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "isNaN" "encodeURIComponent" "parseInt"))

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

;; Not sure which of these is better. Hmm, rjsx-mode hung Emacs...
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))

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

;; Did this predate using gruvbox, or do I still want it?
;; (set-face-foreground 'js2-object-property "light goldenrod")

;; Only use if js2-highlight-vars-mode is installed.
;; TODO: diminish Js2-Highlight-Vars (indicator vars)
(eval-after-load "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;; These defuns may be replaceable by
;; https://github.com/codesuki/add-node-modules-path

;; Flycheck.
;;
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

;; Prettier.
;;
;; TODO: This can probably be updated to work with
;; add-node-modules-to-path.
(defun my/use-prettier-if-in-node-modules ()
  "Use prettier-js-mode if prettier is found in this file's
project's node_modules. Use the prettier binary from this
project."
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
      (prettier-js-mode))))

(when (require 'prettier-js nil t)
  (make-variable-buffer-local 'prettier-js-command)
  (add-hook 'js2-mode-hook #'my/use-prettier-if-in-node-modules)
  (setq prettier-js-width-mode 'fill)
  (setq prettier-js-args
        '("--single-quote"
          "--trailing-comma"
          "es5")))

;; indium
;; (when (require 'indium nil t)
;;   (add-hook 'js-mode-hook #'indium-interaction-mode))

;; (add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(provide 'setup-js2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-js2-mode.el ends here
