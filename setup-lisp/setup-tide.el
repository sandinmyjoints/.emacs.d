;;; setup-tide.el ---
;;
;; Filename: setup-tide.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Fri Sep 26 12:03:22 2025 (-0400)
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

(use-package tide)

(use-package tide
  :disabled
  ;; :demand ;; when I use this, then I have to manually eval to load tide
  :after (company flycheck)
  ;; tide-mode binds these to tide defuns, but I've set up smart-jump to do the tide stuff plus some fallbacks
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("C-?" . tide-documentation-at-point)
         ("M-?" . tide-references))
  :hook ((js2-mode . tide-setup)
         (typescript-mode . tide-setup)
         (typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup))
  :config
  (setq tide-tsserver-start-method 'manual
        tide-disable-suggestions t ;; trying this out
        tide-native-json-parsing t
        tide-default-mode "JS"
        tide-hl-identifier-idle-time 0.1
        tide-filter-out-warning-completions t
        tide-sync-request-timeout 5
        tide-project-cleanup-delay (* 20 60)
        tide-tsserver-process-environment '("NODE_OPTIONS=--max-old-space-size=4096")
        ;; tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log" "NODE_OPTIONS=--max-old-space-size=8192")
        tide-server-max-response-length (* 2 1024 1024))
  (setq tide-always-show-documentation t)

  ;; tide places company-tide first :(
  (pop company-backends)

  ;; If I'm working with typescript, and eslint is configured to lint TS, then
  ;; this is useful. For example, eslint is configured for TS in playground.
  ;; Running eslint with TS type checking rules turned on can be pretty slow, so
  ;; only do it if TS itself did not complain.
  (flycheck-add-next-checker 'typescript-tide '(error . javascript-eslint) 'append)
  (flycheck-add-next-checker 'tsx-tide '(error . javascript-eslint) 'append)

  ;; monkey patch tide-start-server to generate a new buffer name that includes
  ;; the project name.
  (defun tide-start-server ()
    (when (tide-current-server)
      (error "Server already exist"))

    (message "(%s) Starting tsserver..." (tide-project-name))
    (let* ((default-directory (tide-project-root))
           (process-environment (append tide-tsserver-process-environment process-environment))
           (buf (generate-new-buffer (concat tide-server-buffer-name "-" (file-name-nondirectory (directory-file-name default-directory)))))
           (tsserverjs (tide-locate-tsserver-executable))
           ;; Use a pipe to communicate with the subprocess. This fixes a hang
           ;; when a >1k message is sent on macOS.
           (process-connection-type nil)
           (node-process-arguments (append tide-node-flags (list tsserverjs) tide-tsserver-flags))
           (process
            (apply #'start-file-process "tsserver" buf tide-node-executable node-process-arguments)))
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (set-process-filter process #'tide-net-filter)
      (set-process-sentinel process #'tide-net-sentinel)
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer (process-buffer process)
        (buffer-disable-undo))
      (process-put process 'project-name (tide-project-name))
      (process-put process 'project-root default-directory)
      (puthash (tide-project-name) process tide-servers)
      (message "(%s) tsserver server started successfully." (tide-project-name))
      (tide-each-buffer (tide-project-name) #'tide-configure-buffer)))

  (flycheck-add-mode 'typescript-tide 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'typescript-tide 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  ;; For now, need to manually get flycheck to realize typescript-tide works for typescript-ts-mode
  )

(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-base-mode-map (kbd "H-c") 'tide-refactor)
  (define-key typescript-ts-base-mode-map "\C-c@" 'tide-jsdoc-template))

(defun wjb/ts-mode-tide-hook ()
  ;; these really only need to be run once, but with-eval-after-load doesn't run
  ;; for tsx-ts-mode or typescript-ts-base-mode, so I'll put them into this hook.
  (define-key typescript-ts-base-mode-map (kbd "H-c") 'tide-refactor)
  (define-key typescript-ts-base-mode-map "\C-c@" 'tide-jsdoc-template))

(add-hook 'typescript-base-mode-hook #'wjb/ts-mode-tide-hook)

;; monkey patch
(defun tide-make-help-buffer (feature body)
  (with-current-buffer (get-buffer-create (concat "*tide-" feature "*"))
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when body
        (save-excursion
          (tide-insert body))))
    ;; wjb -- my change
    (local-set-key (kbd "q") #'other-frame)
    ;; (local-set-key (kbd "q") #'quit-window)
    (current-buffer)))

;; - Configure javascript-eslint to run after tide checkers (but eslint is still
;; the default checker; this only has an effect when the tide checkers are
;; enabled).
;; - These should probably be moved inside tide-setup or :config or something.
;;
;; Generally, I only use javascript-eslint inside js and jsx files. If I want to use
;; tide, then these lines will run ride as well as eslint:
(flycheck-add-next-checker 'javascript-tide 'javascript-eslint 'append)
(flycheck-add-next-checker 'jsx-tide 'javascript-eslint 'append)

;; define custom.
;; heuristic is used to know whether the jump succeeded or not.
;; error means it failed if an error was signaled.
;; point means it failed if point is the same after the jump as before.
;; smart-jump-list is buffer-local variable that contains the jumps that are in effect.
(with-eval-after-load 'smart-jump (smart-jump-register :modes 'js2-mode
                                                       :jump-fn 'tide-jump-to-definition
                                                       :pop-fn 'tide-jump-back
                                                       :refs-fn 'tide-references
                                                       :should-jump t
                                                       :heuristic 'point
                                                       :async t
                                                       :order 1))

(provide 'setup-tide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-tide.el ends here
