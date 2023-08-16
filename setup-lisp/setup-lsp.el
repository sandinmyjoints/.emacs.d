(use-package eglot
  :disabled
  :config
  ;; TODO: find a language server that actually works with JSX
  ;; (add-to-list 'eglot-server-programs '(rjsx-mode . ("typescript-language-server" "--stdio")))
  )

;; Caveats about lsp for javascript:
;;
;; - Not sure if it is respecting jsconfig.json or not.
;;
;; - In Neodarwin, got (lsp-timed-out-error), probably because the
;;   repo is so big. So, Neodarwin is on the lsp blacklist.
;;
;; - TODO: defer starting lsp javascript server until nvm is figured
;;   out.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (less-css-mode . lsp-deferred)
  :hook (sh-mode . lsp-deferred)
  :hook (html-mode . lsp-deferred)
  :hook (dockerfile-mode . lsp-deferred)
  :hook (yaml-mode . lsp-deferred)
  :hook (web-mode . lsp-deferred)
  :hook (json-mode . lsp-deferred)
  ;; :hook (js-mode . lsp-deferred)
  ;; :hook (js2-mode . lsp-deferred)
  ;; :hook (typescript-mode . lsp-deferred)
  ;; :hook (typescript-ts-mode . lsp-deferred)
  ;; :hook (python-mode. lsp-deferred)
  :init
  (setq lsp-prefer-flymake nil
        lsp-keymap-prefix "M-l")
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda ()
                                       `(,(or (executable-find (cl-first lsp-yaml-server-command))
                                              (lsp-package-path 'yaml-language-server))
                                         ,@(cl-rest lsp-yaml-server-command))))
                    :major-modes '(openapi-yaml-mode yaml-mode docker-compose-mode)
                    :priority 0
                    :server-id 'yamlls
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration
                                         (lsp-configuration-section "yaml"))))
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-package-ensure 'yaml-language-server
                                                              callback error-callback))))
  (setq lsp-auto-guess-root t
        lsp-eldoc-enable-hover nil
        lsp-response-timeout 5
        lsp-completion-provider :capf
        lsp-project-blacklist '("neodarwin" "neodarwin-worktree")))

;;   (lsp-headerline-breadcrumb-mode . nil)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        ;;       lsp-ui-flycheck-enable nil
        ;;       lsp-ui-peek-enable nil
        ;;       lsp-ui-sideline-enable nil
        ;;       lsp-ui-sideline-show-flycheck nil
        ;;       lsp-ui-doc-enable nil
        ;;       lsp-ui-imenu-enable nil
        ;;       lsp-ui-sideline-ignore-duplicate t
        )
  ;; (require 'lsp-ui-flycheck)

  ;; once lsp-ui is registered as a checker, somehow it seems to stop
  ;; other checkers from running. may be related to
  ;; https://github.com/emacs-lsp/lsp-ui/issues/190 but when I checked
  ;; the value of flycheck-checker, it was nil. So I don't understand
  ;; how it is stopping other checkers. Here is how to disable lsp-ui:

  ;; (setq flycheck-disabled-checkers (append '(lsp-ui) flycheck-disabled-checkers))

  ;; Until it works better, instead of using lsp-ui always...
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  ;; just use it for a whitelist of modes.
  ;; (dolist (hook '(sh-mode-hook))
  ;;   (add-hook hook 'lsp-ui-mode))
  )

;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package company-lsp
  :disabled
  :after lsp-mode
  :commands company-lsp)
