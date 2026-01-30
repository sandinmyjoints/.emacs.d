;;; setup-eglot.el --- Eglot setup for TS / TSX / jtsx -*- lexical-binding: t; -*-
;;
;; Integrates Eglot (LSP client) with:
;;   - typescript-ts-mode  (.ts)
;;   - tsx-ts-mode         (.tsx)
;;   - jtsx-jsx-mode       (alt TS)
;;   - jtsx-tsx-mode       (alt TSX)
;;
;; Coexists with existing tide setup: in any buffer where Eglot starts,
;; tide-mode is disabled to avoid duplicate IDE features.
;;
;;; Code:

(use-package eglot
  :commands (eglot eglot-ensure)
  :init
  ;; Register (or extend) server mapping for TypeScript family.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typescript-ts-mode tsx-ts-mode jtsx-jsx-mode jtsx-tsx-mode)
                   . ("typescript-language-server" "--stdio"))))

  (defun wjb/eglot-setup-company ()
    "Set company backends suitable for Eglot (drop tide, prefer capf)."
    (when (boundp 'company-backends)
      (setq-local company-backends
                  '((company-capf company-keywords company-dabbrev-code company-dabbrev)))))

  (defun wjb/eglot-maybe-start ()
    "Start Eglot for TS/TSX/jtsx buffers unless tide-mode already active.
Ensures correct Node version via nvm before launching the server."
    (when (and (not (bound-and-true-p tide-mode))
               (derived-mode-p 'typescript-ts-mode 'tsx-ts-mode 'jtsx-jsx-mode 'jtsx-tsx-mode))
      (when (fboundp 'nvm-use-for-buffer)
        (ignore-errors (nvm-use-for-buffer)))
      (eglot-ensure)))

  :hook ((typescript-ts-mode . wjb/eglot-maybe-start)
         (tsx-ts-mode        . wjb/eglot-maybe-start)
         (jtsx-jsx-mode      . wjb/eglot-maybe-start)
         (jtsx-tsx-mode      . wjb/eglot-maybe-start)
         (js2-mode           . wjb/eglot-maybe-start) ;; note: eglot is not useful unless typescript is installed in project
         ;; After Eglot attaches, finalize per-buffer adjustments.
         (eglot-managed-mode . (lambda ()
                                 (when (derived-mode-p 'typescript-ts-mode 'tsx-ts-mode 'jtsx-jsx-modesx-tsx-mode)
                                   ;; Turn off tide if it was auto-enabled elsewhere.
                                   (when (bound-and-true-p tide-mode)
                                     (tide-mode -1))
                                   (wjb/eglot-setup-company)))))

  :config
  ;; Keep Flycheck; tell Eglot not to touch Flymake (it enables Flymake by default).
  (use-package flycheck-eglot
    :after (flycheck eglot)
    :custom (flycheck-eglot-exclusive nil)
    :config
    (global-flycheck-eglot-mode 1))

  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

  (setq eglot-confirm-server-edits nil
        eglot-connect-timeout 60
        eglot-autoshutdown t)

  ;; Optional: silence verbose events buffer unless debugging.
  ;; (setq eglot-events-buffer-size 0)
  )

(use-package eglot-java
  :after eglot
  :config
  (add-hook 'java-mode-hook 'eglot-java-mode)
  (add-hook 'java-ts-mode-hook 'eglot-java-mode)
  )

(provide 'setup-eglot)
;;; setup-eglot.el ends here
