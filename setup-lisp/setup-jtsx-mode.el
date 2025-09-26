;;; setup-jtsx-mode.el --- jtsx-mode setup for TS / TSX  -*- lexical-binding: t; -*-
;;
;; Configures jtsx-mode for .ts and .tsx files similar to existing ts/tsx setup.
;;

(use-package jtsx
  :ensure t
  :mode (("\\.ts\\'"  . jtsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :init
  (defun wjb/jtsx-common-setup ()
    "Common setup for jtsx-mode and jtsx-tsx-mode."
    ;; Match keybinding you use in typescript-ts modes.
    (define-key (current-local-map) (kbd "C-c C-y") #'wjb-toggle-it-only-js)
    ;; Company backends like other TS buffers.
    (setq-local company-backends wjb/company-backends-ts)
    ;; Keep indentation consistent with your preferred JS/TS indent.
    (when (boundp 'preferred-javascript-indent-level)
      (setq-local tab-width preferred-javascript-indent-level)
      (when (boundp 'typescript-indent-level)
        (setq-local typescript-indent-level preferred-javascript-indent-level))
      (when (boundp 'jtsx-indent-offset)
        (setq-local jtsx-indent-offset preferred-javascript-indent-level)))
    ;; Disable tabs explicitly (mirrors typical TS config).
    (setq-local indent-tabs-mode nil))
  :hook ((jtsx-mode . wjb/jtsx-common-setup)
         (jtsx-tsx-mode . wjb/jtsx-common-setup))
  :config
  ;; If you rely on nvm + per-buffer Node version selection:
  (when (fboundp 'nvm-use-for-buffer)
    (add-hook 'jtsx-mode-hook #'nvm-use-for-buffer -99)
    (add-hook 'jtsx-tsx-mode-hook #'nvm-use-for-buffer -99))

  ;; Match your existing prettier setup (you enabled it for js-base & typescript-ts-base).
  (with-eval-after-load 'prettier-js
    (add-hook 'jtsx-mode-hook #'prettier-js-mode)
    (add-hook 'jtsx-tsx-mode-hook #'prettier-js-mode))

  ;; Flycheck: reuse existing TS checkers if desired.
  (with-eval-after-load 'flycheck
    (dolist (m '(jtsx-mode jtsx-tsx-mode))
      (flycheck-add-mode 'javascript-eslint m)))

  ;; Eldoc / xref parity with other TS modes (optional — only if you use tide/lsp elsewhere).
  ;; (add-hook 'jtsx-mode-hook #'eldoc-mode)
  ;; (add-hook 'jtsx-tsx-mode-hook #'eldoc-mode)
  )

(provide 'setup-jtsx-mode)
;;; setup-jtsx-mode.el ends here
