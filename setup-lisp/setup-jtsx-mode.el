;;; setup-jtsx-mode.el --- jtsx-mode setup for TS / TSX  -*- lexical-binding: t; -*-
;;
;; Configures jtsx for .ts and .tsx files similar to existing ts/tsx setup.
;;

(use-package jtsx
  :ensure t
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
         ;; ("\\.ts\\'" . jtsx-typescript-mode))
  :init
  (defun wjb/jtsx-common-setup ()
    "Common setup for jtsx-jsx-mode and jtsx-tsx-mode."

    (define-key (current-local-map) (kbd "C-c C-y") #'wjb-toggle-it-only-js)

    (setq-local company-backends wjb/company-backends-ts)

    (when (boundp 'preferred-javascript-indent-level)
      (setq-local tab-width preferred-javascript-indent-level)
      (when (boundp 'typescript-indent-level)
        (setq-local typescript-indent-level preferred-javascript-indent-level))
      (when (boundp 'jtsx-indent-offset)
        (setq-local jtsx-indent-offset preferred-javascript-indent-level)))
    ;; Disable tabs explicitly (mirrors typical TS config).
    (setq-local indent-tabs-mode nil))
  :hook ((jtsx-jsx-mode . wjb/jtsx-common-setup)
         (jtsx-tsx-mode . wjb/jtsx-common-setup))
  :config
  (when (fboundp 'nvm-use-for-buffer)
    (add-hook 'jtsx-jsx-mode-hook #'nvm-use-for-buffer -99)
    (add-hook 'jtsx-tsx-mode-hook #'nvm-use-for-buffer -99))

  (with-eval-after-load 'prettier-js
    (add-hook 'jtsx-jsx-mode-hook #'prettier-js-mode)
    (add-hook 'jtsx-tsx-mode-hook #'prettier-js-mode))

  ;; Flycheck: reuse existing TS checkers if desired.
  (with-eval-after-load 'flycheck
    (dolist (m '(jtsx-jsx-mode jtsx-tsx-mode))
      (flycheck-add-mode 'javascript-eslint m)))

  ;; (add-hook 'jtsx-jsx-mode-hook #'eldoc-mode)
  ;; (add-hook 'jtsx-tsx-mode-hook #'eldoc-mode)

  (setq jtsx-enable-jsx-element-tags-auto-sync t)
  )

(provide 'setup-jtsx-mode)
;;; setup-jtsx-mode.el ends here
