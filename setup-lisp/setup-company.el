(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode))
  ;; (add-hook 'js2-mode-hook (lambda () (company-mode)))

  (when (require 'company-lsp nil t)
    (push 'company-lsp company-backends))

  (when (require 'company-restclient nil t)
    (push 'company-restclient company-backends))

  (when (require 'company-emoji nil t)
    (push 'company-emoji company-backends)))

(provide 'setup-company)
