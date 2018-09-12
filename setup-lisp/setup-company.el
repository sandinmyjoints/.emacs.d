(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (add-hook 'js2-mode-hook (lambda () (company-mode)))

  ;; TODO:
  ;; - group backends
  ;; - order backends in a way that makes sense
  (when (require 'company-emoji nil t)
    (push 'company-emoji company-backends))

  (when (require 'company-lsp nil t)
    (push 'company-lsp company-backends))

  (when (require 'company-restclient nil t)
    (push 'company-restclient company-backends))
  )

(provide 'setup-company)
