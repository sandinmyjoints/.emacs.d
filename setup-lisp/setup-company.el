(when (require 'company nil t)
  (add-hook 'js2-mode-hook (lambda () (company-mode)))

  (add-to-list 'company-backends 'company-restclient)
  (when (require 'company-emoji nil t)
    (add-to-list 'company-backends 'company-emoji)))

(provide 'setup-company)
