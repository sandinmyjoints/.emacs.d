(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (add-hook 'js2-mode-hook (lambda () (company-mode)))

  (define-key company-mode-map (kbd "M-/") 'company-complete)

  ;; TODO:
  ;; - group backends
  ;; - order backends in a way that makes sense
  ;; - set backends based on major mode
  ;;
  ;; good summary of how backends work: https://superuser.com/a/528407/93702
  (when (require 'company-emoji nil t)
    (push 'company-emoji company-backends))

  (when (require 'company-lsp nil t)
    (push 'company-lsp company-backends))

  (when (require 'company-restclient nil t)
    (push 'company-restclient company-backends))

  ;; (setq company-backends '(company-lsp
  ;;                          company-restclient
  ;;                          ;; company-bbdb
  ;;                          ;; company-eclim
  ;;                          ;; company-semantic
  ;;                          ;; company-clang
  ;;                          ;; company-xcode
  ;;                          ;; company-cmake
  ;;                          company-capf
  ;;                          company-css
  ;;                          company-files
  ;;                          company-emoji
  ;;                          (company-dabbrev-code company-gtags company-etags company-keywords)
  ;;                          company-yasnippet
  ;;                          ;; company-oddmuse
  ;;                          company-dabbrev))

  )

(provide 'setup-company)
