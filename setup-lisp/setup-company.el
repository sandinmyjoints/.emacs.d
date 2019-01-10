(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (add-hook 'js2-mode-hook (lambda () (company-mode)))

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)

  ;; TODO:
  ;; - group backends
  ;; - order backends in a way that makes sense
  ;; - set backends based on major mode
  ;;
  ;; good summary of how backends work: https://superuser.com/a/528407/93702

  ;; remove backends I'm not going to use
  (dolist (backend '(company-semantic
                     company-bbdb
                     company-eclim
                     company-clang
                     company-xcode
                     company-oddmuse
                     ))
    (delq backend company-backends))

  ;; add some backends I'll use
  ;; push puts an element at the front of a list.
  (when (require 'company-emoji nil t)
    (push 'company-emoji company-backends))

  (when (require 'company-lsp nil t)
    (push 'company-lsp company-backends))

  (when (require 'company-restclient nil t)
    (push 'company-restclient company-backends))

  (when (require 'company-nginx nil t)
    (push 'company-nginx company-backends))

  (when (require 'company-shell nil t)
    (push 'company-shell company-backends))

  (when (require 'company-web nil t)
    (push 'company-web company-backends))

  (setq company-backends (append company-backends '(company-yasnippet)))
  ;; (setq company-backends (delq 'company-yasnippet company-backends))

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
