;; Ido.
(when (require 'ido nil t)
  (ido-mode t)

  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        confirm-nonexistent-file-or-buffer nil
        ido-auto-merge-work-directories-length -1)

  (ido-everywhere t)

  ;; Really use ido everywhere.
  (when (require 'ido-ubiquitous nil t)
    (ido-ubiquitous-mode 1))

  (add-to-list 'ido-ignore-directories "node_modules")

  ;; TODO: extensions order, ignore
  ;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  )

(provide 'setup-ido)
