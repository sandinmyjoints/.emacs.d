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

  (add-to-list 'ido-ignore-buffers "*Ibuffer*")

  ;; TODO: extensions order, ignore
  ;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

  ;; Configure flx to work with ido.
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq flx-ido-threshold 8192)

  ;; per https://github.com/lewang/flx
  (setq gc-cons-threshold 20000000)

  )

(provide 'setup-ido)
