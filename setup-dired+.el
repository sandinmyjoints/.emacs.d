;; Diredplus.
(when (require 'dired+ nil t)
  (toggle-diredp-find-file-reuse-dir 1))

(setq dired-listing-switches "-lah")

(provide 'setup-dired+)
