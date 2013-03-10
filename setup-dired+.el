;; Diredplus.
(when (require 'dired+ nil t)
  (toggle-diredp-find-file-reuse-dir 1))

(provide 'setup-dired+)
