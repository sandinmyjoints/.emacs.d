;; Ido.
(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      confirm-nonexistent-file-or-buffer nil)

(ido-everywhere t)

;; Really use ido everywhere.
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; TODO: extensions order, ignore
;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

(provide 'setup-ido)