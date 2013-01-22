;; Available to use:
;; * C-z
;; * C-,
;; * C-.
;; * C-'
;; * C-c 0

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c s") 'ansi-term)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c b") 'rename-buffer)
(global-set-key (kbd "C-c p") 'python-mode)
(global-set-key (kbd "C-c v") 'describe-variable)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c i") 'indent-relative)
(global-set-key (kbd "C-c d") 'dirtree)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c h") 'whack-whitespace)
(global-set-key (kbd "C-c e") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 're-builder)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-x i") 'find-in-project)  ; Clobbers insert-file.
(global-set-key (kbd "C-c j") 'journal)
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x j") 'magit-status)
(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.
(global-set-key (kbd "C-0") 'idomenu)
(global-set-key (kbd "C-c 0") 'idomenu)
(global-set-key (kbd "C-c C-0") 'idomenu)
(global-set-key (kbd "C-x f") 'recentf-open-files)
;(global-set-key (kbd "C-9") 'mine-goto-symbol-at-point) ; Reserved for mine-goto-symbol-at-point


(provide 'key-bindings)
