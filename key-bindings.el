;; Available to use:
;; * C-z
;; * C-,
;; * C-.
;; * C-'
;; * C-c 0
;; * C-x C-c
;; * C-z
;;
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-z")) ;; Don't suspend that easily.
(global-set-key (kbd "C-|") 'align-regexp)
;; TODO: bind C-M-= (aka C-+) to align-regexp with regexp of =
;; TODO: bind something to align-regexp with a regexp that aligns based on :
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x l") 'other-window-reverse)
(global-set-key (kbd "C-x C-l") 'other-window-reverse) ; Clobbers downcase-region. Too easy to hit accidentally.
;(global-set-key (kbd "C-x C-o") 'other-window) ; Clobbers delete-blank-lines.
(global-set-key (kbd "C-c p") 'bury-buffer)
(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c s") 'ansi-term)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c b") 'rename-buffer)
(global-set-key (kbd "C-c v") 'describe-variable)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c i") 'indent-relative)
(global-set-key (kbd "C-c d") 'dirtree)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c C-SPC") 'just-one-space)
(global-set-key (kbd "C-c h") 'whack-whitespace)
(global-set-key (kbd "C-c !") 'shell-command-on-buffer)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 're-builder)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-x i") 'find-in-project)  ; Clobbers insert-file.
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x j") 'magit-status)
(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.
(global-set-key (kbd "C-0") 'idomenu)
(global-set-key (kbd "C-c 0") 'idomenu)
(global-set-key (kbd "C-c C-0") 'idomenu)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "M-]") 'switch-to-next-buffer)
(global-set-key (kbd "<M-up>") 'scroll-down)
(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "ESC <up>") 'scroll-down)
(global-set-key (kbd "ESC <down>") 'scroll-up)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
;(global-set-key (kbd "C-9") 'mine-goto-symbol-at-point) ; Reserved for mine-goto-symbol-at-point
(global-set-key (kbd "C-c C-v") 'wjb-toggle-invert-in-buffer)
(global-set-key (kbd "C-c C-y") 'wjb-toggle-it-only)


;; Smex.
(when (fboundp 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(provide 'key-bindings)
