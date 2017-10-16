;; Available to use:
;; * C-z
;; * C-,
;; * C-.
;; * C-'
;; * C-c 0
;; * C-x C-c
;; * C-z
;; * C-x C-|
;; * C-x j
;;
;;; Code:

;; From https://gist.github.com/cataska/b1875754128853bfb139
;; Enables doing something like this, so H-x does the same thing as C-x:
;; (defkbalias (kbd "C-x") (kbd "H-x"))
;; But in practice, it turned out I didn't really use that...
(defmacro defkbalias (old new)
  `(define-key (current-global-map) ,new
     (lookup-key (current-global-map) ,old)))

(global-unset-key (kbd "C-x ."))  ;; unset set-fill-prefix

;; Used by Alfred.
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-g g"))
(global-unset-key (kbd "M-g M-g"))

;; js2-refactor uses either H-c, H-r, or C-c C-r.

(global-set-key (kbd "H-x H-e") 'eval-print-last-sexp)

(global-set-key (kbd "H-_") 'undo)
(global-set-key (kbd "H-k") 'kill-sexp)
(global-set-key (kbd "H-s") 'save-buffer)
;; C-g runs whatever command it is bound to, and now H-g runs a keyboard macro
;; that consists of C-g, so when I hit it, I get the message related to quitting
;; after using a keyboard macro.
(global-set-key (kbd "H-g") (kbd "C-g"))

;; Only works in Emacs >=25.1. Default for this is C-x C-;.
(global-set-key (kbd "C-;") #'comment-line)

(global-set-key (kbd "C-x 7") #'describe-char)

(global-set-key [H-up] 'beginning-of-defun)
(global-set-key (kbd "H-1") 'beginning-of-defun)
(global-set-key [H-down] 'end-of-defun)
(global-set-key (kbd "H-2") 'end-of-defun)

(global-set-key (kbd "C-x C-c") nil)
(global-unset-key (kbd "C-z")) ;; Don't suspend that easily.

(global-set-key (kbd "C-|") 'align-regexp)
(global-set-key (kbd "C-:") 'align-on-colon)
(global-set-key (kbd "C-+") 'align-on-equal)

(global-set-key (kbd "C-c f") 'find-file-at-point)

;; projectile-mode keymap
;; C-c p b 'projectile-switch-to-buffer
;; C-x b
;;
;; C-c p g 'projectile-find-file-dwim
;; C-c f
;;
;; C-c p f 'projectile-find-file
;;
;; C-c p k 'projectile-kill-buffer

(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-\\") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x l") 'ace-window)
(global-set-key (kbd "C-x C-l") 'other-window-reverse) ; Clobbers downcase-region. Too easy to hit accidentally.
;(global-set-key (kbd "C-x C-o") 'other-window) ; Clobbers delete-blank-lines.
(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c s") 'ansi-term)
;; (global-set-key (kbd "C-c r") 'query-replace-regexp)
;; (global-set-key (kbd "C-c C-r") 're-builder)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c b") 'rename-buffer)
(global-set-key (kbd "C-c v") 'describe-variable)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c i") 'indent-relative)
(global-set-key (kbd "C-c d") 'dirtree)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c C-SPC") 'just-one-space)
(global-set-key (kbd "C-c h") 'whack-whitespace)
(global-set-key (kbd "C-!") 'shell-command-on-buffer)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c C-b") 'browse-at-remote)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-x i") 'find-in-project)  ; Clobbers insert-file.
(global-set-key (kbd "C-x 9") 'rgrep)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.
(global-set-key (kbd "C-0") 'idomenu)
(global-set-key (kbd "C-c 0") 'idomenu)
(global-set-key (kbd "C-c C-0") 'idomenu)
(global-set-key (kbd "H-t") 'toggle-boolean)
(global-set-key (kbd "C-c ! !") 'toggle-boolean) ;; flycheck uses C-c !
;(global-set-key (kbd "C-x f") 'recentf-open-files)
;(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
;(global-set-key (kbd "C-9") 'mine-goto-symbol-at-point) ; Reserved for mine-goto-symbol-at-point
(global-set-key (kbd "C-c C-v") 'wjb-toggle-invert-in-buffer)
(global-set-key (kbd "C-c C-y") 'wjb-toggle-it-only)
;(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-x w") 'prepare-for-email)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "H-x n e") 'next-error)
(global-set-key (kbd "H-x p e") 'previous-error)

(global-set-key (kbd "M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "M-]") 'switch-to-next-buffer)
(global-set-key (kbd "M-{") 'windmove-left) ;; from Shift-left arrow
(global-set-key (kbd "M-}") 'windmove-right) ;; from Shift-right arrow
(global-set-key (kbd "M-<up>") 'windmove-up) ;; from S-<up>
(global-set-key (kbd "M-<down>") 'windmove-down) ;; from S-<down>
;; (global-set-key (kbd "<M-up>") 'scroll-down)
;; (global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "ESC <up>") 'scroll-down)
(global-set-key (kbd "ESC <down>") 'scroll-up)

(global-set-key (kbd "S-<up>") 'move-text-up)
(global-set-key (kbd "S-<down>") 'move-text-down)

;; Smex.
(when (fboundp 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(provide 'key-bindings)
