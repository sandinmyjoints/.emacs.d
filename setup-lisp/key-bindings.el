;; Available to use:
;; * C-z
;; * C-,
;; * C-.
;; * C-'
;; * C-x C-c
;; * C-z
;; * C-x C-|
;; * C-c C-f
;;
;;; Code:

;; Custom prefix-map:
(define-prefix-command 'wjb-map)
(global-set-key (kbd "C-x C-c") 'wjb-map)
(define-key wjb-map (kbd ",") #'wjb/switch-to-last-compilation-buffer)
(define-key wjb-map (kbd ".") #'wjb/switch-to-last-grep-buffer)
(define-key wjb-map (kbd "d") #'wjb/switch-to-dirtree)
(define-key wjb-map (kbd "t") #'projectile-test-project)
(define-key wjb-map (kbd "c") #'projectile-compile-project)
(define-key wjb-map (kbd "s") (lambda ()
                                (interactive)
                                (pop-to-buffer "sd-standup.md")))

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

(global-set-key (kbd "H-f") #'forward-word)
(global-set-key (kbd "H-b") #'backward-word)

(global-set-key (kbd "M-f") #'wjb/forward-symbol)
(global-set-key (kbd "M-b") #'wjb/backward-symbol)

;; C-g runs whatever command it is bound to, and now H-g runs a keyboard macro
;; that consists of C-g, so when I hit it, I get the message related to quitting
;; after using a keyboard macro.
(global-set-key (kbd "H-g") (kbd "C-g"))
(global-set-key (kbd "H-a") 'company-complete)

(global-set-key (kbd "C-<return>") 'goto-address-at-point)

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

(global-set-key (kbd "C-c i") 'indent-relative)

(global-set-key (kbd "C-c d") 'dirtree)

(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c C-SPC") 'just-one-space)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c h") 'whack-whitespace)

(global-set-key (kbd "C-!") 'shell-command-on-buffer)

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "C-c C-b") 'browse-at-remote)

(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c C-j") 'join-line)

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.

(global-set-key (kbd "C-0") 'multi-occur-in-this-mode)
(global-set-key (kbd "C-c 0") 'multi-occur-in-mode-string)
;; Example: \\*.org
(global-set-key (kbd "C-c C-0") 'multi-occur-in-matching-buffers)

;; (global-set-key (kbd "H-t") 'toggle-boolean) ;; using H-t for Tern
(global-set-key (kbd "C-c ! !") 'toggle-boolean) ;; flycheck uses C-c !
(global-set-key (kbd "C-c ! t") 'toggle-only) ;; flycheck uses C-c !

;;(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;(global-set-key (kbd "C-x f") 'find-file-in-project)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(global-set-key (kbd "C-c C-v") 'wjb-toggle-invert-in-buffer)

;(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(global-set-key (kbd "C-x w") 'prepare-for-email)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "H-x n e") 'next-error)
(global-set-key (kbd "H-x p e") 'previous-error)

(global-set-key (kbd "M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "M-]") 'switch-to-next-buffer)

(global-set-key (kbd "M-;") #'comment-dwim-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement
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

(global-set-key (kbd "C-'") 'helm-mark-all)
(global-set-key (kbd "C-\"") 'helm-ff-run-marked-files-in-dired)

(global-set-key (kbd "C-x C-d") #'wjb/insert-date)
(global-set-key (kbd "C-c ,") #'wjb/switch-to-last-compilation-buffer)
(global-set-key (kbd "C-c .") #'wjb/switch-to-last-grep-buffer)
(global-set-key (kbd "<f5>") #'compile)
(global-set-key [f6] #'recompile)

(provide 'key-bindings)
