;; Magit.

;;; Code:
(when (require 'magit nil t)
  ;; change magit diff colors
  (eval-after-load 'magit
    '(progn
       ;; Use list-faces-display to examine all faces.
       (set-face-foreground 'magit-diff-add "green3") ;; Normal by default.
       (set-face-foreground 'magit-diff-del "red3") ;; Normal by default.
       (set-face-background 'magit-diff-add "black") ;; Dull green by default.
       (set-face-background 'magit-diff-del "black") ;; Dull red by default.
       (set-face-background 'magit-item-highlight "#101212")
       (set-face-background 'magit-diff-none "black") ;; Black by default.
       (unless (display-graphic-p)
         (set-face-background 'magit-item-highlight "black"))

       (set-face-attribute 'magit-item-highlight nil
                           :weight 'normal)))

  (autoload 'magit-blame "magit-blame-mode" "Minor mode for blaming." t))

(setq magit-completing-read-function 'magit-ido-completing-read)

;; magit-gh-pulls
;; This was useful: https://github.com/sigma/magit-gh-pulls/issues/5
;; Tokens are stored in ~/.gitconfig.
;;(require 'magit-gh-pulls)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(provide 'setup-magit)
