;; Magit.
(when (require 'magit nil t)
  ;; change magit diff colors
  (eval-after-load 'magit
    '(progn
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-foreground 'magit-diff-del "red3")
       (set-face-background 'magit-diff-add "black")
       (set-face-background 'magit-diff-del "black")
       (set-face-background 'magit-item-highlight "#121616")
       (unless (display-graphic-p)
         (set-face-background 'magit-item-highlight "black"))))

  (autoload 'magit-blame "magit-blame-mode" "Minor mode for blaming." t))

(provide 'setup-magit)
