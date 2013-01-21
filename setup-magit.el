;; Magit.
(require 'magit)
;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-item-highlight "#141413")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

(autoload 'magit-blame "magit-blame-mode" "Minor mode for blaming." t)

(provide 'setup-magit)
