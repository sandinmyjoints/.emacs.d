(setq-default js2-indent-level 2)
(make-variable-buffer-local 'js2-indent-level)

;(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-auto-indent-p nil)
(setq-default js2-enter-indents-newline nil)
;(setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
;(setq-default js2-idle-timer-delay 0.1)
;(setq-default js2-indent-on-enter-key nil)
;(setq-default js2-mirror-mode nil)
;(setq-default js2-strict-inconsistent-return-warning nil)
;(setq-default js2-rebind-eol-bol-keys nil)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
;(setq-default js2-concat-multiline-strings 'eol)

;; Don't redefine C-a for me please, js2-mode
;(define-key js2-mode-map (kbd "C-a") nil)

;; Use lambda for anonymous functions
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u2190")
                        nil)))))

(provide 'setup-js2-mode)
