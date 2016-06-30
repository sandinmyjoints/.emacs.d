;; From http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; From http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(eval-after-load 'web-mode (lambda ()
                             (when (equal web-mode-content-type "jsx")
                               (flycheck-mode +1))))

(provide 'setup-webmode)
