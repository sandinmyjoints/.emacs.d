
;; fci

;; Fill column indicator.
;; See: https://github.com/alpaker/Fill-Column-Indicator
;;
;; Disabled because too flaky, too many problems with various modes.
;;
(use-package fci-mode
  :disabled
  :config
  (require 'setup-fci)
  (setq fci-rule-color "#555")

  ;; Turn on fci for these modes:
  (dolist (hook '(prog-mode-hook yaml-mode-hook))
    (add-hook hook 'fci-mode))

  ;; ...except for these modes.
  (defun turn-off-fci ()
    (fci-mode -1))

  (dolist (hook '(web-mode-hook))
    (add-hook hook 'turn-off-fci))

  ;; fci-mode doesn't play well with flycheck inlines
  (defun turn-off-fci-before-inlines (errors)
    (when (bound-and-true-p fci-mode)
      (set (make-local-variable 'wjb/fci-mode-was-on) t)
      (turn-off-fci-mode)))

  (defun restore-fci-after-inlines ()
    (when (bound-and-true-p wjb/fci-mode-was-on)
      (turn-on-fci-mode)
      (setq wjb/fci-mode-was-on nil)))

  (advice-add 'flycheck-inline-display-errors
              :before #'turn-off-fci-before-inlines)

  (advice-add 'flycheck-inline-hide-errors
              :after #'restore-fci-after-inlines)

  ;; (advice-remove 'flycheck-inline-display-errors #'turn-off-fci-before-inlines)
  ;; (advice-remove 'flycheck-inline-hide-errors #'restore-fci-after-inlines)


  ;; fci-mode doesn't play well with popups
  (defun on-off-fci-before-company (command)
    (when (and (bound-and-true-p fci-mode) (string= "show" command))
      (set (make-local-variable 'wjb/fci-mode-was-on) t)
      (turn-off-fci-mode))
    (when (and (bound-and-true-p wjb/fci-mode-was-on) (string= "hide" command))
      (turn-on-fci-mode)))

  (advice-add 'company-call-frontends
              :before #'on-off-fci-before-company))

;; From https://github.com/purcell/emacs.d/blob/190528091fd8f72e2fa5bbf89f8492c29f31db78/lisp/init-fci.el
;; Fill column indicator
(when (eval-when-compile (> emacs-major-version 23))
  (defun sanityinc/fci-enabled-p ()
    (bound-and-true-p fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (setq sanityinc/fci-mode-suppressed fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sanityinc/fci-enabled-p)
          (turn-on-fci-mode))))))

(provide 'setup-fci)
