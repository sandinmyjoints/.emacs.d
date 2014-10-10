;; Org-mode setup.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)
(setq org-clock-persist 'history)
;(org-clock-persistence-insinuate)
(setq org-blank-before-new-entry
      '((heading . nil) (plain-list-item . nil))) ;; can switch back to auto soon


(setq org-todo-keywords
      '((sequence "TODO" "ACTIVE" "|" "DONE" "INACTIVE")))

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(add-hook 'org-mode-hook
	  (lambda ()
        (auto-fill-mode 1)
        (set-fill-column 80)
        (fci-mode -1)
        (local-set-key (kbd "<S-up>") 'outline-previous-visible-heading)
        (local-set-key (kbd "<S-down>") 'outline-next-visible-heading)))

(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)
(setq org-return-follows-link t)

(provide 'setup-org)
