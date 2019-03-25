;; Magit.

;;; Code:

;;(when (require 'magit nil t))

;; Old magit diff colors.
;; '(progn
;;    ;; Use list-faces-display to examine all faces.
;;    (set-face-foreground 'magit-diff-add "green3") ;; Normal by default.
;;    (set-face-foreground 'magit-diff-del "red3") ;; Normal by default.
;;    (set-face-background 'magit-diff-add "black") ;; Dull green by default.
;;    (set-face-background 'magit-diff-del "black") ;; Dull red by default.
;;    (set-face-background 'magit-item-highlight "#101212")
;;    (set-face-background 'magit-diff-none "black") ;; Black by default.
;;    (unless (display-graphic-p)
;;      (set-face-background 'magit-item-highlight "black"))

;;    (set-face-attribute 'magit-item-highlight nil
;;                        :weight 'normal))

(set-face-bold 'magit-diff-file-heading nil)
(set-face-background 'magit-diff-context-highlight "grey10")
(set-face-background 'magit-diff-context "grey10")

(setq magit-last-seen-setup-instructions "1.4.0"
      magit-diff-auto-show '(stage-all log-oneline log-follow log-select blame-follow)
      magit-status-expand-stashes nil
      magit-commit-show-diff nil
      magit-revert-buffers 1 ;; important for not slowing down everything
      ;; magit-completing-read-function 'magit-ido-completing-read
      magit-completing-read-function 'ivy-completing-read
      magit-push-always-verify nil
      magit-revision-insert-related-refs nil
      magit-branch-read-upstream-first nil)

;; TODO: ideally, push-remote would be the remote tracking branch, not
;; master -- this can probably be configred in magit somehow In other
;; words, when I create a new local branch that tracks a remote
;; branch, currently rebase is set to master (good) and push is set to
;; the remote branch (good) but in the magit status buffer I see
;; "Unpulled from master (3)" (bad) -- I would only want to see
;; Unpulled from the remote branch.
(setq magit-section-initial-visibility-alist
      '(
        (unpulled . show)
        (staged . show)
        (unstaged . show)
        (recent . hide)
        (untracked . hide)
        (unpushed . hide)
        (stashes . hide)))

(add-hook 'magit-status-sections-hook 'magit-insert-worktrees)

;; this means it will only be shown/hidden by tab, not by killing and reopening
;; the magit-status buffer.
(setq magit-section-cache-visibility '(stashes untracked))

(define-key magit-status-mode-map (kbd "M-u") 'magit-section-up)

;; magit-gh-pulls
;; This was useful: https://github.com/sigma/magit-gh-pulls/issues/5
;; Tokens are stored in ~/.gitconfig.
;;(require 'magit-gh-pulls)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; From http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'endless/visit-pull-request-url))

;; https://magit.vc/manual/magit/Performance.html
(remove-hook 'server-switch-hook 'magit-commit-diff)
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; https://magit.vc/manual/magit/Diff-options.html
;; git diff --color-words="[^[:space:]]|([[:alnum:]]|UTF_8_GUARD)+"
(setq magit-diff-refine-hunk t)

(provide 'setup-magit)
