;; Magit.

;;; Code:

(set-face-bold 'magit-diff-file-heading nil)
;; (set-face-background 'magit-diff-context-highlight "grey10")
;; (set-face-background 'magit-diff-context "grey10")

;; default
;; (setq magit-status-headers-hook '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header))

;; remove tags-header
(setq magit-status-headers-hook '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header))

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

;; see worktrees
(add-hook 'magit-status-sections-hook 'magit-insert-worktrees)

;; did this for neodarwin in dir-locals
;; (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)

;; this means it will only be shown/hidden by tab, not by killing and reopening
;; the magit-status buffer.
;; Setting to the value below caused sections to be hidden/closed after I staged a hunk.
;; (setq magit-section-cache-visibility '(stashes untracked))

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
(setq vc-handled-backends (delq 'Git vc-handled-backends)
      magit-refresh-verbose nil)

(defun wjb/magit-browse-pull-request ()
  "In `magit-log-mode', open the associated pull request at point."
  (interactive)
  (let* ((remote-url
          (car
           (git-link--exec
            "remote" "get-url"
            (format "%s"
                    (magit-get-current-remote)))))
         (beg (line-beginning-position))
         (end (line-end-position))
         (region (buffer-substring-no-properties beg end)))
    (save-match-data
      (message (format "region: %s" region))
      ;; TODO: this isn't working as expected. git@github.com:spanishdict/sd-playground.git
      (message (format "remote: %s" remote-url))
      (message (format "url: %s" (concat
             (s-replace ".git" "" remote-url)
             "/pull/"
             (match-string 1 region))))
      (and (string-match "(\\#\\([0-9]+\\))$" region)
           (browse-url-default-macosx-browser
            (concat
             (s-replace ".git" "" remote-url)
             "/pull/"
             (match-string 1 region)))))))

;; https://magit.vc/manual/magit/Diff-options.html
;; git diff --color-words="[^[:space:]]|([[:alnum:]]|UTF_8_GUARD)+"
(setq magit-diff-refine-hunk t
      ;; magit-git-executable "/opt/homebrew/bin/git"
      magit-git-executable "/opt/homebrew/Cellar/git/2.45.2/bin/git")

(provide 'setup-magit)
