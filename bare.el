;;; Bare-bones Emacs init.el.
;;; Works with Emacs >=23.3.3.

;;; Tramp syntax:
;;; C-x C-f /sudo:root@localhost:/etc/hosts
;;; C-x C-f /sudo::/etc/hosts
(setq tramp-default-method "ssh")

;;; No splash screen, thanks.
(setq inhibit-splash-screen t)

;; Use server.
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Prevent extraneous tabs.
(setq-default indent-tabs-mode nil)

;;; Fewer pop-up windows.
(setq pop-up-windows nil)

;;; Default for new buffers.
(setq-default major-mode 'text-mode)

;;; Avoid backslash madness.
(setq reb-re-syntax 'string)

;;; Set up 4-space tabs.
;;; See: http://stackoverflow.com/a/1819405/599258
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;; Sane backup files.
;;; See: http://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   `(("." . ,(expand-file-name
              (concat user-emacs-directory "backups"))))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;; Keep auto-save files out of the filesystem.
;;; See: http://emacswiki.org/emacs/AutoSave
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Visible bell.
(setq-default visible-bell t)

;;; Set PAGER and EDITOR so git doesn't complain: "terminal is not
;;; fully functional".
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(put 'downcase-region 'disabled nil)

;;; Keyboard for Macs.
(set-keyboard-coding-system nil)
;(setq-default mac-option-key-is-meta nil)
(setq-default mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(global-font-lock-mode t)

;;; Make grep-find more helpful.
(setq find-args "! -name \"*~\" ! -name \"#*#\" -type f -print0 | xargs -0 grep -E -C 5 -niH -e "
      default-find-cmd (concat "find " ". " find-args))
(grep-compute-defaults)
(grep-apply-setting 'grep-find-command default-find-cmd)

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "'%s' is dedicated"
     "'%s' is normal")
   (current-buffer)))

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

;;; Fix junk characters in shell-mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;;; Don't want to see garbage in term-mode.
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;;; Text and fill modes.
(defun textful-settings ()
        (goto-address-mode 1)
        (auto-fill-mode 1)
        (set-fill-column 80))
(add-hook 'rst-mode-hook 'textful-settings)
(add-hook 'text-mode-hook 'textful-settings)

;;; Default fill column.
(set-fill-column 80)

;;; ========================================
;;; Key bindings.
;;; ========================================

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c s") 'ansi-term)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c b") 'rename-buffer)
(global-set-key (kbd "C-c v") 'describe-variable)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c i") 'indent-relative)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c h") 'whack-whitespace)
(global-set-key (kbd "C-c e") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 're-builder)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-c 0") 'kill-ring-save)

;; TODO: Add anything that needs Emacs>=24.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (electric-pair-mode t)
  )
