;;; Bare-bones Emacs init.el.
;;; Works with Emacs >=23.3.3.

;; Customize user interface.
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)
;; Theme.
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")

(global-font-lock-mode t)

;; Interactively do things.
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
;;; See: http://stackoverflow.com/a/1819405/599258
(setq-default indent-tabs-mode nil)

(setq-default indent-line-function 'insert-tab)
;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;;; Sane backup files.
;;; See: http://www.emacswiki.org/emacs/BackupDirectory
(make-directory "~/.emacs.d/backup/" t)
;;; Keep auto-save files out of the filesystem.
;;; See: http://emacswiki.org/emacs/AutoSave
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   `(("." . ,(expand-file-name
              (concat user-emacs-directory "backups"))))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; TODO: Add anything that needs Emacs>=24.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (electric-pair-mode t)
  ;; Scroll up without warning the first time.
  (setq scroll-error-top-bottom t))

;; Write customizations to ~/.emacs.d/custom.el instead of this file.
(setq custom-file "~/.emacs.d/custom.el")

;;; Tramp syntax:
;;; C-x C-f /sudo:root@localhost:/etc/hosts
;;; C-x C-f /sudo::/etc/hosts
(setq tramp-default-method "ssh")

;;; Fewer pop-up windows.
(setq pop-up-windows nil)

;;; Avoid backslash madness.
(setq reb-re-syntax 'string)

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

;; Allow the very useful set-goal-column.
(put 'set-goal-column 'disabled nil)

;; Make grep-find more helpful.
;;
(setq find-args "! -name \"*~\" ! -name \"#*#\" ! -wholename \"*node_modules*\" ! -wholename \"*.git*\" -type f -print0 | xargs -0 grep -E -C 5 -niH -e " default-find-cmd (concat "find " ". " find-args))
(grep-compute-defaults)
(grep-apply-setting 'grep-find-command default-find-cmd)

(defun margin-x ()
  "Give current window a left margin of x columns."
  (interactive)
  (set-window-margins
   (get-buffer-window (current-buffer)) 12 0))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

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

;;; Default fill column.
(set-fill-column 80)

;;; ========================================
;;; Key bindings.
;;; ========================================

(global-set-key (kbd "C-x C-c") nil)
(global-unset-key (kbd "C-z")) ;; Don't suspend that easily.
(global-set-key (kbd "C-|") 'align-regexp)
;; TODO: bind C-M-= (aka C-+) to align-regexp with regexp of =
;; TODO: bind something to align-regexp with a regexp that aligns based on :
(global-set-key (kbd "C-x l") 'other-window-reverse)
(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c s") 'ansi-term)
(global-set-key (kbd "C-c q") 'query-replace-regexp)
(global-set-key (kbd "C-c b") 'rename-buffer)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c h") 'whack-whitespace)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-x C-f") 'find-file)

(defalias 'exit-emacs 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-\\") 'save-buffers-kill-terminal)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(set-language-environment "UTF-8")

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Use server.
(require 'server)
(unless (server-running-p)
  (server-start))
