;; -*- lexical-binding: t -*-

;;; init.el
;;; See: https://github.com/sandinmyjoints/.emacs.d

;; TODO:
;; * Directory local variables. See: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; * find-file-in-project. See: http://emacswiki.org/emacs/FindFileInProject
;; * Coffee-script etags.
;; * Coffee-script compile and flymake.
;; * swank.js:
;;   * https://github.com/Gozala/swank-js
;; * Model structure after/fork: https://github.com/magnars/.emacs.d
;; * Make into a full repo with submodules.
;; * Use ELPA.
;; * Stripped down version for text terminals/new machines, or infer what libraries are installed/exist, or install them automatically.
;; * Review for ideas:
;;   * http://news.ycombinator.com/item?id=1654164
;;   * https://github.com/technomancy/emacs-starter-kit
;; * Window and frame restore. See:
;; http://www.gnu.org/savannah-checkouts/gnu/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;; http://www.emacswiki.org/emacs/LayoutRestore
;; http://www.emacswiki.org/emacs/FrameConfig
;; http://www.emacswiki.org/emacs/SessionManagement
;; http://www.gentei.org/~yuuji/software/windows.el
;; http://www.gentei.org/~yuuji/software/revive.el
;; * Try out:
;;   * yasnippets
;;   * iy go to char
;; * Files to visit on startup in non-dirtree window.
;;   * E.g.: (find-file "~/emacs/gist-3743892/init.el")

;; ========================================
;; Definitions.
;; ========================================

;; Set Emacs Lisp directory.
(setq site-lisp-dir
      (expand-file-name "elisp" user-emacs-directory))

;; Directories to open in dirtree on start.
(setq initial-dirs-to-open
      '("~/scm/sd/fluensa"
        "~/scm/sd/ops"
        "~/.emacs.d"))

;; Set custom markers.
;; Args:
;; 1. Marker.
;; 2. Register to store.
;; 3. Key bindings to set/clear marker.
;; 4. Insert/remove marker from current buffer?
;;;
(setq wjb-custom-markers
      '(("NNN" ?n "" t)
        ("MMM" ?m "" t)
        ("Server" ?s "" nil)
        ("Quiz View" ?q "" nil)
        ("Client" ?c "" nil)))

(setq wjb-test-config-buffer "test.coffee")

;; Custom grep-find via find-in-project.
(setq find-in-project-default-dir ".")

(set-register ?t "TODO ")
(set-register ?h "TODO HERE: ")

;; ========================================
;; Set up load-path.
;; ========================================

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Add all subdirs of site-lisp-dir.
(let ((default-directory site-lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))

;; ========================================
;; Settings.
;; ========================================

;; Unused graphical elements.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Require common lisp.
(require 'cl)

;; Use server.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Save desktop.
;(desktop-save-mode 1)

;; Sane backup files.
;; See: http://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   `(("." . ,(expand-file-name
              (concat user-emacs-directory "backups"))))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Keep auto-save files out of the filesystem.
;; See: http://emacswiki.org/emacs/AutoSave
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set PAGER and EDITOR so git doesn't complain: "terminal is not
;; fully functional".
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

;; Allow downcasing regions.
(put 'downcase-region 'disabled nil)

;; Keyboard for Macs.
(setq-default mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;; Make grep-find more helpful.
;; TODO: dir-local list of paths to exclude from grep-find.
(setq find-args "! -name \"*~\" ! -name \"#*#\" ! -wholename \"*node_modules*\" ! -wholename \"*.git*\" -type f -print0 | xargs -0 grep -E -C 5 -niH -e "
      default-find-cmd (concat "find " ". " find-args))
(grep-compute-defaults)
(grep-apply-setting 'grep-find-command default-find-cmd)

;; TODO: ignore .git and node_modules

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-isearch-filenames (quote dwim))
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(markdown-command "/Users/william/bin/markdown")
 '(ns-command-modifier (quote meta))
 '(rst-level-face-base-light 51)
 '(safe-local-variable-values (quote ((find-in-project-dir . "~/.emacs.d") (find-in-project-dir . "~/scm/sd/fluensa/src") (find-in-project-dir . "~/scm/sd/database-platform"))))
 '(tool-bar-mode nil)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch Git))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ========================================
;; Appearance.
;; ========================================

(require 'appearance)

;; ========================================
;; Package management.
;; ========================================

(require 'setup-package)

;; Install packages if they're missing.
(defun init--install-packages ()
  (packages-install
   (cons 'exec-path-from-shell melpa)
   (cons 'git-commit-mode marmalade)
   (cons 'gitconfig-mode marmalade)
   (cons 'gitignore-mode marmalade)
   (cons 'ido-ubiquitous marmalade)
   (cons 'magit marmalade)
   (cons 'rainbow-mode melpa)
   ;(cons 'paredit melpa)
   ;(cons 'move-text melpa)
   ;(cons 'gist melpa)
   ;(cons 'htmlize melpa)
   ;(cons 'elisp-slime-nav melpa)
   ;;(cons 'elnode marmalade)
   ;(cons 'slime-js marmalade)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; ========================================
;; Sane defaults.
;; ========================================

;; From:
;; https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el
(require 'sane-defaults)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup environment variables from the user's shell.
(when is-mac (exec-path-from-shell-initialize))

;; ========================================
;; Custom defuns.
;; ========================================

(require 'defuns)

;; ========================================
;; Misc.
;; ========================================

;; Set shortcuts to clear custom markers. Requires lexical binding.
(dolist (marker-data wjb-custom-markers)
        (let ((marker (pop marker-data))
              (marker-register (pop marker-data))
              (marker-key (pop marker-data))
              (handle-in-current-buffer (pop marker-data)))
          (progn
            (set-register marker-register marker)
            (global-set-key marker-key (lambda (arg)
                                          (interactive "P")
                                          (wjb-toggle-marker arg marker handle-in-current-buffer))))))

;; Open grep results in the same frame. See:
;; http://stackoverflow.com/questions/2299133/emacs-grep-find-link-in-same-window/2299261#2299261
(eval-after-load "compile"
  '(defun compilation-goto-locus (msg mk end-mk)
     "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non-nil, mark is set there
and overlay is highlighted between MK and END-MK."
     ;; Show compilation buffer in other window, scrolled to this error.
     (let* ((from-compilation-buffer (eq (window-buffer (selected-window))
                                         (marker-buffer msg)))
            ;; Use an existing window if it is in a visible frame.
            (pre-existing (get-buffer-window (marker-buffer msg) 0))
            (w (if (and from-compilation-buffer pre-existing)
                   ;; Calling display-buffer here may end up (partly) hiding
                   ;; the error location if the two buffers are in two
                   ;; different frames.  So don't do it if it's not necessary.
                   pre-existing
                 (let ((display-buffer-reuse-frames t)
                       (pop-up-windows t))
                   ;; Pop up a window.
                   (display-buffer (marker-buffer msg)))))
            (highlight-regexp (with-current-buffer (marker-buffer msg)
                                ;; also do this while we change buffer
                                (compilation-set-window w msg)
                                compilation-highlight-regexp)))
       ;; Ideally, the window-size should be passed to `display-buffer' (via
       ;; something like special-display-buffer) so it's only used when
       ;; creating a new window.
       (unless pre-existing (compilation-set-window-height w))

       (switch-to-buffer (marker-buffer mk))

       ;; If narrowing gets in the way of going to the right place, widen.
       (unless (eq (goto-char mk) (point))
         (widen)
         (goto-char mk))
       (if end-mk
           (push-mark end-mk t)
         (if mark-active (setq mark-active)))
       ;; If hideshow got in the way of
       ;; seeing the right place, open permanently.
       (dolist (ov (overlays-at (point)))
         (when (eq 'hs (overlay-get ov 'invisible))
           (delete-overlay ov)
           (goto-char mk)))

       (when highlight-regexp
         (if (timerp next-error-highlight-timer)
             (cancel-timer next-error-highlight-timer))
         (unless compilation-highlight-overlay
           (setq compilation-highlight-overlay
                 (make-overlay (point-min) (point-min)))
           (overlay-put compilation-highlight-overlay 'face 'next-error))
         (with-current-buffer (marker-buffer mk)
           (save-excursion
             (if end-mk (goto-char end-mk) (end-of-line))
             (let ((end (point)))
               (if mk (goto-char mk) (beginning-of-line))
               (if (and (stringp highlight-regexp)
                        (re-search-forward highlight-regexp end t))
                   (progn
                     (goto-char (match-beginning 0))
                     (move-overlay compilation-highlight-overlay
                                   (match-beginning 0) (match-end 0)
                                   (current-buffer)))
                 (move-overlay compilation-highlight-overlay
                               (point) end (current-buffer)))
               (if (or (eq next-error-highlight t)
                       (numberp next-error-highlight))
                   ;; We want highlighting: delete overlay on next input.
                   (add-hook 'pre-command-hook
                             'compilation-goto-locus-delete-o)
                 ;; We don't want highlighting: delete overlay now.
                 (delete-overlay compilation-highlight-overlay))
               ;; We want highlighting for a limited time:
               ;; set up a timer to delete it.
               (when (numberp next-error-highlight)
                 (setq next-error-highlight-timer
                       (run-at-time next-error-highlight nil
                                    'compilation-goto-locus-delete-o)))))))
       (when (and (eq next-error-highlight 'fringe-arrow))
         ;; We want a fringe arrow (instead of highlighting).
         (setq next-error-overlay-arrow-position
               (copy-marker (line-beginning-position)))))))


;; ========================================
;; Some hooks.
;; ========================================

;; Force 2-space indentation in css-mode.
(add-hook 'css-mode-hook
          (function
           (lambda ()
             (setq css-indent-offset 2))))


;; ========================================
;; Require/autoload and config packages.
;; ========================================

(require 'ido)
(eval-after-load 'ido '(require 'setup-ido))

;(eval-after-load 'org '(require 'setup-org))

(require 'dired+)
(eval-after-load 'dired+ '(require 'setup-dired+))

(autoload 'magit-status "magit")
(autoload 'magit-log "magit")
(eval-after-load 'magit '(require 'setup-magit))

;(eval-after-load 'grep '(require 'setup-rgrep))
;(eval-after-load 'shell '(require 'setup-shell))
;(require 'setup-hippie)
;(require 'setup-yasnippet)
;(require 'setup-perspective)
;(require 'setup-ffip)
;(require 'setup-html-mode)
;(require 'setup-paredit)

;; Rainbow mode.
(require 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'coffee-mode-hook 'rainbow-mode)
(add-hook 'less-css-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; Anything.
;(require 'helm-config)
;(helm-mode 1)

;; Imenu.
(require 'imenu)
(autoload 'idomenu "idomenu" nil t)

(defadvice ido-imenu (before push-mark activate)
    (push-mark))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)


;; TODO: Fix this to work with lexical binding.
;; See: http://www.delorie.com/gnu/docs/emacs/cl_22.html
;; See: https://gist.github.com/2360578
;; (defun ido-goto-symbol (&optional a-symbol)
;;   "Will update the imenu index and then use ido to select a symbol to navigate to"
;;   (interactive)
;;   (imenu--make-index-alist)
;;   (let ((name-and-pos '())
;;         (mine-symbol-names '()))
;;     (flet ((addsymbols (symbol-list)
;;                        (when (listp symbol-list)
;;                          (dolist (symbol symbol-list)
;;                            (let ((name nil) (position nil))
;;                              (cond
;;                               ((and (listp symbol) (imenu--subalist-p symbol))
;;                                (addsymbols symbol))

;;                               ((listp symbol)
;;                                (setq name (car symbol))
;;                                (setq position (cdr symbol)))

;;                               ((stringp symbol)
;;                                (setq name symbol)
;;                                (setq position (get-text-property 1 'org-imenu-marker symbol))))

;;                              (unless (or (null position) (null name))
;;                                (add-to-list 'mine-symbol-names name)
;;                                (add-to-list 'name-and-pos (cons name position))))))))
;;       (addsymbols imenu--index-alist))
;;     (let* ((selected-symbol
;;             (if (null a-symbol)
;;                 (ido-completing-read "Symbol? " mine-symbol-names)
;;               a-symbol))
;;            (position (cdr (assoc selected-symbol name-and-pos))))
;;       (cond
;;        ((overlayp position)
;;         (goto-char (overlay-start position)))
;;        (t
;;         (goto-char position))))))

;; Alternative that doesn't use `flet` but doesn't seem to work, either:
;; (defun ido-goto-symbol (&optional symbol-list)
;;   "Refresh imenu and jump to a place in the buffer using Ido."
;;   (interactive)
;;   (unless (featurep 'imenu)
;;     (require 'imenu nil t))
;;   (cond
;;    ((not symbol-list)
;;     (let ((ido-mode ido-mode)
;;           (ido-enable-flex-matching
;;            (if (boundp 'ido-enable-flex-matching)
;;                ido-enable-flex-matching t))
;;           name-and-pos symbol-names position)
;;       (unless ido-mode
;;         (ido-mode 1)
;;         (setq ido-enable-flex-matching t))
;;       (while (progn
;;                (imenu--cleanup)
;;                (setq imenu--index-alist nil)
;;                (ido-goto-symbol (imenu--make-index-alist))
;;                (setq selected-symbol
;;                      (ido-completing-read "Symbol? " symbol-names))
;;                (string= (car imenu--rescan-item) selected-symbol)))
;;       (unless (and (boundp 'mark-active) mark-active)
;;         (push-mark nil t nil))
;;       (setq position (cdr (assoc selected-symbol name-and-pos)))
;;       (cond
;;        ((overlayp position)
;;         (goto-char (overlay-start position)))
;;        (t
;;         (goto-char position)))))
;;    ((listp symbol-list)
;;     (dolist (symbol symbol-list)
;;       (let (name position)
;;         (cond
;;          ((and (listp symbol) (imenu--subalist-p symbol))
;;           (ido-goto-symbol symbol))
;;          ((listp symbol)
;;           (setq name (car symbol))
;;           (setq position (cdr symbol)))
;;          ((stringp symbol)
;;           (setq name symbol)
;;           (setq position
;;                 (get-text-property 1 'org-imenu-marker symbol))))
;;         (unless (or (null position) (null name)
;;                     (string= (car imenu--rescan-item) name))
;;           (add-to-list 'symbol-names name)
;;           (add-to-list 'name-and-pos (cons name position))))))))

(defun mine-goto-symbol-at-point ()
  "Will navigate to the symbol at the current point of the cursor"
  (interactive)
  (ido-goto-symbol (thing-at-point 'symbol)))


;; Map files to modes
(require 'mode-mappings)

;; Dirtree.
(autoload 'dirtree "dirtree" "Dirtree." t)

;; Lua mode.
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Text and fill modes.
(defun textful-settings ()
        (goto-address-mode 1)
        (auto-fill-mode 1)
        (set-fill-column 80))
(add-hook 'markdown-mode-hook 'textful-settings)
(add-hook 'rst-mode-hook 'textful-settings)
(add-hook 'text-mode-hook 'textful-settings)

;; Fill column indicator.
;; See: https://github.com/alpaker/Fill-Column-Indicator
(require 'fill-column-indicator)

;; Make fci-mode global...
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; ...except for these modes.
(defun no-fci ()
  (fci-mode -1))
(add-hook 'dirtree-mode-hook 'no-fci)
(add-hook 'dired-mode-hook 'no-fci)
(add-hook 'dired+-mode-hook 'no-fci)
(add-hook 'org-mode-hook 'no-fci)

;; Org-mode.
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . auto)))

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(add-hook 'org-mode-hook
	  (lambda () (auto-fill-mode 1) (set-fill-column 80)))

;; Python
;; TODO: Fix ipython for use in emacs.
;(setq py-install-directory "~/emacs/")
;(require 'python-mode)
;(setq py-shell-name "ipython")

;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; json
(require 'json)
(require 'json-pretty-print)

;; Smart-tab. See: https://raw.github.com/genehack/smart-tab/master/smart-tab.el
(require 'smart-tab)
(global-smart-tab-mode 1)

;; multiple-cursors.
;; See: https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(defun create-cursor ()
  (interactive)
  (mc/create-fake-cursor-at-point))

;(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'set-rectangular-region-anchor)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ,") 'create-cursor)
(global-set-key (kbd "C-c C-,") 'create-cursor)
(global-set-key (kbd "C-c .") 'multiple-cursors-mode)
(global-set-key (kbd "C-c C-.") 'multiple-cursors-mode)

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; smart-forward
;; See: https://github.com/magnars/smart-forward.el
(require 'smart-forward)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Scheme.
(setq scheme-program-name
      "/usr/local/bin/racket")

;; Common Lisp.
;; Inferior Lisp.
(set-variable 'inferior-lisp-program "/usr/local/bin/clisp")

;; SLIME.
(add-to-list 'load-path "~/scm/slime/")
;; Autoload on use. See: http://ambience.info.ucl.ac.be/slime.html
(when (load "slime-autoloads" t)
  (setq slime-auto-connect 'always)
  (slime-setup '(slime-fancy inferior-slime)))
;(require 'slime-autoloads)
;(slime-setup '(slime-fancy))

;; EPG.
(require 'epa-file)
(epa-file-enable)


;; ========================================
;; Key bindings.
;; ========================================

(require 'key-bindings)

;; Ido keymap.
(defun wjb-ido-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map
    (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map
    (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook 'wjb-ido-keys)


;; ========================================
;; Final.
;; ========================================

;; Open up some dirs in dirtree.
(let ((dirtree-buffer "*dirtree*"))
  (dolist (dir initial-dirs-to-open)
    (dirtree dir dirtree-buffer))
  ;; Dedicate window and resize.
  (let ((window (get-buffer-window dirtree-buffer)))
    (set-window-dedicated-p window 'true)
    ;; TODO: Resize more intelligently.
    (adjust-window-trailing-edge window -5 t)))

;; Byte-recompile site-lisp-dir.
(byte-recompile-directory site-lisp-dir 0)

;; Workaround for a bug in emacs' http fetching. See:
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-12/msg00196.html
(setq url-http-attempt-keepalives nil)

(when is-mac (require 'mac))
