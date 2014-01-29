;; -*- lexical-binding: t -*-

;;; init.el
;;; See: https://github.com/sandinmyjoints/.emacs.d


;; ========================================
;; Definitions.
;; ========================================

;; Set Emacs Lisp directory.
(setq site-lisp-dir
      (expand-file-name "elisp" user-emacs-directory))

;; Directories to open in dirtree on start.
(setq initial-dirs-to-open
      '("~/.emacs.d"
        "~/scm/wjb"
        "~/scm/sd/ops"
        "~/scm/sd/fluencia"
        "~/scm/sd/fluencia-clean"
        "~/scm/sd/neodarwin"
        "~/scm/sd/neodarwin-clean"))

;; An initial file to open if it exists.
(setq initial-file "~/.emacs.d/init.el")

;; Set custom markers.
;; Args:
;; 1. Marker.
;; 2. Register to store.
;; 3. Key bindings to set/clear marker.
;; 4. Insert/remove marker from current buffer?
;;
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

;; Require Common Lisp. (cl in <=24.2, cl-lib in >=24.3.)
(if (require 'cl-lib nil t)
  ;; Madness: cl-block-wrapper was an alias for identity in 24.2, then it was
  ;; renamed cl--block-wrapper in 24.3, but somehow my 10.6.8 machine still
  ;; wants cl-block-wrapper when running 24.3 (though my 10.8.3 machine has no
  ;; such problem), so help it out.
  (progn
    (defalias 'cl-block-wrapper 'identity)
    (defalias 'member* 'cl-member)
    (defalias 'adjoin 'cl-adjoin))
  ;; Else we're on an older version so require cl.
  (require 'cl))

;; Use server.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Save desktop.
;(desktop-save-mode 1)

;; Allow the very useful set-goal-column.
(put 'set-goal-column 'disabled nil)

;; 24-hour time.
(setq display-time-24hr-format t)

;; Tramp.
(setq tramp-default-method "ssh")

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
;; TODO: dir-local list of paths to exclude from grep-find (e.g., .git and node_modules)
;;
(setq find-args "! -name \"*~\" ! -name \"#*#\" ! -wholename \"*node_modules*\" ! -wholename \"*.git*\" -type f -print0 | xargs -0 grep -E -C 5 -niH -e " default-find-cmd (concat "find " ". " find-args))
(grep-compute-defaults)
(grep-apply-setting 'grep-find-command default-find-cmd)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" default)))
 '(dired-isearch-filenames (quote dwim))
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(markdown-command "/Users/william/bin/markdown")
 '(ns-command-modifier (quote meta))
 '(rst-level-face-base-light 51)
 '(safe-local-variable-values (quote ((find-in-project-dir . "~/src/sd/neodarwin/src") (find-in-project-default-dir . "/Users/william/scm/sd/fluencia-clean/src") (find-in-project-dir . "/Users/william/scm/sd/fluencia/src") (find-in-project-default-dir . "/Users/william/scm/sd/fluencia/src") (find-in-project-default-dir . "/Users/william/scm/sd/fluencia-clean/src") (find-in-project-default-dir . "/Users/william/scm/sd/fluencia/src") (find-in-project-dir . "/Users/william/scm/sd/fluencia/src") (find-in-project-dir . /Users/william/scm/sd/fluencia/src) (find-in-project-dir . "~/.emacs.d") (find-in-project-dir . "~/scm/sd/fluencia/src") (find-in-project-dir . "~/scm/sd/database-platform"))))
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
(when (require 'package nil t)
  (defun init--install-packages ()
    (packages-install
     (cons 'edit-server melpa)
     (cons 'exec-path-from-shell melpa)
     ;(cons 'git-commit-mode marmalade)
     (cons 'gitconfig-mode marmalade)
     (cons 'gitignore-mode marmalade)
     (cons 'ido-ubiquitous marmalade)
     ;(cons 'magit marmalade) ;; Should be ok, because tracks maint branch.
     (cons 'rainbow-mode melpa) ;; Emacs >=24 only
     (cons 'dired+ marmalade)
     (cons 'tree-mode melpa) ; dirtree requirement.
     (cons 'auto-install melpa)
     (cons 'json-mode marmalade)
     (cons 'fill-column-indicator melpa)
     (cons 'yasnippet marmalade)
     ;(cons 'paredit melpa)
     ;(cons 'move-text melpa)
     ;(cons 'gist melpa)
     ;(cons 'htmlize melpa)
     ;(cons 'elisp-slime-nav melpa)
     ;;(cons 'elnode marmalade)
     ;(cons 'slime-js marmalade)
     (cons 'anzu melpa)
     (cons 's melpa)
     (cons 'f melpa)
     (cons 'dash melpa)
     (cons 'nvm melpa)
     (cons 'virtualenvwrapper melpa)
     (cons 'rainbow-delimiters melpa)
   ))

  (condition-case nil
      (init--install-packages)
    (error
     (package-refresh-contents)
     (init--install-packages))))

;; For Emacs Lisp not available as submodule or package (e.g., windata.el).
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/"))

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

;; dired-jump is useful.
(require 'dired-x)

;; ========================================
;; Some hooks.
;; ========================================

;; Force 2-space indentation in css-mode.
(add-hook 'css-mode-hook
          (function
           (lambda ()
             (setq css-indent-offset 2))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (abbrev-mode 1))))
;; ========================================
;; Require/autoload and config packages.
;; ========================================

(when (require 'smex nil t)
  (smex-initialize))

(when (require 'ido nil t)
  (eval-after-load 'ido '(require 'setup-ido)))

;; Org-mode.
(require 'org-install)
(eval-after-load 'org '(require 'setup-org))

(when (require 'dired+ nil t)
  (eval-after-load 'dired+ '(require 'setup-dired+)))

(autoload 'magit-status "magit")
(autoload 'magit-log "magit")
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'magit '(require 'rebase-mode))

(autoload 'ibuffer "ibuffer" "List buffers." t)

;(eval-after-load 'grep '(require 'setup-rgrep))
;(eval-after-load 'shell '(require 'setup-shell))
;(require 'setup-hippie)

;; Yasnippet.
;(require 'setup-yasnippet)
;; Work-around for tab complaining when yas is active in ansi-term. See:
;; https://github.com/capitaomorte/yasnippet/issues/289
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;(require 'setup-perspective)
;(require 'setup-ffip)
;(require 'setup-html-mode)
;(require 'setup-paredit)

;; Rainbow mode.
(when (require 'rainbow-mode nil t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  (add-hook 'less-css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

;; Anything.
;(require 'helm-config)
;(helm-mode 1)

;; Imenu.
(when (require 'imenu nil t)
  (autoload 'idomenu "idomenu" nil t))

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
;;(autoload 'dirtree "dirtree" "Dirtree." t)


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

(add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 80)))

;; Paired tick is useful in some modes.
;; TODO: This throws errors if run here. Probably needs to go into mode hooks.
;; (modify-syntax-entry ?\` "$" markdown-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" text-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" rst-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" org-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" coffee-mode-syntax-table)

;; Fill column indicator.
;; See: https://github.com/alpaker/Fill-Column-Indicator
;;
;; Commenting out for now due to bug in Emacs 24.3 that causes C-p to skip a
;; line. See: https://github.com/alpaker/Fill-Column-Indicator/issues/31
;;
;; TODO: Uncomment for Emacs 24.4.
;;
;; (when (require 'fill-column-indicator nil t)

;;   ;; Make fci-mode global...
;;   (define-globalized-minor-mode global-fci-mode fci-mode
;;     (lambda () (fci-mode 1)))
;;   (global-fci-mode 1)

;;   ;; ...except for these modes.
;;   (defun no-fci ()
;;     (fci-mode -1))
;;   (add-hook 'dirtree-mode-hook 'no-fci)
;;   (add-hook 'dired-mode-hook 'no-fci)
;;   (add-hook 'dired+-mode-hook 'no-fci)
;;   (add-hook 'org-mode-hook 'no-fci)
;;   (add-hook 'magit-mode-hook 'no-fci)
;;   (add-hook 'term-mode-hook 'no-fci)
;;   (add-hook 'shell-mode-hook 'no-fci))


;; Python
;; TODO: Fix ipython for use in emacs.
;(setq py-install-directory "~/emacs/")
;(require 'python-mode)
;(setq py-shell-name "ipython")

;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; js2-mode
(eval-after-load 'js2-mode '(require 'setup-js2-mode))

;; json
(require 'json nil t)
(require 'json-pretty-print nil t)

;; Smart-tab. See: https://raw.github.com/genehack/smart-tab/master/smart-tab.el
(when (require 'smart-tab nil t)
  (global-smart-tab-mode 1))

(when (require 'anzu-mode nil t)
  (global-anzu-mode 1))

;; multiple-cursors.
;;
;; See: https://github.com/magnars/multiple-cursors.el
;;
;;(require 'multiple-cursors)

(defun create-cursor (arg)
  "Create fake cursor. If called with prefix, turn on
mc/maybe-multiple-cursors-mode."
  (interactive "P")
  (cond
   ((null arg)
      (mc/create-fake-cursor-at-point))
  (t
    (mc/maybe-multiple-cursors-mode))))

;(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'set-rectangular-region-anchor)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-M->") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c ,") 'create-cursor)
(global-set-key (kbd "C-c C-,") 'create-cursor)
(global-set-key (kbd "C-c .") '(lambda ()
                                 (interactive)
                                 (mc/maybe-multiple-cursors-mode)))

;; expand-region.
;; See: https://github.com/magnars/expand-region.el
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; smart-forward
;; See: https://github.com/magnars/smart-forward.el
(when (require 'smart-forward nil t)
  (global-set-key (kbd "M-<up>") 'smart-up)
  (global-set-key (kbd "M-<down>") 'smart-down)
  (global-set-key (kbd "M-<left>") 'smart-backward)
  (global-set-key (kbd "M-<right>") 'smart-forward))

;; Scheme.
;; (setq scheme-program-name
;;       "/usr/local/bin/racket")
(setq scheme-program-name
      "/Applications/MIT-GNUScheme.app/Contents/Resources/mit-scheme")
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

;; Edit-server for Edit-in-Emacs Chrome extension.
(when (require 'edit-server nil t)
  (edit-server-start))

;; TODO: add this to list of packages to install
;; TODO: figure out how to handle adding coffee-mode
(when (require 'yasnippet nil t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/yasnippet-coffee-script-snippets/")
  (yas-global-mode 1))

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

;; Open up some dirs in dirtree if it's available.
(when (and (require 'tree-mode nil t) (require 'dirtree nil t))
  (let ((dirtree-buffer "*dirtree*"))
    (dolist (dir initial-dirs-to-open)
      (when (file-accessible-directory-p dir)
        (dirtree dir dirtree-buffer)))
    ;; Dedicate window and resize.
    (let ((window (get-buffer-window dirtree-buffer)))
      (set-window-dedicated-p window t)
      ;; TODO: Resize more intelligently.
      (adjust-window-trailing-edge window -5 t))))

;; Byte-recompile site-lisp-dir.
(byte-recompile-directory site-lisp-dir 0)

;; Workaround for a bug in emacs' http fetching. See:
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-12/msg00196.html
(setq url-http-attempt-keepalives nil)

(when is-mac (require 'mac))

;; Load something that might be useful.
(when (file-readable-p initial-file)
  (setq initial-buffer-choice initial-file))

;; Spaces around line numbers.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

(when (require 'rvm nil t)
  (rvm-use-default)) ;; use rvm's default ruby for the current Emacs session

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; Paired tick is useful in some modes.
;; TODO: Probably Can't run these until the mode has been loaded or something.
;; (modify-syntax-entry ?\` "$" markdown-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" text-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" rst-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" org-mode-syntax-table)
;; (modify-syntax-entry ?\` "$" coffee-mode-syntax-table)
