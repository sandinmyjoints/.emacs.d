;; Based on:
;; https://github.com/magnars/.emacs.d/blob/master/setup-package.el

(when (require 'package nil t)

  (defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
  (defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/")) ;; tracks upstream
  (defvar melpa-stable '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/")) ;; up to developer when to push
  (defvar org '("org" . "http://orgmode.org/elpa/"))

  (add-to-list 'package-archives melpa)
  (add-to-list 'package-archives melpa-stable)
  (add-to-list 'package-archives marmalade)
  (add-to-list 'package-archives org)

  (package-initialize)

  (unless (and (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
               (file-exists-p "~/.emacs.d/elpa/archives/melpa"))
    (file-exists-p "~/.emacs.d/elpa/archives/gnu")
    (package-refresh-contents))

  (defun packages-install (&rest packages)
    (mapc (lambda (package)
            (let ((name (car package))
                  (repo (cdr package)))
              (when (not (package-installed-p name))
                (if (y-or-n-p (format "Package %s is missing. Install it? " package))
                    (let ((package-archives (list repo)))
                      (package-initialize)
                      (package-install name))))))
          packages)
    (package-initialize)
    (delete-other-windows)))

;; Install packages if they're missing.
(when (require 'package nil t)
  (defun init--install-packages ()
    (packages-install
     (cons 'edit-server melpa-stable)
     (cons 'exec-path-from-shell melpa-stable)
     (cons 'gitconfig-mode marmalade)
     (cons 'gitignore-mode marmalade)
     (cons 'ido-ubiquitous marmalade)
     (cons 'magit melpa-stable)
     (cons 'git-rebase-mode melpa)
     ;(cons 'magit marmable) ;; Should be ok, because tracks maint branch.
     ;However, https://github.com/magit/magit#installing-from-marmalade says it
     ;is way outdated, so sticking with installing from git for now.
     (cons 'rainbow-mode melpa-stable) ;; Emacs >=24 only
     (cons 'dired+ marmalade)
     (cons 'tree-mode melpa-stable) ; dirtree requirement.
     (cons 'auto-install melpa-stable)
     (cons 'json-mode marmalade)
     (cons 'fill-column-indicator melpa-stable)
     (cons 'yasnippet marmalade)
     ;(cons 'paredit melpa-stable)
     ;(cons 'move-text melpa-stable)
     ;(cons 'gist melpa-stable)
     ;(cons 'htmlize melpa-stable)
     ;(cons 'elisp-slime-nav melpa-stable)
     ;(cons 'elnode marmalade)
     ;(cons 'slime-js marmalade)
     (cons 'anzu melpa-stable)
     (cons 's melpa-stable)
     (cons 'f melpa-stable)
     (cons 'dash melpa-stable)
     (cons 'nvm melpa-stable)
     (cons 'virtualenvwrapper melpa-stable)
     (cons 'rainbow-delimiters melpa-stable)
     (cons 'yaml-mode melpa-stable)
     (cons 'flycheck melpa-stable)
     (cons 'web-mode melpa-stable)
   ))

  (condition-case nil
      (init--install-packages)
    (error
     (package-refresh-contents)
     (init--install-packages))))


;; A different take on a package installer, from
;; https://github.com/purcell/emacs.d
;;
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'setup-package)
