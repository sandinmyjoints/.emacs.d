;; Based on:
;; https://github.com/magnars/.emacs.d/blob/master/setup-package.el

(when (require 'package nil t)

  (defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
  (defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/")) ;; tracks upstream
  (defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/")) ;; up to developer when to push
  (defvar org '("org" . "http://orgmode.org/elpa/"))

  (add-to-list 'package-archives melpa)
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
     (cons 'edit-server melpa)
     (cons 'exec-path-from-shell melpa)
     (cons 'gitconfig-mode marmalade)
     (cons 'gitignore-mode marmalade)
     (cons 'ido-ubiquitous marmalade)
     ;(cons 'magit marmalade) ;; Should be ok, because tracks maint branch.
     ;However, https://github.com/magit/magit#installing-from-marmalade says it
     ;is way outdated, so sticking with installing from git for now.
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
     ;(cons 'elnode marmalade)
     ;(cons 'slime-js marmalade)
     (cons 'anzu melpa)
     (cons 's melpa)
     (cons 'f melpa)
     (cons 'dash melpa)
     (cons 'nvm melpa)
     (cons 'virtualenvwrapper melpa)
     (cons 'rainbow-delimiters melpa)
     (cons 'yaml-mode melpa)
     (cons 'flycheck melpa)
     (cons 'web-mode melpa)
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
