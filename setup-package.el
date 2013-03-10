;; Based on:
;; https://github.com/magnars/.emacs.d/blob/master/setup-package.el

(when (require 'package nil t)

  (defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
  (defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))
  (defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))

  (add-to-list 'package-archives melpa)
  (add-to-list 'package-archives marmalade)

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

(provide 'setup-package)
