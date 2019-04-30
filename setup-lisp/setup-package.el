;;; setup-package.el --- Configure package management and install packages.
;;
;; Filename: setup-package.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Oct  1 09:24:43 2014 (-0700)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; Based on:
;; https://github.com/magnars/.emacs.d/blob/master/setup-package.el

(when (require 'package nil t)

  (defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
  (defvar elpy '("elpy" . "https://jorgenschaefer.github.io/packages/"))
  (defvar melpa '("melpa" . "https://melpa.org/packages/")) ;; tracks upstream
  (defvar melpa-mirror '("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")) ;; tracks upstream
  (defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (defvar org '("org" . "https://orgmode.org/elpa/"))

  (add-to-list 'package-archives org)
  (add-to-list 'package-archives elpy)
  (add-to-list 'package-archives melpa)
  (add-to-list 'package-archives melpa-stable)

  ;; TODO: package-archive-prioities: see
  ;; https://emacs.stackexchange.com/a/2989/2163

  ;; Use if melpa is down:
  ;; (add-to-list 'package-archives melpa-mirror)

  ;; TODO: See http://www.lonecpluspluscoder.com/2014/11/using-elpa-pinned-packages-gnu-emacs-24-4/
  ;; (add-to-list 'package-pinned-packages '(magit . "melpa-stable"))
  ;; TODO: as of Emacs 24.4, can use variable package-pinned-packages.

  (package-initialize)

  (unless (and (file-exists-p "~/.emacs.d/elpa/archives/melpa")
               (file-exists-p "~/.emacs.d/elpa/archives/gnu"))
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile
    (require 'use-package))

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
    (package-initialize))

  ;; Install packages if they're missing.
  (defun init--install-packages ()
    (packages-install
     (cons 'auto-compile melpa)
     (cons 'dash melpa-stable)
     (cons 's melpa-stable)
     (cons 'f melpa-stable)
     (cons 'ht melpa)

     (cons 'exec-path-from-shell melpa-stable)
     (cons 'reveal-in-osx-finder melpa)

     (cons 'flx-ido melpa)
     (cons 'ido-completing-read+ melpa)
     (cons 'amx melpa)
     ;; (cons 'smex melpa)

     (cons 'elpy elpy)
     (cons 'pip-requirements melpa)
     (cons 'virtualenvwrapper melpa-stable)
     (cons 'ein melpa)

     (cons 'magit melpa-stable)
     (cons 'gitconfig-mode melpa)
     (cons 'gitignore-mode melpa)
     (cons 'browse-at-remote melpa)

     (cons 'tree-mode melpa) ; dirtree requirement.
     ;; he updates it here: https://www.emacswiki.org/emacs/download/dired%2b.el
     ;; (cons 'dired+ melpa)
     (cons 'yaml-mode melpa-stable)
     (cons 'web-mode melpa-stable)
     (cons 'less-css-mode melpa-stable)
     (cons 'nginx-mode melpa)
     (cons 'log4j-mode melpa)
     (cons 'pandoc-mode melpa)

     (cons 'rainbow-mode melpa-stable) ;; Emacs >=24 only
     (cons 'rainbow-delimiters melpa-stable)
     (cons 'paren-face melpa)
     (cons 'beacon melpa)
     (cons 'auto-dim-other-buffers melpa)
     (cons 'diminish melpa-stable)
     (cons 'xterm-color melpa)
     (cons 'vimish-fold melpa)
     (cons 'unicode-troll-stopper melpa)
     (cons 'smart-mode-line melpa-stable)
     (cons 'minions melpa-stable)
     (cons 'multiple-cursors melpa)
     (cons 'mc-extras melpa)
     (cons 'highlight-thing melpa)
     (cons 'expand-region melpa)
     (cons 'easy-kill melpa)
     (cons 'easy-kill-extras melpa)
     (cons 'page-break-lines melpa)
     (cons 'centered-cursor-mode melpa)
     (cons 'eyebrowse melpa)
     (cons 'nameframe melpa)
     (cons 'olivetti melpa)
     (cons 'writeroom-mode melpa)

     (cons 'which-key melpa)
     (cons 'apu melpa) ;; Apropos Unicode characters.
     (cons 'google-this melpa)
     (cons 'atomic-chrome melpa)
     (cons 'quickrun melpa)
     (cons 'wgrep melpa)
     (cons 'symbol-overlay melpa)
     (cons 'smartscan melpa)
     (cons 'hungry-delete melpa)
     (cons 'launchctl melpa)
     (cons 'smart-tab melpa)
     (cons 'ace-window melpa-stable)
     (cons 'vlf melpa)
     (cons 'nhexl-mode melpa)
     (cons 'sqlformat melpa) ;; requires sqlformat binary
     (cons 'urlenc melpa)
     (cons 'yasnippet melpa)
     (cons 'anzu melpa)
     (cons 'beginend melpa)
     ;; (cons 'pivotal-tracker melpa)
     ;; (cons 'org-pivotal melpa) ;; using own fork
     ;; TODO: add css-comb and web-beautify
     (cons 'header2 melpa)
     (cons 'paradox melpa)
     (cons 'date-at-point melpa)
     (cons 'recompile-on-save melpa)
     (cons 'comment-dwim-2 melpa)
     (cons 'aws-snippets melpa)
     (cons 'posframe melpa)

     (cons 'helpful melpa)
     (cons 'discover-my-major melpa)
     (cons 'elisp-demos melpa)
     (cons 'smart-dash melpa)
     (cons 'prodigy melpa)
     (cons 'electric-operator melpa)

     (cons 'know-your-http-well melpa)
     (cons 'restclient melpa)

     (cons 'flycheck-status-emoji melpa)
     (cons 'flycheck melpa)
     ;; (cons 'flycheck-pos-tip melpa)
     ;;
     ;; two different flycheck-inlines, though they are about to
     ;; merge: https://github.com/stardiviner/flycheck-inline/issues/4
     ;; (cons 'flycheck-inline melpa) ;; using my own fork.

     (cons 'nvm melpa)
     (cons 'json-reformat melpa)
     (cons 'json-snatcher melpa)
     (cons 'js2-mode melpa)
     ;; (cons 'js-doc melpa) ;; using own fork.
     (cons 'js2-refactor melpa-stable)
     (cons 'js2-highlight-vars melpa)
     (cons 'prettier-js melpa)
     (cons 'add-node-modules-path melpa)
     (cons 'discover-js2-refactor melpa)
     (cons 'coffee-mode melpa)
     ;; (cons 'npm-mode melpa) ;; using own fork.
     (cons 'yarn-mode melpa)
     ;; (cons 'jest-mode melpa) ;; using own fork.

     (cons 'eglot melpa)
     (cons 'lsp-mode melpa)
     (cons 'lsp-ui melpa)

     (cons 'markdown-mode melpa)
     (cons 'markdown-toc melpa)

     (cons 'docker melpa)
     (cons 'dockerfile-mode melpa)
     (cons 'docker-compose-mode melpa)
     (cons 'docker-tramp melpa)

     (cons 'projectile melpa)

     (cons 'company melpa)
     (cons 'company-restclient melpa)
     (cons 'company-emoji melpa)
     (cons 'company-lsp melpa)
     (cons 'company-nginx melpa)
     (cons 'company-shell melpa)
     (cons 'company-web melpa)
     (cons 'company-flx melpa)
     (cons 'company-statistics melpa)
     (cons 'company-quickhelp melpa)

     ;; (cons 'helm-aws melpa) ;; using own fork.
     (cons 'helm-projectile melpa)
     (cons 'helm-ls-git melpa)
     (cons 'helm-dired-recent-dirs melpa)
     (cons 'helm-org-rifle melpa)
     (cons 'ace-jump-helm-line melpa)

     (cons 'ivy melpa)
     (cons 'ivy-hydra melpa)
     (cons 'ivy-posframe melpa)
     (cons 'counsel melpa)
     (cons 'counsel-projectile melpa)
     (cons 'counsel-css melpa)
     (cons 'counsel-tramp melpa)
     (cons 'swiper melpa)

     (cons 'ob-sql-mode melpa)
     (cons 'org-clock-csv melpa)

     (cons 'dumb-jump melpa)
     (cons 'smart-jump melpa)

     (cons 'pcre2el melpa)
     (cons 'perl6-mode melpa)
     (cons 'flycheck-perl6 melpa)

     (cons 'paredit melpa)
     (cons 'paredit-everywhere melpa)

     (cons 'gruvbox-theme melpa)
     (cons 'nimbus-theme melpa)

     ;; Untried, but consider:
     ;; (cons 'flycheck-color-mode-line melpa)
     ;; https://github.com/marsmining/ox-twbs

     ;; Uninstalled, but consider:

     ;; (cons 'gh melpa)
     ;; (cons 'smartparens melpa-stable)
     ;; (cons 'phi-search melpa)

     ;; (cons 'counsel-etags melpa)
     ;; (setq-default counsel-etags-find-program "gfind")
     ;; (setq-default counsel-etags-grep-program "ggrep")

     ;; (cons 'fill-column-indicator melpa-stable)

     ;; (cons 'magit-gh-pulls melpa) ;; Doesn't work as of June 2016
     ;; (cons 'charmap melpa)
     ;; (cons 'smart-forward)
     ;; (cons 'move-text melpa-stable)
     ;; (cons 'gist melpa-stable)
     ;; (cons 'htmlize melpa-stable)
     ;; (cons 'elisp-slime-nav melpa-stable)
     ;; (cons 'elnode marmalade)
     ;; (cons 'slime-js marmalade)
     (cons 'edit-server melpa)
     (cons 'edit-server-htmlize melpa)
     ;; (cons 'indium melpa)
     ;; (cons 'smart-indent-rigidly melpa)
     ;; (cons 'auto-install melpa)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-package.el ends here
