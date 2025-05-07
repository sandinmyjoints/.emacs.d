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
  ;; TODO: package-archive-prioities: see
  ;; https://emacs.stackexchange.com/a/2989/2163

  ;; TODO: See http://www.lonecpluspluscoder.com/2014/11/using-elpa-pinned-packages-gnu-emacs-24-4/
  ;; (add-to-list 'package-pinned-packages '(magit . "melpa-stable"))
  ;; TODO: as of Emacs 24.4, can use variable package-pinned-packages.
  ;;
  ;; package.el adds installed packages to package-selected-packages in
  ;; custom.el. I have the option of managing it myself.

  (defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
  (defvar nongnu '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (defvar melpa '("melpa" . "https://melpa.org/packages/")) ;; tracks upstream
  (defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

  (setq package-archives '())

  (push gnu package-archives)
  (push nongnu package-archives)
  (push melpa package-archives)
  (push melpa-stable package-archives)

  ;; (defvar org '("org" . "https://orgmode.org/elpa/"))
  ;; (push org package-archives)

  ;; (defvar elpy '("elpy" . "https://jorgenschaefer.github.io/packages/"))
  ;; (push elpy package-archives)

  (unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
               (file-exists-p "~/.emacs.d/elpa/archives/nongnu")
               (file-exists-p "~/.emacs.d/elpa/archives/melpa")
               (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
    (package-refresh-contents)) ;; Network dependency.

  (defun packages-install (&rest packages)
    (mapc (lambda (package)
            (let ((name (car package))
                  (repo (cdr package)))
              (when (not (package-installed-p name))
                (if (y-or-n-p (format "Package %s is missing. Install it? " package))
                    (let ((package-archives (list repo)))
                      ;; package-install adds to package-selected-packages.
                      (package-install name))))))
          packages))

  ;; Install packages if they're missing.
  (defun init--install-packages ()
    (packages-install
     (cons 'modus-themes melpa) ;; someday on gnu
     (cons 'auto-compile melpa)
     (cons 'dash melpa-stable)
     (cons 's melpa-stable)
     (cons 'f melpa-stable)
     (cons 'ht melpa)

     (cons 'exec-path-from-shell melpa-stable)
     (cons 'reveal-in-osx-finder melpa)
     (cons 'envrc melpa)

     (cons 'flx-ido melpa)
     (cons 'ido-completing-read+ melpa)
     ;; (cons 'amx melpa)

     (cons 'pip-requirements melpa)
     (cons 'virtualenvwrapper melpa-stable)
     ;; useful but both are kind of a pain:
     ;; (cons 'elpy elpy)
     ;; (cons 'ein melpa)

     (cons 'magit melpa-stable)
     (cons 'magit-popup melpa-stable)
     (cons 'ghub melpa-stable)
     (cons 'forge melpa-stable)
     ;; (cons 'git-modes melpa) ;; installs several packages; they don't get recognized as installed
     (cons 'browse-at-remote melpa)
     ;; (cons 'github-review melpa)
     (cons 'git-link melpa-stable)

     ;; (cons 'tree-mode melpa) ; dirtree requirement.
     ;; he updates it here: https://www.emacswiki.org/emacs/download/dired%2b.el
     ;; (cons 'dired+ melpa) ;; is in /elisp
     (cons 'diredfl melpa)
     (cons 'dired-subtree melpa)
     (cons 'trashed melpa)
     (cons 'yaml-mode melpa)
     (cons 'web-mode melpa-stable)
     (cons 'less-css-mode melpa-stable)
     (cons 'nginx-mode melpa)
     (cons 'log4j-mode melpa)
     (cons 'pandoc-mode melpa)
     (cons 'dotenv-mode melpa)
     (cons 'csv-mode gnu)

     (cons 'treesit-auto melpa)

     ;; It always thinks these two aren't installed even when they are, so
     ;; disabling for now:
     ;;
     ;; (cons 'rainbow-mode gnu) ;; Emacs >=24 only
     ;; (cons 'rainbow-delimiters melpa-stable)

     (cons 'paren-face melpa)
     (cons 'beacon melpa)
     ;; (cons 'auto-dim-other-buffers melpa)
     (cons 'diminish melpa-stable)
     (cons 'xterm-color melpa)
     ;; (cons 'unicode-troll-stopper melpa)
     (cons 'minions melpa-stable)
     (cons 'visual-regexp melpa)
     (cons 'multiple-cursors melpa)
     (cons 'mc-extras melpa)
     (cons 'expand-region melpa)
     (cons 'easy-kill melpa)
     (cons 'easy-kill-extras melpa)
     (cons 'copy-as-format melpa)
     (cons 'page-break-lines melpa)
     (cons 'centered-cursor-mode melpa)
     ;; (cons 'eyebrowse melpa)
     ;; (cons 'nameframe melpa)
     ;; (cons 'olivetti melpa)

     (cons 'nerd-icons melpa)
     (cons 'doom-themes melpa)
     (cons 'doom-modeline melpa)
     (cons 'ligature melpa)

     ;; It always thinks these two aren't installed even when they are, so
     ;; disabling for now:
     ;;
     ;; (cons 'all-the-icons melpa)
     ;; (cons 'all-the-icons-dired melpa)

     (cons 'which-key melpa)
     (cons 'which-key-posframe melpa)
     ;; (cons 'apu nongnu) ;; Apropos Unicode characters.
     ;; (cons 'google-this melpa)
     ;; (cons 'atomic-chrome melpa)
     (cons 'quickrun melpa)
     (cons 'wgrep melpa)
     (cons 'symbol-overlay melpa)
     (cons 'launchctl melpa)
     (cons 'ace-window melpa-stable)
     (cons 'vlf melpa)
     ;; (cons 'nhexl-mode gnu)
     (cons 'sqlformat melpa) ;; requires sqlformat binary
     (cons 'urlenc melpa)
     (cons 'yasnippet melpa)
     ;; (cons 'yafolding melpa)
     (cons 'anzu melpa)
     (cons 'beginend melpa)
     (cons 'mwim melpa)
     ;; (cons 'pivotal-tracker melpa)
     ;; (cons 'org-pivotal melpa) ;; using own fork
     ;; TODO: add css-comb and web-beautify
     ;; (cons 'header2 nongnu)
     (cons 'date-at-point melpa)
     (cons 'recompile-on-save melpa)
     (cons 'comment-dwim-2 melpa)
     ;; (cons 'aws-snippets melpa)
     (cons 'posframe melpa)

     (cons 'helpful melpa)
     ;; (cons 'discover-my-major melpa)
     (cons 'elisp-demos melpa)
     (cons 'smart-dash melpa)
     (cons 'replace-from-region melpa)
     ;; (cons 'prodigy melpa)

     (cons 'know-your-http-well melpa)
     (cons 'restclient melpa)

     (cons 'flycheck melpa)
     ;; (cons 'flycheck-status-emoji melpa)
     (cons 'flycheck-package melpa)
     (cons 'flycheck-inline melpa)

     (cons 'nvm melpa)
     (cons 'json-reformat melpa)
     (cons 'json-snatcher melpa)
     (cons 'js2-mode melpa)
     (cons 'rjsx-mode melpa)
     (cons 'indium melpa)
     ;; (cons 'js-doc melpa) ;; using own fork.
     (cons 'js2-refactor melpa-stable)
     ;; (cons 'js2-highlight-vars melpa)
     (cons 'prettier-js melpa)
     ;; (cons 'discover-js2-refactor melpa)
     (cons 'tide melpa)
     (cons 'coffee-mode melpa)
     (cons 'yarn-mode melpa)
     ;; (cons 'npm-mode melpa) ;; using own fork.
     ;; (cons 'jest-mode melpa) ;; using own fork.

     ;; (cons 'eglot melpa)
     (cons 'eldoc-box melpa)

     (cons 'markdown-mode melpa)
     (cons 'markdown-toc melpa)

     (cons 'docker melpa)
     (cons 'dockerfile-mode melpa)
     (cons 'docker-compose-mode melpa)
     ;; (cons 'docker-tramp melpa)

     (cons 'projectile melpa)

     (cons 'company melpa)
     (cons 'company-restclient melpa)
     (cons 'company-emoji melpa)
     (cons 'company-nginx melpa)
     (cons 'company-shell melpa)
     (cons 'company-web melpa)
     ;; (cons 'company-flx melpa)
     (cons 'company-statistics melpa)
     ;; (cons 'company-quickhelp melpa)
     ;; (cons 'company-ctags melpa)

     ;; (cons 'helm-aws melpa) ;; using own fork.
     (cons 'helm-projectile melpa)
     (cons 'helm-ls-git melpa)
     (cons 'helm-dired-recent-dirs melpa)
     (cons 'helm-org-rifle melpa)
     ;; (cons 'ace-jump-helm-line melpa)

     (cons 'ivy melpa)
     (cons 'ivy-hydra melpa)
     (cons 'ivy-posframe melpa)
     (cons 'counsel melpa)
     ;; (cons 'counsel-etags melpa)
     (cons 'counsel-projectile melpa)
     (cons 'counsel-css melpa)
     (cons 'counsel-tramp melpa)
     (cons 'swiper melpa)
     (cons 'prescient melpa)
     (cons 'ivy-prescient melpa)
     (cons 'company-prescient melpa)
     ;; (cons 'project-shells melpa)

     (cons 'treemacs melpa)
     (cons 'treemacs-icons-dired melpa)

     (cons 'ob-sql-mode melpa)
     (cons 'ox-reveal melpa)
     (cons 'ox-pandoc melpa)
     (cons 'ox-slack melpa)
     (cons 'org-clock-csv melpa)

     (cons 'htmlize melpa)

     (cons 'dumb-jump melpa)
     (cons 'smart-jump melpa)
     (cons 'ag melpa) ;; smart-jump uses this

     (cons 'pcre2el melpa)
     ;; (cons 'flycheck-perl6 melpa)

     (cons 'paredit melpa)
     (cons 'paredit-everywhere melpa)

     (cons 'gruvbox-theme melpa)
     (cons 'nimbus-theme melpa)
     (cons 'solaire-mode melpa)

     (cons 'bash-completion melpa)

     ;; (cons 'adaptive-wrap gnu)
     (cons 'visual-fill-column melpa)
     (cons 'change-inner melpa)

     (cons 'kill-dollar-mode melpa)
     (cons 'pcsv melpa)

     ;; Untried, but consider:
     ;; (cons 'flycheck-color-mode-line melpa)
     ;; https://github.com/marsmining/ox-twbs

     ;; Uninstalled, but consider:

     ;; (cons 'smex melpa)
     ;; (cons 'writeroom-mode melpa)
     ;; (cons 'smartscan melpa)
     ;; (cons 'hungry-delete melpa)
     ;; (cons 'smart-tab melpa)
     ;; (cons 'highlight-thing melpa)
     ;; (cons 'equake melpa)
     ;; (cons 'electric-operator melpa)
     ;; (cons 'gh melpa)
     ;; (cons 'smartparens melpa-stable)
     ;; (cons 'phi-search melpa)
     ;; (cons 'dashboard melpa)
     ;; (cons 'paradox melpa)

     ;; (cons 'fill-column-indicator melpa-stable)

     ;; (cons 'magit-gh-pulls melpa) ;; Doesn't work as of June 2016
     ;; (cons 'charmap melpa)
     ;; (cons 'smart-forward)
     ;; (cons 'move-text melpa-stable)
     ;; (cons 'gist melpa-stable)
     ;; (cons 'elisp-slime-nav melpa-stable)
     ;; (cons 'elnode marmalade)
     ;; (cons 'slime-js marmalade)
     ;; (cons 'edit-server melpa)
     ;; (cons 'edit-server-htmlize melpa)
     ;; (cons 'smart-indent-rigidly melpa)
     ;; (cons 'auto-install melpa)
     ;; (cons 'emacsql-sqlite-module melpa-stable)
     ;; (cons 'perl6-mode melpa)
     ))

  (condition-case nil
      (init--install-packages)
    (error
     (message "Error installing packages in setup-package.el. Try package-refresh-contents, then (init--install-packages)."))))

(provide 'setup-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-package.el ends here
