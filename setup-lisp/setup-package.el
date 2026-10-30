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

  ;; (push gnu package-archives)
  ;; (push nongnu package-archives)
  (push melpa package-archives)
  (push melpa-stable package-archives)

  ;; (defvar org '("org" . "https://orgmode.org/elpa/"))
  ;; (push org package-archives)

  ;; (defvar elpy '("elpy" . "https://jorgenschaefer.github.io/packages/"))
  ;; (push elpy package-archives)

  (unless (seq-every-p (lambda (archive)
                         (file-exists-p (expand-file-name (concat "archives/" (car archive))
                                                          package-user-dir)))
                       package-archives)
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

  (defvar wjb/packages
    (list
     (cons 'modus-themes melpa) ;; someday on gnu
     (cons 'auto-compile melpa)
     (cons 'dash melpa-stable)
     (cons 's melpa-stable)
     (cons 'f melpa-stable)
     (cons 'ht melpa)

     (cons 'exec-path-from-shell melpa-stable)
     (cons 'reveal-in-osx-finder melpa)
     (cons 'osx-plist melpa)
     (cons 'envrc melpa)
     (cons 'pinentry gnu)
     (cons 'editorconfig melpa)
     (cons 'dtrt-indent melpa)

     (cons 'pip-requirements melpa)
     (cons 'virtualenvwrapper melpa-stable)
     (cons 'elpy melpa)
     (cons 'rustic melpa)

     (cons 'magit melpa-stable)
     (cons 'ghub melpa-stable)
     (cons 'forge melpa-stable)
     (cons 'transient melpa)
     (cons 'git-modes nongnu)
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
     (cons 'yaml-imenu melpa)
     (cons 'web-mode melpa-stable)
     (cons 's3ed melpa)
     (cons 'nginx-mode melpa)
     (cons 'log4j-mode melpa)
     (cons 'pandoc-mode melpa)
     (cons 'dotenv-mode melpa)
     (cons 'hcl-mode melpa)
     (cons 'terraform-mode melpa)
     (cons 'csv-mode gnu)

     (cons 'treesit-auto melpa)

     (cons 'rainbow-mode gnu)
     (cons 'rainbow-delimiters melpa)

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

     (cons 'nerd-icons melpa)
     (cons 'treemacs-nerd-icons melpa)
     (cons 'doom-themes melpa)
     (cons 'doom-modeline melpa)
     (cons 'ligature melpa)

     (cons 'all-the-icons melpa)
     (cons 'all-the-icons-dired melpa)

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
     (cons 'yafolding melpa)
     (cons 'imenu-list melpa)
     (cons 'anzu melpa)
     (cons 'beginend melpa)
     (cons 'mwim melpa)
     ;; TODO: add css-comb and web-beautify
     (cons 'header2 melpa)
     (cons 'date-at-point melpa)
     (cons 'comment-dwim-2 melpa)
     ;; (cons 'aws-snippets melpa)
     (cons 'posframe melpa)
     (cons 'mini-frame melpa)

     (cons 'helpful melpa)
     ;; (cons 'discover-my-major melpa)
     (cons 'smart-dash melpa)
     (cons 'replace-from-region melpa)
     ;; (cons 'prodigy melpa)

     (cons 'know-your-http-well melpa)
     (cons 'restclient melpa)
     (cons 'ob-restclient melpa)

     (cons 'flycheck melpa)
     ;; (cons 'flycheck-status-emoji melpa)
     (cons 'flycheck-package melpa)
     (cons 'flycheck-inline melpa)
     (cons 'flycheck-posframe melpa)

     (cons 'nvm melpa)
     (cons 'json-mode melpa)
     (cons 'json-reformat melpa)
     (cons 'json-snatcher melpa)
     (cons 'js2-mode melpa)
     (cons 'rjsx-mode melpa)
     (cons 'jtsx melpa)
     ;; (cons 'js-doc melpa) ;; using own fork.
     (cons 'js2-refactor melpa-stable)
     (cons 'xref-js2 melpa)
     ;; (cons 'js2-highlight-vars melpa)
     (cons 'prettier-js melpa)
     ;; (cons 'discover-js2-refactor melpa)
     (cons 'tide melpa)
     (cons 'coffee-mode melpa)
     (cons 'yarn-mode melpa)
     ;; (cons 'npm-mode melpa) ;; using own fork.
     ;; (cons 'jest-mode melpa) ;; using own fork.

     (cons 'markdown-mode melpa)
     (cons 'markdown-toc melpa)

     (cons 'docker melpa)
     (cons 'dockerfile-mode melpa)
     (cons 'docker-compose-mode melpa)
     ;; (cons 'docker-tramp melpa)

     (cons 'projectile melpa)

     (cons 'company melpa)
     (cons 'lsp-mode melpa)
     (cons 'lsp-ui melpa)
     (cons 'lsp-java melpa)
     (cons 'dap-mode melpa)
     (cons 'consult-lsp melpa)
     ;; (cons 'company-restclient melpa)
     ;; (cons 'company-emoji melpa)
     ;; (cons 'company-nginx melpa)
     ;; (cons 'company-shell melpa)
     ;; (cons 'company-web melpa)
     ;; (cons 'company-flx melpa)
     ;; (cons 'company-statistics melpa)
     ;; (cons 'company-quickhelp melpa)
     ;; (cons 'company-ctags melpa)
     (cons 'corfu melpa)
     (cons 'cape melpa)
     (cons 'orderless melpa)
     (cons 'vertico melpa)
     (cons 'marginalia melpa)
     (cons 'nerd-icons-completion melpa)
     (cons 'consult melpa)
     (cons 'consult-projectile melpa)
     (cons 'consult-dir melpa)
     (cons 'consult-yasnippet melpa)

     (cons 'eldoc-box melpa)
     (cons 'flycheck-eglot melpa)
     (cons 'eglot-java melpa)

     ;; (cons 'helm-aws melpa) ;; using own fork.
     ;; (cons 'helm-projectile melpa)
     ;; (cons 'helm-ls-git melpa)
     ;; (cons 'helm-dired-recent-dirs melpa)
     ;; (cons 'helm-org-rifle melpa)
     ;; (cons 'ace-jump-helm-line melpa)

     ;; (cons 'flx-ido melpa)
     ;; (cons 'ido-completing-read+ melpa)
     ;; (cons 'amx melpa)
     ;; (cons 'project-shells melpa)

     (cons 'treemacs melpa)
     (cons 'treemacs-projectile melpa)
     ;; (cons 'treemacs-icons-dired melpa)

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
     (cons 'smartparens melpa)

     (cons 'gruvbox-theme melpa)
     (cons 'nimbus-theme melpa)
     (cons 'solaire-mode melpa)

     (cons 'bash-completion melpa)

     (cons 'adaptive-wrap gnu)
     (cons 'visual-fill-column melpa)
     (cons 'change-inner melpa)

     (cons 'kill-dollar-mode melpa)
     (cons 'pcsv melpa)

     ;; used by ai-code-interface
     ;; using eat b/c claude-code vterm code has to do some dance with
     ;; buffers/delete-window that causes an error for me
     (cons 'eat melpa)
     (cons 'ghostel melpa)
     (cons 'vterm melpa)

     (cons 'shell-maker melpa)
     (cons 'chatgpt-shell melpa)
     (cons 'dall-e-shell melpa)
     (cons 'acp melpa)
     (cons 'agent-shell melpa)
     (cons 'gptel melpa)
     (cons 'copilot-chat melpa)

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

  ;; Install packages if they're missing.
  (defun init--install-packages ()
    (apply #'packages-install wjb/packages))

  (defun wjb/sync-selected-packages ()
    "Set `package-selected-packages' to the names in `wjb/packages'."
    (interactive)
    (package--save-selected-packages (mapcar #'car wjb/packages)))

  (condition-case nil
      (init--install-packages)
    (error
     (message "Error installing packages in setup-package.el. Try package-refresh-contents, then (init--install-packages)."))))

(provide 'setup-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-package.el ends here
