;;; setup-company.el ---
;;
;; Filename: setup-company.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Fri Oct  3 15:05:19 2025 (-0400)
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


;; company / completion

(use-package pcomplete)

;; - a simple list gathers from all.
;; - a list with :with only gathers if the(/a?) backend before :with succeeds at the prefix command (more about with: https://github.com/company-mode/company-mode/issues/79)

;; If the group contains keyword ':with', the backends listed after this
;; keyword are ignored for the purpose of the 'prefix' command.
;; but I'm not sure whether the first one doesn't return prefix, the :with ones will be or not be called either?

;; :with means completions unconditionally; whereas the default is to only use them if
;; they return the same prefix as the first defined checker in the group

;; - order backends in a way that makes sense
;; - group backends
;; - set backends based on major mode. For example, higher minimum prefix in text modes (4)
;; - different behavior within comments
;; - understand company-capf

;; commands:
;; company-complete
;; company-complete-common-or-cycle
;; company-other-backend
;; company-diag

(use-package company
  :demand
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .2)
  (company-minimum-prefix-length 4)
  (company-show-numbers nil)
  (company-tooltip-align-annotations 't)
  (company-tooltip-width-grow-only t)
  (company-dabbrev-downcase nil)
  (company-selection-wrap-around t)
  (company-global-modes '(not git-commit-mode))

  :config
  (global-company-mode t)

  (make-variable-buffer-local 'company-backends)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)

  (defun wjb/set-company-minimum-prefix-length ()
    (setq-local company-minimum-prefix-length 3))
  (add-hook 'prog-mode-hook #'wjb/set-company-minimum-prefix-length)
  (add-hook 'restclient-mode-hook #'wjb/set-company-minimum-prefix-length)
)

(use-package company-yasnippet
  :after (company yasnippet)
  :config
  ;; company-yasnippet returns matches (for example, for a partial name of a
  ;; snippet) vs. yas-expand which expands a known snippet trigger.
  ;; Typing these on empty point gives autocomplete list of all snippets.
  ;; Hitting TAB (yas-expand) after an exact snippet trigger expands that snippet.
  (global-set-key (kbd "H-u") #'company-yasnippet) ;; H-y is awkward; H-u is close
  (global-set-key (kbd "C-c y") #'company-yasnippet)
  ;; (global-set-key (kbd "C-c C-y") #'company-yasnippet)
  )

(use-package company-buffer-line
  :after company
  :commands (company-same-mode-buffer-lines)
  :bind ("C-x C-l" . company-same-mode-buffer-lines))

(use-package company-emoji
  :after company)

(use-package company-restclient
  :after (company restclient))

(use-package company-nginx
  :ensure t
  :after (nginx-mode)
  :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

(defvar wjb/company-backends-original
  '(company-bbdb company-eclim company-semantic company-clang company-cmake company-capf company-files
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-dabbrev)
  "Original value of company-backends, fwiw.")

(defun wjb/company-backends-generic ()
  "Try some backend orderings."
  ;; mode-specific, smart
  (let (generic-backend (list))
    (dolist
        ;; last ends up first
        (backend '(company-clang company-cmake company-capf company-shell company-restclient company-css company-tide) generic-backend)
      (if (equal list 'company-capf)
          (push
           (list backend 'company-dabbrev-code :with :separate 'company-dabbrev 'company-emoji 'company-keywords)
           generic-backend)

        (push
         (list backend :with :separate 'company-dabbrev-code 'company-keywords)
         generic-backend)))

    ;; generic
    (setq generic-backend (append generic-backend '(company-files)))
    ;; fallback backends -- likely to return in almost all cases
    (setq generic-backend (append generic-backend
                       '(
                         ;; code
                         (company-dabbrev-code company-keywords)
                         ;; text
                         (company-emoji company-dabbrev)
                         )
                       ))
    (setq-default company-backends generic-backend)))

(wjb/company-backends-generic)

;; I think this ordering is good with manual cycling
;; prog:
;; (mode-primary :with :separate company-ctags company-capf company-keywords company-dabbrev-code)
;; text:
;; (company-capf company-keywords company-dabbrev-code company-dabbrev)
;; end:
;; (company-emoji company-capf company-dabbrev-code company-ctags company-keywords)
;; (company-files company-emoji company-dabbrev)

;; - tags-completion-at-point-function doesn't seem to work with company, and
;;   company has etags and ctags backends already, so it's unnecessary.
;; - pcomplete-completions-at-point seems to work only in org-mode, otherwise complains about pcomplete-here.

;; (setq completion-at-point-functions '())
;; (remove-hook completion-at-point-functions 'tags-completion-at-point-function)
;; (add-hook completion-at-point-functions #'pcomplete-completions-at-point)

(defvar wjb/company-backends-ts)
(setq wjb/company-backends-ts
      '(company-tide
        (company-capf company-keywords company-dabbrev-code company-dabbrev)))

(defvar wjb/company-backends-js)
(setq wjb/company-backends-js
      '(
        ;; (company-tide :with :separate company-capf company-keywords company-dabbrev-code)
        (company-tide :with :separate company-keywords company-dabbrev-code)
        (company-capf company-keywords company-dabbrev-code company-dabbrev)))

(defvar wjb/company-backends-org)
;; todo get company-capf working: pcomplete-completions-at-point. Maybe I need
;; to teach pcomplete what to do?
(setq wjb/company-backends-org
      '(company-emoji company-files company-capf company-dabbrev-code company-dabbrev))
(setq wjb/company-backends-md wjb/company-backends-org)

(defvar wjb/company-backends-css)
(setq wjb/company-backends-css
      '((company-css :with :separate company-capf company-keywords company-dabbrev-code)
        (company-capf company-keywords company-dabbrev-code company-dabbrev)))

(defvar wjb/company-backends-el)
(setq wjb/company-backends-el
      '((company-capf :with :separate company-keywords company-dabbrev-code)
        (company-keywords company-dabbrev-code company-dabbrev)))

(provide 'setup-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-company.el ends here
