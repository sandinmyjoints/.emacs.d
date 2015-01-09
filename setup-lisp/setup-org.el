;;; setup-org.el --- Configure org-mode.
;;
;; Filename: setup-org.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Fri Jan  9 11:50:37 2015 (-0800)
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


;; Org-mode setup.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)
(setq org-clock-persist 'history)
;(org-clock-persistence-insinuate)
(setq org-blank-before-new-entry
      '((heading . nil) (plain-list-item . nil))) ;; can switch back to auto soon


(setq org-todo-keywords
      '((sequence "TODO" "ACTIVE" "|" "DONE" "INACTIVE")))

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(add-hook 'org-mode-hook
	  (lambda ()
        (auto-fill-mode 1)
        (set-fill-column 80)
        (fci-mode -1)
        (local-set-key (kbd "<S-up>") 'outline-previous-visible-heading)
        (local-set-key (kbd "<S-down>") 'outline-next-visible-heading)))

(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)
(setq org-return-follows-link t)

(provide 'setup-org)

(setq org-html-table-row-tags
      (cons '(cond (top-row-p "<tr class=\"tr-top\">")
                   (bottom-row-p "<tr class=\"tr-bottom\">")
                   (t (if (= (mod row-number 2) 1)
			  "<tr class=\"tr-odd\">"
			"<tr class=\"tr-even\">")))
	    "</tr>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-org.el ends here
