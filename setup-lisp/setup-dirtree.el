;;; setup-dirtree.el ---
;;
;; Filename: setup-dirtree.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sun Nov 12 13:46:54 2017 (-0800)
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

;; Directories to open in dirtree on start. TODO This should be in custom.el,
;; but probably need to move when dirtree starts up to happen following init.el
;; being processed because custom.el isn't loaded until the very end.
;; TODO: Would be nice to start these closed instead of expanded.
(defvar initial-dirs-to-open '())

;; Open up some dirs in dirtree if it's available.
(defun do-setup-dirtree ()
  (interactive)
  (message "running do-setup-dirtree")
  (when (and (require 'tree-mode nil t) (require 'dirtree nil t))
    (let ((dirtree-buffer "*dirtree*"))
      (dolist (dir initial-dirs-to-open)
        (when (file-accessible-directory-p dir)
          (dirtree dir dirtree-buffer)))
      ;; Dedicate window and resize.
      (let ((window (get-buffer-window dirtree-buffer)))
        (set-window-fringes window 0 0 nil)
        (set-window-dedicated-p window t)
        ;; TODO: Resize more intelligently.
        (adjust-window-trailing-edge window -5 t)
        (cd "~/scm/sd")))))

;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(add-hook 'dirtree-mode-hook 'hidden-mode-line-mode)

(autoload 'dirtree "dirtree" "Add directory to tree view")
(add-hook 'after-init-hook #'do-setup-dirtree)

(provide 'setup-dirtree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-dirtree.el ends here
