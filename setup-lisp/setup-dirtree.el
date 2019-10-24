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
;; M-x M-x (twice) to reload smex commands.
;; TODO: I think this defun is probably the source of the problems with dirtree at startup.
(defun wjb/setup-dirtree ()
  (interactive)
  (when (and (require 'tree-mode nil t) (require 'dirtree nil t))
    (let ((dirtree-buffer "*dirtree*"))
      (dolist (dir initial-dirs-to-open)
        (when (file-accessible-directory-p dir)
          (dirtree dir dirtree-buffer)))
      ;; Dedicate window and resize.
      (let ((window (get-buffer-window dirtree-buffer)))
        (set-window-fringes window 0 0 nil)
        (set-window-dedicated-p window t)
        ;; TODO: Resize more intelligently. Ideally, dirtree should be:
        ;; generally: 20% of window width
        ;; minimum: 30 columns or so
        ;; (with-current-buffer "*dirtree*"
        ;;   (setq-local window-size-fixed nil))
        (adjust-window-trailing-edge window -5 t)
        ;; (with-current-buffer "*dirtree*"
        ;;   (setq-local window-size-fixed t))
        (cd "~/scm/sd")))
    (with-current-buffer "*dirtree*"
      (read-only-mode))
    ))

(autoload 'dirtree "dirtree" "Add directory to tree view")
(add-hook 'after-init-hook #'wjb/setup-dirtree)

(defalias 'wjb/dirtree #'setup-dirtree)

(add-hook 'dirtree-mode-hook 'hidden-mode-line-mode)

(provide 'setup-dirtree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-dirtree.el ends here
