;;; setup-ediff.el --- Setup ediff the way I like.
;;
;; Filename: setup-ediff.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Mon Aug 24 17:48:09 2015 (-0700)
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

;; Based on http://stackoverflow.com/a/1680994

(if (locate-library "ediff")
    (progn
      (autoload 'ediff-files "ediff")
      (autoload 'ediff-buffers "ediff")

      (eval-after-load "ediff" '(progn
                                  (setq diff-switches               "-c"
                                        ediff-custom-diff-options   "-c"
                                        ediff-split-window-function 'split-window-horizontally
                                        ediff-window-setup-function 'ediff-setup-windows-plain)

                                  (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
                                  (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
                                  (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)))))

(provide 'setup-ediff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-ediff.el ends here
