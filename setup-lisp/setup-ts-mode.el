;;; setup-ts-mode.el ---
;;
;; Filename: setup-ts-mode.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Fri Sep 26 12:44:25 2025 (-0400)
;; Version:
;; Package-Requires: ((emacs "29.1"))
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

;; TODO convert to use-package :bind.
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-base-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-js))

;; always nil!
;; (with-eval-after-load 'tsx-ts-mode (message "loaded"))
;; (with-eval-after-load 'typescript-ts-base-mode (message "loaded"))

(defun wjb/ts-mode-hook ()
  ;; ;; these really only need to be run once, but with-eval-after-load doesn't run
  ;; ;; for tsx-ts-mode or typescript-ts-base-mode, so I'll put them into this hook.
  ;; (define-key typescript-ts-base-mode-map (kbd "C-c C-y") 'wjb-toggle-it-only-js)
  (when wjb/using-company
    (setq company-backends wjb/company-backends-ts)))

(add-hook 'typescript-ts-base-mode-hook #'wjb/ts-mode-hook)

(provide 'setup-ts-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-ts-mode.el ends here
