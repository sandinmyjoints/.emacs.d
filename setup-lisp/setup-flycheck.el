;;; setup-flycheck.el ---
;;
;; Filename: setup-flycheck.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Oct  8 16:45:35 2014 (-0700)
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

;; Let's only turn on flycheck manually.
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

;; flycheck errors on a tooltip (doesnt work on console)
(when (display-graphic-p (selected-frame))
  (eval-after-load 'flycheck
    '(progn
       (flycheck-pos-tip-mode)
       ;; (custom-set-variables
       ;;  '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
       ;;(setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
       ;; disable jshint since we prefer eslint checking
       (append flycheck-disabled-checkers '(javascript-jshint html-tidy))
       ;; use eslint with web-mode for jsx files
       (flycheck-add-mode 'javascript-eslint 'web-mode)
       )))

(setq flycheck-display-errors-function #'flycheck-display-error-messages
      flycheck-temp-prefix ".flycheck")
;;(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages) ;; tooltip

(provide 'setup-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-flycheck.el ends here
