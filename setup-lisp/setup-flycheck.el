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


;; Most basic way: flycheck errors in minibuffer (works in consoles).
;; (unless (display-graphic-p (selected-frame))
;;   (with-eval-after-load 'flycheck
;;     (setq flycheck-display-errors-function 'flycheck-display-error-messages)
;;     (setq-default flycheck-display-errors-function 'flycheck-display-error-messages)))

;; for convenience, to turn off inline-mode:
;; (flycheck-inline-mode -1)
;; (setq-default flycheck-display-errors-function 'flycheck-display-error-messages)

;; (when (display-graphic-p (selected-frame))
;;   (eval-after-load 'flycheck
;;     '(progn
;;        ;; flycheck errors on a tooltip (doesn't work in consoles).
;;        (flycheck-pos-tip-mode)
;;        ;; See https://github.com/flycheck/flycheck-pos-tip/issues/6
;;        (add-hook 'post-command-hook 'flycheck-pos-tip-hide-messages)
;;        (custom-set-variables
;;         '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
;;        )))

;; Below from https://github.com/magnars/.emacs.d/blob/master/settings/setup-flycheck.el:
(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.4 2.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change.
This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-temp-prefix ".flycheck")))

(provide 'setup-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-flycheck.el ends here
