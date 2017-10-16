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

;; This turns on Flycheck globally in only these modes. Others can be turned on
;; per-buffer.
(setq flycheck-global-modes
      '(js2-mode
        rjsx-mode
        coffee-mode
        emacs-lisp-mode
        json-mode
        sh-mode
        yaml-mode
        python-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(progn
     (setq-default flycheck-disabled-checkers
                   (append '(javascript-jshint html-tidy python-pylint emacs-lisp-checkdoc) flycheck-disabled-checkers))
     (setq flycheck-display-errors-delay 0.8)
     (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
     (flycheck-status-emoji-mode 1)))

;; flycheck errors on a tooltip (doesnt work on console)
(when (display-graphic-p (selected-frame))
  (eval-after-load 'flycheck
    '(progn
       (flycheck-pos-tip-mode)
       ;; (custom-set-variables
       ;;  '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
       (flycheck-add-mode 'javascript-eslint 'web-mode))))

;; Below from https://github.com/magnars/.emacs.d/blob/master/settings/setup-flycheck.el:
(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 5.0)))

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
    ;; alternative is flycheck-display-error-messages
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    '(flycheck-temp-prefix ".flycheck")))

;; See https://github.com/flycheck/flycheck-pos-tip/issues/6
(add-hook 'post-command-hook 'flycheck-pos-tip-hide-messages)

(provide 'setup-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-flycheck.el ends here
