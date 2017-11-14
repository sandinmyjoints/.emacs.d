;;; setup-modeline.el --- Set up the modeline.
;;
;; Filename: setup-modeline.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sun Jun 21 21:26:25 2015 (-0700)
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

;; Original value was
;; (setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;        (vc-mode vc-mode)
;;        "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; Helpful reading:
;; - https://github.com/lunaryorn/blog/blob/master/posts/make-your-emacs-mode-line-more-useful.md
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html

(setq-default mode-line-format
      (list  '(
               ;; Leave these in front.
               "%e"
               "%n"
               mode-line-front-space

               " "
               (:exec venv-current-name)
               " "
               (:eval (car nvm-current-version))

               mode-line-mule-info
               mode-line-client
               mode-line-modified
               mode-line-remote
               mode-line-frame-identification
               mode-line-buffer-identification
               sml/pos-id-separator
               mode-line-position
               (vc-mode vc-mode)
               sml/pre-modes-separator
               mode-line-modes
               mode-line-misc-info
               mode-line-end-spaces)))

(add-to-list 'sml/replacer-regexp-list '("local_notes" ":LN:") t)

(provide 'setup-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-modeline.el ends here
