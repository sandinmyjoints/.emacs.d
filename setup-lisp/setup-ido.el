;;; setup-ido.el --- Configure ido-mode.
;;
;; Filename: setup-ido.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Oct  1 09:26:00 2014 (-0700)
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


;; Ido.
;; TODO: ido + flx vs. ivy vs. helm
;; - https://www.reddit.com/r/emacs/comments/6na75b/ido_versus_ivy_questions_of_a_neophyte/
;; - https://www.reddit.com/r/emacs/comments/331gqp/introducing_ivymode_or_emacs/
;; - https://oremacs.com/2015/04/16/ivy-mode/
(when (require 'ido nil t)
  (require 'flx-ido)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil)

  (ido-mode t)

  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        confirm-nonexistent-file-or-buffer nil
        ido-auto-merge-work-directories-length -1)

  (ido-everywhere t)

  (require 'ido-completing-read+)
  (ido-ubiquitous-mode 1)

  ;; (add-to-list 'ido-ubiquitous-command-overrides '(disable exact "rgrep"))

  (add-to-list 'ido-ignore-directories "node_modules")
  (add-to-list 'ido-ignore-buffers "*Ibuffer*")
  (add-to-list 'ido-ignore-files "\\.DS_Store")

  ;; TODO: extensions order, ignore
  ;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

  (setq flx-ido-threshold 8192)

  ;; Ido keymap.
  ;; TODO: want this to work with magit completions
  (defun wjb-ido-keys ()
    "Add my keybindings for ido."
    (define-key ido-completion-map
      (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map
      (kbd "C-p") 'ido-prev-match))

  (add-hook 'ido-setup-hook 'wjb-ido-keys))


(provide 'setup-ido)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-ido.el ends here
