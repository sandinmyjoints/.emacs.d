;;; init.el --- Entry point into Emacs configuration.
;;
;; Filename: init.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sun Nov 12 11:16:22 2017 (-0800)
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

(defun init ()
  (setq package-enable-at-startup nil)
  (setq package--init-file-ensured nil)
  ;; TODO: Goal is to run this after main.
  (package-initialize)

  ;; Set file containing machine-local customized settings.
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))

  (defvar site-lisp-dir
        (expand-file-name "elisp" user-emacs-directory))

  (defvar more-lisp-dir
        (expand-file-name "setup-lisp" user-emacs-directory))

  (add-to-list 'load-path site-lisp-dir t)
  (add-to-list 'load-path more-lisp-dir t)

  ;; Add all subdirs of site-lisp-dir.
  (let ((default-directory site-lisp-dir))
    (normal-top-level-add-subdirs-to-load-path))

  (require 'main))

(let
    ((file-name-handler-alist nil)
     (gc-cons-threshold most-positive-fixnum))
  (init))

;;(do-setup-dirtree)
(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
