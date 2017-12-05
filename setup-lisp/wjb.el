;;; wjb.el ---
;;
;; Filename: wjb.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sun Nov 12 12:59:26 2017 (-0800)
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

;; Preload registers.
(set-register ?t "TODO ")
(set-register ?h "TODO HERE: ")

;; Set custom markers.
;; Args:
;; 1. Marker.
;; 2. Register to store.
;; 3. Key bindings to set/clear marker.
;; 4. Insert/remove marker from current buffer?
;;
(defvar wjb-custom-markers
      '(("NNN" ?n "" t)
        ;("MMM" ?m "" t)
        ("Server" ?s "" nil)
        ("Quiz View" ?q "" nil)
        ("Client" ?c "" nil)))

(defvar wjb-test-config-buffer "test.coffee")

;; Set shortcuts to clear custom markers. Requires lexical binding.
(dolist (marker-data wjb-custom-markers)
        (let ((marker (pop marker-data))
              (marker-register (pop marker-data))
              (marker-key (pop marker-data))
              (handle-in-current-buffer (pop marker-data)))
          (progn
            (set-register marker-register marker)
            (global-set-key marker-key (lambda (arg)
                                          (interactive "P")
                                          (wjb-toggle-marker arg marker handle-in-current-buffer))))))

;; Text and fill modes.
(defun wjb-textful-settings ()
  "Textful settings."
  (goto-address-mode 1)
  (auto-fill-mode 1)
  (set-fill-column 80))

(add-hook 'markdown-mode-hook 'wjb-textful-settings)
(add-hook 'rst-mode-hook 'wjb-textful-settings)
(add-hook 'text-mode-hook 'wjb-textful-settings)

(provide 'wjb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wjb.el ends here
