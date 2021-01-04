;;; early-init.el ---
;;
;; Filename: early-init.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Sat Jan  2 09:40:49 2021 (-0500)
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

(setq load-prefer-newer t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      package-quickstart t)

;; Initial and default settings. Should match these:
;; defaults write org.gnu.Emacs Width 120
;; defaults write org.gnu.Emacs Height 40
;; defaults write org.gnu.Emacs Top 40
;; defaults write org.gnu.Emacs Left 200
;;
(setq default-frame-alist '((width . 120)
                            (height . 40)
                            (top . 40)
                            (left . 200)
                            (line-spacing . 2)
                            (cursor-type . box)
                            (cursor-in-non-selected-windows . hollow)
                            ;; Menu and tool bar will be disabled but don't
                            ;; show them even before getting to the code that
                            ;; disables them.
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            (font . "Fira Code-15")
                            (alpha . 90)))

;; Setting this to nil means it will use default-frame-alist.
(setq initial-frame-alist nil)

;; Make this frame, the initial frame, fullscreen and add some transparency.
;; TODO(emacs-mac): Disabling since the non-emacs-macport version has to be on another space.
;; (set-frame-parameter nil 'fullscreen 'fullboth)
(set-frame-parameter nil 'alpha '(90 . 50))

;; (setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
