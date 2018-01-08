;;; appearance.el ---
;;
;; Filename: appearance.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sun Nov 12 19:50:29 2017 (-0800)
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


;; Note: Use (list-faces-display) to examine all faces.

;; Fonts.
;;
;; TODO: Use (null window-system) or (display-graphic-p)) to conditionally
;; execute.
;;
;; Too wide!
;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)
;;
;; Too thick!
;; (set-face-attribute 'default nil :family "Menlo" :height 140)
;;
;; Just right.
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)

(if (functionp 'set-fontset-font) ; nil in Terminal
    (set-fontset-font "fontset-default" 'unicode "Menlo"))

(setq-default line-spacing 2)

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil)
  (auto-dim-other-buffers-mode t)
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (set-face-background 'auto-dim-other-buffers-face "#181818"))

;; Colors.
;;
;; Uncomment if not using theme:
;;
;; (set-foreground-color "white")
;; (set-background-color "black")
;; (set-face-foreground 'default "white")
;; (set-face-background 'default "black")
;; (set-face-foreground 'region "gray60")
;; (set-face-background 'region "#464740")
;; (set-face-foreground 'font-lock-warning-face "#ff6666")
;; (set-face-foreground 'font-lock-comment-face "tan1")

(defun wjb/theme ()
  (interactive)
  (message "running wjb-theme")
  ;; ...but with keywords gray instead of red.
  (set-face-foreground 'font-lock-keyword-face "#a8a8a8")
  ;; ...but with face-background set to near black
  (set-face-background 'default "#000")
  (set-cursor-color "#30F0F0")
  ;; #504945
  (set-face-background 'region "#2d3d45")

  ;; Highlight current line
  (global-hl-line-mode 1)
  ;; Customize background color of highlighted line
  ;;(set-face-background 'hl-line "#1A1A1A")
  (set-face-background 'hl-line "#202020"))

(defalias 'wjb-theme #'wjb/theme)

;; Themes.
;;
;; Themes I like:
;; 1. afternoon (load-theme 'afternoon)
;; 2. ample (load-theme 'ample)
;; 3. gruvbox (load-theme 'gruvbox)
;;    - Problem with gruvbox is its keyword face is red and builtin face is
;;       orange. They look like errors.
;; 4. Haven't tried it but https://github.com/arcticicestudio/nord works across multiple
;; applications.
;;
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t)
  (set-face-background 'markdown-code-face "#000")
  (set-face-attribute 'markdown-code-face nil :family "DejaVu Sans Mono" :height 140)
  (wjb-theme))

(provide 'appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance.el ends here
