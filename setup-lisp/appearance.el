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

;; Don't use these graphical elements.
;;
(if (display-graphic-p)
    (progn
      (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
      (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
      (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
      (if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode 1))

;; Set to always be fullscreen.
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Fonts.
;;
;; This makes the font the default on all graphical frames.
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-13"))

;; list all known fonts:
;; (font-family-list)
;;
;; Examine font of char at point: C-u C-x =
;;
;; Too wide!
;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)
;;
;; Too thick!
;; (set-face-attribute 'default nil :family "Menlo" :height 140)
;;
;; Just right.
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 130)
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140 :weight 'normal)
;;
;; http://typeof.net/Iosevka/
;; (set-face-attribute 'default nil :family "Iosevka" :height 144 :weight 'light)

;; set a fallback
(set-fontset-font t nil "Courier New" nil 'append)

(if (functionp 'set-fontset-font) ; nil in Terminal
    (set-fontset-font "fontset-default" 'unicode "Menlo"))

(setq-default line-spacing 2)

;; TODO: leave some blank space at right on large monitors
;; (set-window-margins nil 0 4)

(use-package auto-dim-other-buffers
  :defer 1
  :diminish auto-dim-other-buffers-mode
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-mode t))

;; Themes. Goal is to have one dark and one light theme that both work well, and
;; also have matching themes for Terminal.app.
;; - Dark: nimbus or gruvbox-dark-hard
;; - Light: gruvbox-light-hard / Novel in Terminal.app.
;;
;; themes I like:
;; 1. afternoon (change-theme 'afternoon)
;; 2. ample (change-theme 'ample)
;; 3. gruvbox (change-theme 'gruvbox)
;;    - Problem with gruvbox is its keyword face is red and builtin face is
;;       orange. They look like errors.
;; 4. Haven't tried it but https://github.com/arcticicestudio/nord works across multiple
;; applications.
;; 5. nimbus (use-package nimbus-theme)
;;
;; Themes to try:
;; - https://github.com/mswift42/reykjavik-theme
;;
(use-package gruvbox-theme ;; dark
  :defer 1
  :disabled
  :config
  (setq wjb/dark t)
  (change-theme 'gruvbox-dark-hard t)
  (wjb/gruvbox-dark)
  (wjb/turn-on-hl-line)
  (wjb/custom-appearance))

(use-package gruvbox-theme ;; light
  :defer 1
  :disabled
  :config
  (setq wjb/dark nil)
  (change-theme 'gruvbox-light-hard t)
  (wjb/gruvbox-light)
  (global-hl-line-mode -1)
  (wjb/custom-appearance))

(use-package nimbus-theme
  :defer 1
  :config
  (setq wjb/dark t)
  (change-theme 'nimbus)
  (wjb/turn-on-hl-line)
  (wjb/custom-appearance))

;; Nice theme but not updated since 2014. Enabling it produces a warning;
;; https://stackoverflow.com/a/1322978/599258 might help with debugging it.
(use-package afternoon-theme
  :defer 1
  :disabled
  :config
  (change-theme 'afternoon)
  (wjb/turn-on-hl-line)
  (wjb/custom-appearance))

(use-package ample-theme
  :defer 1
  :disabled
  :init
  (load-theme 'ample t t)
  (load-theme 'ample-flat t t)
  (load-theme 'ample-light t t)
  :config
  (setq wjb/dark t)
  ;; (change-theme 'ample)
  (change-theme 'ample-flat)
  (wjb/turn-on-hl-line)

  ;; (setq wjb/dark nil)
  ;; (change-theme 'ample-light)
  ;; (global-hl-line-mode -1)

  (wjb/custom-appearance))

;; TODO: try counsel-load-theme
;; See http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapc #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

(defvar wjb/dark t)
(defvar wjb/dark-cursor-color "#30F0F0")
(defvar wjb/light-cursor-color "purple")

(defun wjb/gruvbox-dark ()
  ;; instead of red:
  (set-face-foreground 'font-lock-keyword-face "#a8a8a8")

  ;; raise region contrast:
  (set-face-background 'region "#2d3d45")

  ;; to make background true black:
  ;; (set-face-background 'default "#000")

  (set-face-attribute 'markdown-code-face nil :family "DejaVu Sans Mono" :height 130)
  ;; (set-face-background 'markdown-code-face "#000")
  )

(defun wjb/gruvbox-light ()
  ;; (set-face-foreground 'font-lock-keyword-face "#a8a8a8")
  ;; (set-face-background 'default "#000")
  ;; (set-cursor-color wjb/dark-cursor-color)
  ;; (set-face-background 'region "#2d3d45")
  )

(declare-function color-lighten-name "colors.el")

(defun wjb/set-hl-line-bg ()
  "Customize background color of highlighted line."
  ;; very dark.
  ;;(set-face-background 'hl-line "#1A1A1A")

  ;; pretty dark.
  ;; (set-face-background 'hl-line "#202020")

  ;; Set it lighter relative to current background.
  (set-face-background 'hl-line
                       (color-darken-name
                        (face-attribute 'default :background) 4)))

(defun wjb/turn-on-hl-line ()
  "Turn on highlighted line."
  (use-package hl-line)
  (wjb/set-hl-line-bg)
  (global-hl-line-mode 1))

(defvar wjb/initial-mouse-color (cdr (assq 'mouse-color (frame-parameters))))

(defun wjb/custom-appearance ()
  (if wjb/dark
      (progn
        (set-mouse-color "light gray")
        (set-cursor-color wjb/dark-cursor-color)
        (set-face-foreground 'org-checkbox-done-text
                             (color-darken-name
                              (face-attribute 'default :foreground) 20)))
    (progn
      (set-mouse-color "black")
      (set-cursor-color wjb/light-cursor-color)
        (set-face-foreground 'org-checkbox-done-text
                             (color-lighten-name
                              (face-attribute 'default :foreground) 20))))

  ;; (set-face-attribute 'markdown-code-face nil :family "DejaVu Sans Mono" :height 130)

  ;; For themes that don't have adob faces defined (ample):
  ;; (set-face-background 'auto-dim-other-buffers-face "#181818")
  ;; (set-face-background 'auto-dim-other-buffers-face
  ;;                      (color-lighten-name
  ;;                       (face-attribute 'default :background) 5))
  )

;; Change cursor color according to mode.
;; From https://www.emacswiki.org/emacs/ChangingCursorDynamically
(defvar wjb/set-cursor-color-color "")
(defvar wjb/set-cursor-color-buffer "")
(defun wjb/set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only (if wjb/dark "white" "#116")
           (if overwrite-mode "red"
             wjb/dark-cursor-color))))
    (unless (and
             (string= color wjb/set-cursor-color-color)
             (string= (buffer-name) wjb/set-cursor-color-buffer))
      (set-cursor-color (setq wjb/set-cursor-color-color color))
      (setq wjb/set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'wjb/set-cursor-color-according-to-mode)
(add-hook 'after-init-hook 'wjb/set-cursor-color-according-to-mode)

;; Basic colors, if not using theme:
;;
;; (set-foreground-color "white")
;; (set-background-color "black")
;; (set-face-foreground 'default "white")
;; (set-face-background 'default "black")
;; (set-face-foreground 'region "gray60")
;; (set-face-background 'region "#464740")
;; (set-face-foreground 'font-lock-warning-face "#ff6666")
;; (set-face-foreground 'font-lock-comment-face "tan1")

(provide 'appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance.el ends here
