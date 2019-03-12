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
;; This makes the font the default on all graphical frames.
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-14"))

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
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140 :weight 'normal)
;;
;; http://typeof.net/Iosevka/
;; (set-face-attribute 'default nil :family "Iosevka" :height 144 :weight 'light)

(if (functionp 'set-fontset-font) ; nil in Terminal
    (set-fontset-font "fontset-default" 'unicode "Menlo"))

(setq-default line-spacing 2)

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-mode t)
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

(defvar wjb/default-cursor-color "#30F0F0")
(defun wjb/customize-gruvbox ()
  (interactive)
  ;; ...but with keywords gray instead of red.
  (set-face-foreground 'font-lock-keyword-face "#a8a8a8")
  ;; ...but with face-background set to near black
  (set-face-background 'default "#000")
  (set-cursor-color wjb/default-cursor-color)
  ;; #504945
  (set-face-background 'region "#2d3d45")
  )

(defalias 'wjb-theme #'wjb/customize-gruvbox)

(defun wjb/turn-on-hl-line ()
  ;; Highlight current line
  (global-hl-line-mode 1)
  ;; Customize background color of highlighted line.

  ;; very dark.
  ;;(set-face-background 'hl-line "#1A1A1A")

  ;; pretty dark.
  ;; (set-face-background 'hl-line "#202020")

  ;; lighter relative to current background
  (set-face-background 'hl-line
                       (color-lighten-name
                        (face-attribute 'default :background) 10)))

;; See http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
;; Commenting out; prefer change-theme (see below).
;; (defadvice load-theme (before theme-dont-propagate activate)
;;   (mapcar #'disable-theme custom-enabled-themes))

(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

;; (change-theme 'gruvbox)

;; Themes.
;;
;; Themes I like:
;; 1. afternoon (change-theme 'afternoon)
;; 2. ample (change-theme 'ample)
;; 3. gruvbox (change-theme 'gruvbox)
;;    - Problem with gruvbox is its keyword face is red and builtin face is
;;       orange. They look like errors.
;; 4. Haven't tried it but https://github.com/arcticicestudio/nord works across multiple
;; applications.
;; 5. nimbus (use-package nimbus-theme)
;;
;; TODO: Just a guess, but you probably have to do something like:

;; (use-package spacemacs-common :ensure 'spacemacs-theme :config (load-theme 'spacemacs-dark))
;; from https://www.reddit.com/r/emacs/comments/9ik7ug/two_questions_regarding_usepackage/e6kc4nc/

(use-package gruvbox-theme
  :defer 1
  :disabled
  :config
  (change-theme 'gruvbox-dark-hard t)
  (set-face-background 'markdown-code-face "#000")
  (set-face-attribute 'markdown-code-face nil :family "DejaVu Sans Mono" :height 140)
  (wjb/customize-gruvbox)
  (wjb/turn-on-hl-line))

(use-package nimbus-theme
  :defer 1
  :config
  (set-face-attribute 'markdown-code-face nil :family "DejaVu Sans Mono" :height 140)
  (wjb/turn-on-hl-line))

;; Change cursor color according to mode.
;; From https://www.emacswiki.org/emacs/ChangingCursorDynamically
(defvar wjb/set-cursor-color-color "")
(defvar wjb/set-cursor-color-buffer "")
(defun wjb/set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "white"
           (if overwrite-mode "red"
             wjb/default-cursor-color))))
    (unless (and
             (string= color wjb/set-cursor-color-color)
             (string= (buffer-name) wjb/set-cursor-color-buffer))
      (set-cursor-color (setq wjb/set-cursor-color-color color))
      (setq wjb/set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'wjb/set-cursor-color-according-to-mode)

(provide 'appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance.el ends here
