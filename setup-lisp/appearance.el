;; -*- rainbow-mode: t; -*-
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
;; How to change faces (get/set face attributes): https://emacs.stackexchange.com/questions/29183/how-to-get-read-face-attributes

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

;; Fonts.
;;
;; List all known fonts:
;; (font-family-list)
;;
;; Examine font of char at point: C-u C-x =
;;
;; Remove a font from the default frame alist:
;; (setq default-frame-alist (asoc-remove-keys (lambda (key) (equal key 'font)) default-frame-alist)
;;
;; Set default font, both for current frame and for all frames.
(set-face-font 'default "Fira Code-15")
(add-to-list 'default-frame-alist
             '(font . "Fira Code-15"))

;; (set-face-font 'default "Cascadia Code-14")
;; (add-to-list 'default-frame-alist
;;              '(font . "Cascadia Code-14"))

;; (set-face-font 'default "DejaVu Sans Mono-14")
;; (add-to-list 'default-frame-alist
;;              '(font . "DejaVu Sans Mono-14"))

;; Define fonts for specific Unicode ranges or blocks, or set a general fallback.
;; See: https://github.com/jletourneau/emacs.d/blob/97b0965d04255edab69f7a2f62a634bc1e755a51/include/_char_ranges.el
;; See: https://www.reddit.com/r/emacs/comments/8tz1r0/how_to_set_font_according_to_languages_that_i/
;;
;; Desired behavior: use my chosen font (based on theme, probably) for all the
;; characters it supports, use Noto Sans for anything else if it's installed,
;; else fall back to a mono system font like Courier.
;;
;; (cl-prettyprint (fontset-list))
;; ("-*-Fira Code-normal-normal-normal-*-15-*-*-*-m-0-fontset-auto3"
;;  "-*-Noto Sans-normal-normal-normal-*-14-*-*-*-p-0-fontset-auto2"
;;  "-*-Fira Code-normal-normal-normal-*-14-*-*-*-m-0-fontset-auto1"
;;  "-*-Monaco-normal-normal-normal-*-12-*-*-*-m-0-fontset-startup"
;;  "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default")

;; (describe-fontset "fonset-auto3")
;; (describe-fontset "fonset-startup")
;; (describe-fontset "fonset-standard")
;; (describe-fontset "fonset-default")

;; (call-interactively #'list-character-sets)
;; (fontset-info "fontset-default")
;;
;; (set-face-attribute 'default nil :family "Noto Sans" :height 140)
;; I would expect Noto Sans to be used for these, but it is not:
;; 。⺉
;; It is used for this:
;; ᪰ code point 1ab0
;;😊
;; (member "Symbola" (font-family-list))
;; (length (font-family-list))
;; (set-fontset-font t ?😊 "Segoe UI Emoji")
;; (when (functionp 'set-fontset-font) ; nil in Terminal
;;   (set-fontset-font "fontset-default" '(#x2e80 . #x2eff) "Noto Sans")
;;   (set-fontset-font "fontset-default" '(#x3000 . #x303f) "Noto Sans"))

;; Font heights
;; 140 Good for laptop
;; 150 Good for external monitor

;; ######## Font reviews ########
;;
;; Fira Code
;; - Looks especially good in light themes.
;; - Nice ligatures.
;;
;; https://github.com/tonsky/FiraCode/wiki
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
;;
;; (set-face-attribute 'default nil :family "Fira Code" :height 140)
;; (set-face-attribute 'default nil :family "Fira Code" :height 150)

;; Cascadia Code
;; - looks *great* with nimbus, eighties
;; - Nice ligatures
;;
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 140)
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 150)

;; DejaVu Sans Mono
;; - very good Unicode support
;; - no ligatures
;;
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 150 :weight 'normal)

;; Iosevka
;;
;; http://typeof.net/Iosevka/
;; https://github.com/be5invis/Iosevka/
;;
;; (set-face-attribute 'default nil :family "Iosevka" :height 150 :weight 'normal)

;; Anonymous Pro
;; - readable at very small sizes
;;
;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

;; Menlo
;; Too thick!
;; (set-face-attribute 'default nil :family "Menlo" :height 140)

;; More fonts to try:
;; - Input
;; - Hack
;; - Source Code Pro
;; - Noto (huge Unicode support)
;; - https://www.reddit.com/r/emacs/comments/cymay9/variable_pitch_fonts_for_programming/

;; TODO: leave some blank space at right (padding) on large monitors
;; (set-window-margins nil 0 4)

;; TODO
(use-package auto-dim-other-buffers
  :defer 1
  :disabled
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
;; 5. nimbus (use-package nimbus-theme) -- fantastic!
;; - tomorrow themes -- very nice!
;; (require 'color-theme-sanityinc-tomorrow)
;; light, icy M-x color-theme-sanityinc-tomorrow-day
;; dark M-x color-theme-sanityinc-tomorrow-night
;; dark M-x color-theme-sanityinc-tomorrow-blue
;; actually dark M-x color-theme-sanityinc-tomorrow-bright
;; dark, cool M-x color-theme-sanityinc-tomorrow-eighties
;; - wilmersdorf -- nice!
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "elisp/wilmersdorf-emacs-theme"))
;; (change-theme 'wilmersdorf)
;; (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal)
;; - tron-legacy -- interesting
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "elisp/tron-legacy-emacs-theme"))
;; (change-theme 'tron-legacy)
;; (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal)
;; - seti -- interesting
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "elisp/seti-theme"))
;; (change-theme 'seti)
;; - emacs-synthwave-theme -- too crazy
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "elisp/emacs-synthwave-theme"))
;; (change-theme 'synthwave)
;; (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal)
;; - kaolin themes
;; (change-theme 'kaolin-bubblegum)
;; (change-theme 'kaolin-temple)
;; (change-theme 'kaolin-valley-light)
;; - eziam (light)
;; X mucks with org-mode (change-theme 'eziam-light)
;; (change-theme 'eziam-dusk)
;; (change-theme 'eziam-dark)
;; - leuven (light)
;; (change-theme 'leuven)
;; - parchment (light)
;; X mucks with org-mode (change-theme 'parchment)
;; (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal)
;; - flatui
;; X bad for magit (change-theme 'flatui)
;; Themes to try:
;; - https://github.com/mswift42/reykjavik-theme
;;
;; modes with trouble in various themes: easy-kill is not themed by eighties

(use-package gruvbox-theme ;; dark
  :defer 1
  :disabled
  :config
  (setq wjb/dark t)
  (change-theme 'gruvbox-dark-hard t)
  (wjb/gruvbox-dark)
  (wjb/turn-on-hl-line)
  (wjb/custom-appearance))

;; gruvbox colors for slack: #F9F5D7,#F8F8FA,#61ACBB,#FFFFFF,#FFFFFF,#282828,#427B58,#9D0006
(defun wjb/light-theme ()
  (interactive)
  (setq wjb/dark nil)
  (change-theme 'gruvbox-light-soft t)
  ;; (change-theme 'gruvbox-light-medium t)
  (wjb/gruvbox-light)
  (wjb/turn-on-hl-line)
  (wjb/custom-appearance)
  (set-face-attribute 'default nil :family "Fira Code" :height 150)
  (set-frame-parameter nil 'alpha '(98 . 50))
  ;; region is #d5c4a1
  ;; easy-kill-selection inherits secondary-selection which is #ebdbb2
  ;; they are too close
  ;; make it #ebdbcc
  (set-face-background 'easy-kill-selection "#ebdbcc"))

(use-package gruvbox-theme ;; light
  :defer 1
  :disabled
  :config
  (call-interactively #'wjb/light-theme))

(defun wjb/dark-theme ()
  "Activate my dark theme."
  (interactive)
  (setq wjb/dark t)
  (change-theme 'nimbus)
  (set-face-attribute 'default nil :family "Cascadia Code" :height 150)
  (wjb/turn-on-hl-line)
  (set-frame-parameter nil 'alpha '(90 . 50))
  ;; (set-face-background 'default "#000")
  (wjb/custom-appearance))

(use-package nimbus-theme
  :defer 1
  :disabled
  :config
  (call-interactively #'wjb/dark-theme))

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

(defvar wjb/dark t "Non-nil when a dark theme is active.")
;; (setq wjb/dark t)
;; (setq wjb/dark nil)

;; https://github.com/morhetz/gruvbox
(defvar wjb/dark-cursor-color "#30F0F0") ;; #458588 #076678 #blue #0000FF #0766FF
(defvar wjb/light-cursor-color "#98FF1a") ;; #98971a #79740e #green #00FF00
(defvar wjb/read-only-cursor-dark "white")
(defvar wjb/read-only-cursor-light "#d65d0e") ;; "#116"

(defun wjb/gruvbox-dark ()
  ;; instead of red:
  (set-face-foreground 'font-lock-keyword-face "#a8a8a8")

  ;; raise region contrast:
  (set-face-background 'region "#2d3d45")

  ;; to make background true black:
  ;; (set-face-background 'default "#000")

  ;; (set-face-background 'markdown-code-face "#000")
  (set-face-attribute 'markdown-code-face nil :family "DejaVu Sans Mono" :height 130))

(defun wjb/gruvbox-light ()
  ;; (set-face-foreground 'font-lock-keyword-face "#a8a8a8")
  ;; (set-face-background 'default "#000")
  ;; (set-cursor-color wjb/dark-cursor-color)
  ;; (set-face-background 'region "#2d3d45")

  ;; gruvbox-light-hard doesn't define these.
  (set-face-background 'diredp-dir-priv nil)
  (set-face-background 'diredp-dir-priv nil)
  (set-face-background 'diredp-exec-priv nil)
  (set-face-background 'diredp-link-priv nil)
  (set-face-background 'diredp-read-priv nil)
  (set-face-background 'diredp-write-priv nil)
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

;; (global-hl-line-mode -1)
(defun wjb/turn-on-hl-line ()
  "Turn on highlighted line."
  (use-package hl-line
    :config
    (setq hl-line-sticky-flag nil))
  (wjb/set-hl-line-bg)
  (global-hl-line-mode 1))

(defvar wjb/initial-mouse-color (cdr (assq 'mouse-color (frame-parameters))))

(defun wjb/custom-appearance ()
  ;; some themes try to jack this
  (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal)

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
         (if buffer-read-only (if wjb/dark wjb/read-only-cursor-dark wjb/read-only-cursor-light)
           (if overwrite-mode "red"
             (if wjb/dark wjb/dark-cursor-color wjb/light-cursor-color)))))
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

;; transparency:
(defvar wjb/more-transparent 90)
(defvar wjb/less-transparent 98)

;; TODO transparency looks good enough with dark theme to always be one, but
;; not so good with light theme.
(defun wjb/focus-in-hook ()
  (if wjb/dark
      (set-frame-parameter (selected-frame) 'alpha wjb/more-transparent)
    (set-frame-parameter (selected-frame) 'alpha wjb/less-transparent)))

(defun wjb/focus-out-hook ()
  (set-frame-parameter (selected-frame) 'alpha wjb/more-transparent))

(add-hook 'focus-in-hook #'wjb/focus-in-hook)
(add-hook 'focus-out-hook #'wjb/focus-out-hook)

;; To make it default, you can add this:

;; (add-to-list 'default-frame-alist
;;              '(alpha . 84))

(provide 'appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance.el ends here
