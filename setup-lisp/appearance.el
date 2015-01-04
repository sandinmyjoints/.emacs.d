;; Note: Use list-faces-display to examine all faces.

;; Turn on/off display stuff.
;;
(setq visible-bell nil
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

;; Don't use these graphical elements.
;;
(if (display-graphic-p)
    (progn
      (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
      (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
      (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))

;; Settings.
;;
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode 1))

;; Highlight matching parentheses when point is on them.
;;
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Fonts.
;;
;; TODO: Use (null window-system) or (display-graphic-p)) to conditionally
;; execute.
;;
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)
(if (functionp 'set-fontset-font) ; nil in Terminal
    (set-fontset-font "fontset-default" 'unicode "Anonymous"))

(setq-default line-spacing 1)

;; Colors.
;;
(set-foreground-color "white")
(set-background-color "black")
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(set-face-foreground 'region "gray60")
(set-face-background 'region "#464740")
(set-face-foreground 'font-lock-warning-face "#ff6666")
(set-cursor-color "#CCD")

;; Set to always be fullscreen.
(set-frame-parameter nil 'fullscreen 'fullboth)

(defadvice split-window-vertically
    (after my-window-splitting-advice first () activate)
    (set-window-buffer (next-window) (other-buffer)))

;; TODO investigate for putting a left margin on dirtree window in fullscreen
;; mode. See:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Margins.html
;; Probably just need a hook that runs margin-x.
;(add-hook 'window-configuration-change-hook
;          (lambda ()
;            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 8 0)))

;; Highlight current line
;(global-hl-line-mode 1)

;; Customize background color of highlighted line
;(set-face-background 'hl-line "#1A1A1A")

(provide 'appearance)
