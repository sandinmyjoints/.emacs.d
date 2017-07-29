;; Note: Use (list-faces-display) to examine all faces.

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

(setq fci-rule-color "#333")

;; Themes.
;;
;; Themes I like:
;; 1. afternoon (load-theme 'afternoon)
;; 2. ample (load-theme 'ample)
;; 3. gruvbox (load-theme 'gruvbox)
;;    - Problem with gruvbox is its keyword face is red and builtin face is
;       orange. They look like errors.

(defun theme-it () ""
       (progn
	 (load-theme 'gruvbox-dark-hard)
   ;; ...but with keywords gray instead of red.
   (set-face-foreground 'font-lock-keyword-face "#a8a8a8")
	 ;; ...but with face-background set to near black
	 (set-face-background 'default "#020202")
	 ;;(set-face-background 'auto-dim-other-buffers-face "#202020") ;; is this making it slow to switch back to Emacs?
	 ))
;;(theme-it)

;; Highlight current line
(global-hl-line-mode 1)
;; Customize background color of highlighted line
;;(set-face-background 'hl-line "#1A1A1A")
;;(set-face-background 'hl-line "#202020")

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

(provide 'appearance)
