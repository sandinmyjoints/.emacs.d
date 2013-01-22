(setq visible-bell nil
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(set-face-background 'region "#464740")

;; Highlight current line
;(global-hl-line-mode 1)

;; Customize background color of highlighted line
;(set-face-background 'hl-line "#222222")

(set-face-foreground 'font-lock-warning-face "#ff6666")

;; Highlight matching parentheses when the point is on them.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode 1))

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; TODO: Use (null window-system) to conditionally execute.
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)
(if (functionp 'set-fontset-font) ; nil in Terminal
    (set-fontset-font "fontset-default" 'unicode "Anonymous"))
(setq-default line-spacing 1)

(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "#444")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "#469")

;; Nice sizing.  See:
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
;; TODO: Replace window-system (dep) with display-graphic-p. See:
;; http://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 140))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 200)
                                      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(defadvice split-window-vertically
    (after my-window-splitting-advice first () activate)
    (set-window-buffer (next-window) (other-buffer)))


(provide 'appearance)
