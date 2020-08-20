;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

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

;; Make this frame, the initial frame, fullscreen.
(set-frame-parameter nil 'fullscreen 'fullboth)
(set-frame-parameter nil 'alpha '(90 . 50))

;; (setq package-enable-at-startup nil)
