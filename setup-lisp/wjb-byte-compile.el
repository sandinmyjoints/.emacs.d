;; Defuns that get called a lot, so are byte-compiled.
;; eval this:
;; (emacs-lisp-byte-compile-and-load)

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

;; Keep region active when hit C-g. From http://emacs.stackexchange.com/a/11064
(defun my-keyboard-quit-advice (fn &rest args)
  (let ((region-was-active (region-active-p)))
    (unwind-protect
        (apply fn args)
      (when region-was-active
        (activate-mark t)))))

;; Below from https://github.com/magnars/.emacs.d/blob/master/settings/setup-flycheck.el:
(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.7 2.0)))

;; Based on:
;; - https://github.com/lewang/flx#gc-optimization
;; - http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold wjb/gc-cons-threshold))

(provide 'wjb-byte-compile)
