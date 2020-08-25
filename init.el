;;; init.el --- Entry point into configuration. -*- no-byte-compile: t -*-
;;
;; Filename: init.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sun Nov 12 11:16:22 2017 (-0800)
;; Version:
;; Package-Requires: ((emacs "24.3"))
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

;; (setq debug-on-error t)
(defun init ()
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

  ;; Never save cursor-type.
  (push '(cursor-type . :never) frameset-filter-alist)

  ;; fullscreen frame settings -- depends on screen size, though
  ;; '(default-frame-alist
  ;;     '((width . 254)
  ;;      (height . 72)
  ;;      (top . 0)
  ;;      (left . 0)
  ;;      (fullscreen . fullboth)))

  ;; This finds and sets up autoloads.
  (when (< emacs-major-version 27)
    (setq package--init-file-ensured nil)
    (package-initialize))

  ;; Set file containing machine-local customized settings.
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))

  (defvar site-lisp-dir
    (expand-file-name "elisp" user-emacs-directory))

  (defvar setup-lisp-dir
    (expand-file-name "setup-lisp" user-emacs-directory))

  (add-to-list 'load-path site-lisp-dir t)
  (add-to-list 'load-path setup-lisp-dir t)

  ;; Add all subdirs of site-lisp-dir.
  ;; TODO this is problematic b/c so many subdirs in emacs-libvterm
  ;; https://www.emacswiki.org/emacs/LoadPath
  (let ((default-directory site-lisp-dir))
    (normal-top-level-add-subdirs-to-load-path))

  (add-hook 'after-init-hook
            (lambda ()
              (message "after-init-hook after %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "startup-hook after %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

  )

(defvar wjb/gc-cons-threshold (* 2 800000))
(defvar wjb/gc-timer)
(setq garbage-collection-messages nil
      load-prefer-newer t)

(let
    ((file-name-handler-alist nil)
     (gc-cons-threshold most-positive-fixnum)
     (garbage-collection-messages t))

  (message "pre-init after %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)

  (init)
  (message "init done after %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)

  (require 'main)
  (message "main done after %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)

  ;; ========================================
  ;; Machine-local custom configuration.
  ;; ========================================

  (load custom-file t t)
  (message "custom-file done after %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done)

  ;; This would result in a big GC after init finishes, right when I want to
  ;; start using Emacs. Instead, give init a while to run, then schedule gc to
  ;; run once after some amount of idle time, then when it finishes, reset the
  ;; threshold to a reasonable value. The key to this is that when I start
  ;; Emacs, the idle timer starts counting, and I usually don't touch it until
  ;; init is done, by which time the idle timer is going to go off.
  (add-hook 'post-gc-hook (lambda ()
                            (when (fboundp 'wjb/gc-timer)
                              (when (timerp 'wjb/gc-timer)
                                (cancel-timer 'wjb/gc-timer))
                              (makunbound 'wjb/gc-timer)
                              (setq post-gc-hook nil))))
  (run-with-timer
   10 nil (lambda ()
            (message "Initial timer done. Preparing to run gc.")
            (setq wjb/gc-timer
                  (run-with-idle-timer
                   5 nil (lambda ()
                           (message "Garbage collecting while idle.")
                           (garbage-collect)
                            (message "First gc done. Resetting gc-cons-threshold.")
                           ;; see https://www.reddit.com/r/emacs/comments/bqu69o/making_emacs_snappier_i_need_a_second_opinion/
                           ;; This might be messing with Zoom screen sharing!
                           ;; (add-hook 'focus-out-hook #'garbage-collect t)
                           (setq gc-cons-threshold wjb/gc-cons-threshold)))))))

(put 'list-timers 'disabled nil)

(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
