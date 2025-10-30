;;; early-init.el ---
;;
;; Filename: early-init.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Sat Jan  2 09:40:49 2021 (-0500)
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

;; from https://github.com/d12frosted/homebrew-emacs-plus/issues/323#issuecomment-2762214714
(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))

    (when brew-bin
      ;; Get gcc paths.
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)

        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))

      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))

    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))

    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))

;; Set up library paths for native compilation on macOS.
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths))

;; Precompute activation actions to speed up startup.
;; (setq package-quickstart t)

;; When non-nil (default), packages are available within startup files. When
;; nil, they are not, (package-activate-all) or (package-initialize) must be
;; called manually.
;;
;; (setq package-enable-at-startup nil)

(setq load-prefer-newer t)

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
                            ;; (alpha . 90)
                            (alpha-background . 85)))

;; Setting this to nil means the initial frame will use default-frame-alist.
(setq initial-frame-alist nil)

;; Make this frame, the initial frame, fullscreen.
;; TODO(emacs-mac): Disabling since the non-emacs-macport version has to be on another space to be fullscreen.
;; (set-frame-parameter nil 'fullscreen 'fullboth)

;; Transparency.
;; (active . inactive), ie, (focused . blurred)
;; (set-frame-parameter nil 'alpha '(100 . 60))
;; (set-frame-parameter nil 'alpha-background 85)
;; (modify-frame-parameters nil '((alpha-background . 75)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
