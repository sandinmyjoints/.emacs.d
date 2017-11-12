;;; setup-mac.el --- Customizations for OS X.
;;
;; Filename: setup-mac.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Fri Jan  2 17:59:40 2015 (-0800)
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

;; The mac- prefix is specific to the Mac port. Cocoa Emacs uses variables
;; starting with ns-.
;;
;; Keyboard for Macs.
(setq-default mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;; With Seil, I can map capslock to arbitrary keys, such as fn (keycode 63) or
;; L-option (keycode 58). Then I can set mac-option-modifier or
;; mac-function-modifier to super or hyper, and capslock will do super or hyper.
;; But it's tradeoff: I either lose the ability to use fn keys or to use option
;; to insert OS X extended characters.
;;
;; For now, I'm not modifying fn so I can use it regularly. I'm using
;; right-command as hyper. I use hyper a lot of smartparens bindings.
(setq mac-option-modifier nil)
(setq mac-function-modifier nil)
(setq mac-right-command-modifier 'hyper)

(require-package 'exec-path-from-shell)
;(add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
(exec-path-from-shell-initialize)

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; Toggle fullscreen.
(global-set-key (quote [M-f10]) (quote toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame.
;; Only works on Cocoa Emacs.
(setq ns-pop-up-frames nil)

;; Darwin/OS X ls doesn't support --dired out of the box.
;; First option: emulate ls via ls-lisp.
;(setq ls-lisp-use-insert-directory-program nil)
;(require 'ls-lisp)
;; Second options: if GNU coreutils is installed, use gls.
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")

(require 'osx-plist nil t)

(provide 'setup-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-mac.el ends here
