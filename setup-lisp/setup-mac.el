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
(setq-default mac-command-key-is-meta t
              mac-command-modifier 'meta

              ;; With Seil, I can map capslock to arbitrary keys, such as fn
              ;; (keycode 63) or L-option (keycode 58). Then I can set
              ;; mac-option-modifier or mac-function-modifier to super or hyper,
              ;; and capslock will do super or hyper. But it's tradeoff: I
              ;; either lose the ability to use fn keys or to use option to
              ;; insert OS X extended characters.
              ;;
              ;; For now, I'm not modifying fn so I can use it regularly. I'm using
              ;; right-command as hyper.
              mac-option-modifier nil
              mac-function-modifier nil
              mac-right-command-modifier 'hyper)

;; An alternative is, https://github.com/arouanet/path-helper,
;; however, it has this limitation:
;;
;; "An obvious downside of this approach is that if the PATH is manually set
;; elsewhere, such as in the user .profile file, it will not be visible to
;; path-helper. But properly configured macOS packages such as MacTeX, which
;; contribute to the PATH by adding a file in /etc/paths.d/, will work as
;; expected."
;;
(use-package exec-path-from-shell
  ;; :defer 2 ;; exec-path-from-shell-initialize takes a couple seconds to run.
  :demand ;; try if having problems
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG"))
    (add-to-list 'exec-path-from-shell-variables var))
  ;; Copy vars in exec-path-from-shell-variables.
  ;; https://emacs.stackexchange.com/a/553/2163
  ;;(add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash"
      locate-command "mdfind")

;; Don't open files from the workspace in a new frame.
;; Only works on Cocoa Emacs.
(defvar ns-pop-up-frames nil)

(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(use-package osx-plist
  :mode "\\.plist\\'")

(use-package reveal-in-osx-finder)

;; Allow editing of binary .plist files.
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;;It is necessary to perform an update!
(jka-compr-update)

(provide 'setup-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-mac.el ends here
