;;; setup-grep.el --- Set up grep.
;;
;; Filename: setup-grep.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Sat Jun 25 23:17:29 2016 (-0700)
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


;; Make grep-find more helpful.
;; TODO: dir-local list of paths to exclude from grep-find (e.g., .git and node_modules, dist, _tmp, minified files)
;; TODO: Document this better.
;; http://stackoverflow.com/a/2148754
;; comparison with ag: https://www.reddit.com/r/programming/comments/16bvah/the_silver_searcher_is_a_35x_faster_drop_in/
;; wjb-default-find-command
;; wjb-default-find-command
;;
;; -wholename  = -path
;; -path       = pathname
;; -name       = last part of pathname
;;
;; Consider ignoring:
;; *[-.]min[.-]*
;; *.gz.js
;;
;; Consider having two commands: one that ignores everything I might want
;; ignored, and one that ignores conservatively.
;;
;; Approaches:
;; - find | grep via grep-find
;; - grep -r by itself
;; - rg by itself
;; - rg with helm (via helm-ag)
;;
;; - search sane defaults in .
;; - search EVERYTHING in .
;; - search wildcard pattern I specify (consider subdirs) in .
;; - all of these in some other path
;; - 5 lines of context, jump to match line using compile buffer navigation
;;
;; inputs:
;; - path (default to .)
;; - file glob
;; - search pattern (regex)

(require 'grep)

;; Custom grep-find via find-in-project.

;; Setup find.
(defvar wjb-find-bin "find")
(defvar wjb-find-args "! -name \"TAGS\" ! -name \"*~\" ! -name \"#*#\" ! -wholename \"*/node_modules*\" ! -wholename \"*/.git*\" ! -wholename \"*local/Yarn*\" ! -wholename \"*/.storybook-static*\" -type f -print0 ")

(when (executable-find "gfind")
  (setq wjb-find-bin "gfind")
  (setq wjb-find-args "! -name \"TAGS\" ! -name \"*~\" ! -name \"#*#\" ! -path \"*/node_modules*\" ! -path \"*/.git*\" ! -path \"*local/Yarn*\" ! -path \"*/.storybook-static*\" ! -path \"*/_tmp*\" ! -path \"*/coverage*\" ! -path \"*/dist*\" -type f -print0"))

;; TODO: fd is often faster than GNU grep, but its arguments are "<pattern
;; path>", whereas grep is "<starting point> <expression>". So the the command
;; construction would need to account for this.
;;
;; (when (executable-find "fd")
;;   (setq wjb-find-bin "fd")
;;   (setq wjb-find-args "-E \"*local/Yarn*\" -E \"*/.storybook-static*\" -E \"*/_tmp*\" -E \"*/coverage*\" -E \"*/dist*\" --type f --print0"))

;; Setup grep.
(defvar wjb-grep-bin "grep")
(defvar wjb-grep-args "--line-buffered -E -C 5 -niH -e ")

(when (executable-find "ggrep")
  (setq wjb-grep-bin "ggrep"))

(when (executable-find "rg")
  (setq wjb-grep-bin "rg")
  (setq wjb-grep-args "-C 5 --no-heading -niH -e "))

(defvar wjb-grep-part (format "%s %s" wjb-grep-bin wjb-grep-args))
(defvar wjb-xargs-part "| xargs -0 -P 2 ")
(defvar wjb-after-the-pipe (concat (format "%s %s" wjb-xargs-part wjb-grep-part) "%s"))

(defvar wjb-find-part (format "%s . %s" wjb-find-bin wjb-find-args))
(defvar wjb-default-find-command
  (format "%s %s %s " wjb-find-part wjb-xargs-part wjb-grep-part))

;; Set it as the find command.
;; How to use grep-apply-setting: http://stackoverflow.com/a/25633595/599258
(grep-compute-defaults)
(grep-apply-setting 'grep-find-command wjb-default-find-command)

(defvar wjb-find-in-project-default-dir ".")
(defun find-in-project (path grep-string)
  "rgrep in current project dir."
  (interactive (list (read-directory-name "starting point: " wjb-find-in-project-default-dir)
                     (read-from-minibuffer "search for: ")))
  (let ((default-directory path))
    (grep-find (concat wjb-default-find-command grep-string))))

;; C-x 9 -> 9 = p reversed
(defun find-in-project-glob-by-path (path name-pattern grep-string)
  "find|xargs in current project dir by path."
  (interactive (list (read-directory-name "starting point: " wjb-find-in-project-default-dir)
                     (if (functionp #'ivy-read)
                         (ivy-read "path glob: " '() :require-match nil :initial-input "*")
                       (read-from-minibuffer "path glob: " "*"))
                     (if (functionp #'ivy-read)
                         (ivy-read "search for: " '() :require-match nil)
                       (read-from-minibuffer "search for: "))))
  (let* ((default-directory path)
         (wjb-path-or-iname (if (s-contains? "find" wjb-find-bin)
                                "-ipath"
                              ""))
         (command-template (concat wjb-find-bin " . " wjb-path-or-iname " '%s' "wjb-find-args wjb-after-the-pipe))
         (actual-command (format command-template name-pattern grep-string)))
    (grep-find actual-command)))

;; C-x j -> j close to n for name
(defun find-in-project-glob-by-name (path name-pattern grep-string prefix)
  "find|xargs in current project dir by name. Negate with prefix arg."
  (interactive (list (read-directory-name "starting point: " wjb-find-in-project-default-dir)
                     (if (functionp #'ivy-read)
                         (ivy-read "name glob: " '() :require-match nil :initial-input "*")
                       (read-from-minibuffer "name glob: " "*"))
                     (if (functionp #'ivy-read)
                         (ivy-read "search for: " '() :require-match nil)
                       (read-from-minibuffer "search for: "))
                     current-prefix-arg))
  (let* ((default-directory path)
         (wjb-path-or-iname (if (s-contains? "find" wjb-find-bin)
                                "-iname"
                              ""))
         (command-template (concat wjb-find-bin " . %s " wjb-path-or-iname " '%s' "  wjb-find-args wjb-after-the-pipe))
         (negate-or-not (if prefix "!" ""))
         (actual-command (format command-template negate-or-not name-pattern grep-string)))
    (grep-find actual-command)))

(defun find-in-project-glob-by-name-old (path name-pattern grep-string prefix)
  "find|xargs in current project dir by name. Negate with prefix arg."
  (interactive (list (read-directory-name "starting point: " wjb-find-in-project-default-dir)
                     (read-from-minibuffer "name glob: " "*")
                     (read-from-minibuffer "search for: ")
                     current-prefix-arg))
  (let* ((default-directory path)
         (command-template (concat "gfind . %s -iname '%s' " wjb-find-args wjb-after-the-pipe))
         (negate-or-not (if prefix "!" ""))
         (actual-command (format command-template negate-or-not name-pattern grep-string)))
    (grep-find actual-command)))


;; TODO: match multiple globs. It needs multiple ipaths and -o for "or":
;; $ gfind public -ipath '*.png' -o -ipath '*.js'
;; or could use -regex which works on the whole path.
;; $ gfind public -regex '.*\(png\|js\)'
;;
;; Useful (-ipath - path vs. iname - filename):
;; without the leading *, it matches nothing.
;; gfind . -ipath '*selectors/index.js' ! -name "*~" ! -name "#*#" ! -path "*node_modules*" ! -path "*.git*" ! -path "*_tmp*" ! -path "*coverage*" ! -path "*dist*" -type f -print0 | xargs -0 -P 2 rg -C 5 --no-heading -niH -e part

;; rgrep allows a shell wildcard pattern on filenames, but find-in-project does not.
;; make rgrep behave how I want, like my own find-in-project command.
(add-to-list 'grep-find-ignored-directories "node_modules")
(grep-apply-setting 'grep-find-template "gfind . <X> -type f <F> -exec ggrep <C> -nH -C 5 -e <R> {} +")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
(global-set-key (kbd "C-c g") 'grep-find)
;; i is old/everything, 9 is by path, j is by name
(global-set-key (kbd "C-x i") 'find-in-project)  ; Clobbers insert-file.
;; (global-set-key (kbd "C-x 9") 'rgrep)
(global-set-key (kbd "C-x 9") 'find-in-project-glob-by-path)
(global-set-key (kbd "C-x C-9") 'find-in-project-glob-by-path)
(global-set-key (kbd "C-x j") 'find-in-project-glob-by-name)
(global-set-key (kbd "C-x C-j") 'find-in-project-glob-by-name)

;; Ways to do my find in project from the command line:
;; find . -name "models.py" | xargs grep -niEH -C 5 <query>
;; grep -E --color=auto -Iin -r -C 3 --exclude *~ <query> <dir>
;; alias fin='grep -E --color=auto -Iin -r -C 3 --exclude *~'

;; Open grep results in the same frame.
;;
;; same-window-buffer-names
;; * https://stackoverflow.com/questions/12231783/in-emacs-when-i-do-a-grep-find-how-can-i-have-the-files-displayed-to-me-open-i?noredirect=1&lq=1
;; * https://stackoverflow.com/questions/15814031/want-compile-goto-error-variant-that-replaces-compilation-buffer-in-current-wind?noredirect=1&lq=1
;;
;; compile-goto-error is the function when I hit Enter on a grep result. It
;; opens in a new frame. Why? Normal behavior may be to open compile results in
;; a new window, but I have set pop-up-windows nil in sane-defaults so for me,
;; compile results open in the same window. But then hitting enter opens in a
;; new frame which still seems odd.
;;
;; I also want the same behavior from xref. It defines its own xref-goto-xref. L521
;;
;; This is from https://stackoverflow.com/a/20548556. It seems to work and is
;; much smaller than the previous solution. Though it seems not to work when
;; dirtree window is not open.
(defadvice compile-goto-error (around my-compile-goto-error activate)
  (let ((display-buffer-overriding-action '(display-buffer-reuse-window (inhibit-same-window . nil))))
    ad-do-it))

;; from https://emacs.stackexchange.com/a/34724/2163:
(defvar my-inhibit-set-window-dedicated nil)
(advice-add 'set-window-dedicated-p :around
  (lambda (orig-fun &rest args)
    "Honor inhibitor variable `my-inhibit-set-window-dedicated'."
    (unless my-inhibit-set-window-dedicated
      (apply orig-fun args))))

(advice-add 'xref--show-pos-in-buf :around
  (lambda (orig-fun &rest args)
    "Inhibit `set-window-dedicated-p'."
    (let ((my-inhibit-set-window-dedicated t))
      (apply orig-fun args))))

(use-package wgrep
  :after grep)

;; ag
;;(when (executable-find "ag")
;;  (require-package 'ag)
;;  (require-package 'wgrep-ag)
;;  (setq-default ag-highlight-search t)
;;  (add-to-list 'ag-arguments "-C 5")
;;  (global-set-key (kbd "C-x 9") 'ag-project))

;; (setq helm-ag-base-command "rg -C 5 --no-heading -niH -e")

(provide 'setup-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-grep.el ends here
