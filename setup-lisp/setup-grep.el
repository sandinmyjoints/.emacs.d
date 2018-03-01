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
;; TODO: custom grep and find, so can use ggrep and gfind if installed.
;; TODO: dir-local list of paths to exclude from grep-find (e.g., .git and node_modules, dist, _tmp, minified files)
;; TODO: Document this better.
;; See also find-in-project in defuns.el.
;; http://stackoverflow.com/a/2148754
;; comparison with ag: https://www.reddit.com/r/programming/comments/16bvah/the_silver_searcher_is_a_35x_faster_drop_in/
;; find-args for OS X find: "! -name \"*~\" ! -name \"#*#\" ! -wholename \"*node_modules*\" ! -wholename \"*.git*\" -type f -print0 | xargs -0 grep -E -C 5 -niH -e "
;; find-args for GNU find:
;;
;; -wholename = -path
;; -path = pathname
;; -name = last part of pathname
;;
;; Consider ignoring:
;; *[-.]min[.-]*
;; *.gz.js
;;
;; Consider having two commands: one that ignores everything I might want
;; ignored, and one that ignores conservatively.

(setq grep-name "grep")
(when (executable-find "ggrep")
  (setq grep-name "ggrep"))

(setq find-name "find")
(when (executable-find "gfind")
  (setq find-name "gfind"))

(setq find-args (format "! -name \"*~\" ! -name \"#*#\" ! -path \"*node_modules*\" ! -path \"*.git*\" ! -path \"*_tmp*\" ! -path \"*coverage*\" ! -path \"*dist*\" -type f -print0 | xargs -0 -P 2 %s --line-buffered -E -C 5 -niH -e " grep-name)
      default-find-cmd (concat find-name " . " find-args))


;; How to use grep-apply-setting: http://stackoverflow.com/a/25633595/599258

(grep-compute-defaults)
(grep-apply-setting 'grep-find-command default-find-cmd)

;; make rgrep behave how I want, like my own find-in-project command.
(add-to-list 'grep-find-ignored-directories "node_modules")
(grep-apply-setting 'grep-find-template "gfind . <X> -type f <F> -exec ggrep <C> -nH -C 5 -e <R> {} +")

;; rgrep allows a shell wildcard pattern on filenames, but find-in-project does not.

;; Custom grep-find via find-in-project.
(defvar find-in-project-default-dir ".")

(defun find-in-project (path grep-string)
  "rgrep in current project dir."
  (interactive (list (read-directory-name "path: " find-in-project-default-dir)
                     (read-from-minibuffer "find: ")))
  (let ((default-directory path))
    (grep-find
     (concat "gfind . " find-args grep-string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-x i") 'find-in-project)  ; Clobbers insert-file.
(global-set-key (kbd "C-x 9") 'rgrep)

;; Ways to do my find in project from the command line:
;; find . -name "models.py" | xargs grep -niEH -C 5 <query>
;; grep -E --color=auto -Iin -r -C 3 --exclude *~ <query> <dir>
;; alias fin='grep -E --color=auto -Iin -r -C 3 --exclude *~'

;(eval-after-load 'grep '(require 'setup-rgrep))

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


;; ag
;;(when (executable-find "ag")
;;  (require-package 'ag)
;;  (require-package 'wgrep-ag)
;;  (setq-default ag-highlight-search t)
;;  (add-to-list 'ag-arguments "-C 5")
;;  (global-set-key (kbd "C-x 9") 'ag-project))

(provide 'setup-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-grep.el ends here
