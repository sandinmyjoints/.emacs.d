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

(setq find-args "! -name \"*~\" ! -name \"#*#\" ! -path \"*node_modules*\" ! -path \"*.git*\" ! -path \"*_tmp*\" ! -path \"*coverage*\" ! -path \"*dist*\" -type f -print0 | xargs -0 -P 2 ggrep --line-buffered -E -C 5 -niH -e "
      default-find-cmd (concat "gfind " ". " find-args))
(grep-compute-defaults)
(grep-apply-setting 'grep-find-command default-find-cmd)

;; make rgrep behave how I want, like my own find-in-project command.
(add-to-list 'grep-find-ignored-directories "node_modules")
(grep-apply-setting 'grep-find-template "gfind . <X> -type f <F> -exec ggrep <C> -nH -C 5 -e <R> {} +")

;; rgrep allows a shell wildcard pattern on filenames, but find-in-project does not.

;; Ways to do my find in project from the command line:
;; find . -name "models.py" | xargs grep -niEH -C 5 <query>
;; grep -E --color=auto -Iin -r -C 3 --exclude *~ <query> <dir>
;; alias fin='grep -E --color=auto -Iin -r -C 3 --exclude *~'

;(eval-after-load 'grep '(require 'setup-rgrep))


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
