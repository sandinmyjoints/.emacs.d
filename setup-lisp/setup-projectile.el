;;; setup-projectile.el --- Set up Projectile.
;;
;; Filename: setup-projectile.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Fri Jun 24 22:17:02 2016 (-0700)
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

;; projectile-mode keymap
;; C-c p b 'projectile-switch-to-buffer
;; C-x b
;;
;; C-c p g 'projectile-find-file-dwim
;; C-c f
;;
;; C-c p f 'projectile-find-file
;;
;; C-c p k 'projectile-kill-buffer

(projectile-global-mode +1)

(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "local_notes")
(add-to-list 'projectile-globally-ignored-directories "yarn-offline-mirror")

(add-to-list 'projectile-globally-ignored-file-suffixes "pyc")
(add-to-list 'projectile-globally-ignored-file-suffixes ".pyc")
(add-to-list 'projectile-globally-ignored-file-suffixes "elc")
(add-to-list 'projectile-globally-ignored-file-suffixes ".elc")

;; i can exclude per-project using .projectile

;; Some info about caching: https://tuhdo.github.io/helm-projectile.html#sec-6-7
;;
;; Turn on cache per repo with this in .dir-locals.el:
;; ((nil . ((projectile-enable-caching . t))))
;;
;; (setq projectile-enable-caching t)
;;
;; C-u C-c p f = projectile-invalidate-cache
;; C-c p z = projectile-cache-current-file

(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action 'projectile-vc)
(setq-default projectile-indexing-method 'alien)


;; fd is supposedly faster than find, but it might not be installed,
;; so better to use it via dir-locals. However, it keeps bugging me
;; about it being a risky variable...
(setq projectile-git-command "fd . -0")

(defalias 'find-file-in-project 'projectile-find-file-dwim)
;;
(eval-after-load 'projectile-mode
  ;; projectile-find-file-dwim is more generalized than projectile-find-file
  (define-key projectile-mode-map (kbd "C-c p g") 'projectile-find-file-dwim))


;; auth
;; src/controller/file.js
;; test/controller/file.js
(defun wjb/related-files-corresponding-path (path)
  (if (string-match (rx (group (or "src" "test"))
                        (group "/" (+? anything))
                        (group (1+ (not (any "/"))) (or ".js" ".jsx" ".coffee"))) path)
      (let* ((top-dir (match-string 1 path))
             (mid-path (match-string 2 path))
             (filename (match-string 3 path)))
        (if (equal top-dir "test")
            (list :impl (concat "src" mid-path filename))
          (list :test (concat "test" mid-path filename))))))

;; intake
;; src/file.js
;; test/server/file.test.js
(defun wjb/related-files-corresponding-path-po-intake (path)
  (if (string-match (rx (group (or "src" "test"))
                        (group "/" (*? anything))
                        (group (1+ (not (any "/"))) (or ".js" ".jsx" ".coffee"))) path)
      (let* ((top-dir (match-string 1 path))
             (mid-path (match-string 2 path))
             (src-mid-path (s-replace "/server" "" mid-path))
             (filename (match-string 3 path))
             (filename-base (file-name-base filename))
             (filename-extension (file-name-extension filename t))
             (filename-test (concat filename-base ".test" filename-extension))
             (filename-impl (s-replace ".test" "" filename)))
        ;; (debug)
        (if (equal top-dir "test")
            (list :impl (concat "src" src-mid-path filename-impl))
          (list :test (concat "test/server" mid-path filename-test))))))

;; playground
;; src/controller/file.js
;; test/controller/file.test.js
(defun wjb/related-files-corresponding-path-playground (path)
  (if (string-match (rx (group (or "src" "test"))
                        (group "/" (+? anything))
                        (group (1+ (not (any "/"))) (or ".js" ".jsx" ".coffee"))) path)
      (let* ((top-dir (match-string 1 path))
             (mid-path (match-string 2 path))
             (filename (match-string 3 path))
             (filename-base (file-name-base filename))
             (filename-extension (file-name-extension filename t))
             (filename-test (concat filename-base ".test" filename-extension))
             (filename-impl (s-replace ".test" "" filename)))
        ;; (if (equal path "/Users/william/scm/sd/sd-playground/test/controllers/reconcile.test.js")
        ;;     (debug))
        (if (equal top-dir "test")
            (list :impl (concat "src" mid-path filename-impl))
          (list :test (concat "test" mid-path filename-test))))))

;; neodarwin server
;; test/file.coffee but for controllers there's common|desktop|mobile

;; neodarwin components
;; src/components/test.js
;; src/components/test.jsx
(defun wjb/related-files-same-dir-components (path)
  (if (string-match (rx (group (+? anything))
                        (group (1+ (not (any "/"))) (or ".js" ".jsx"))) path)
      (let* ((dir (match-string 1 path))
             (filename (match-string 2 path))
             (filename-base (file-name-base filename))
             (filename-extension (file-name-extension filename t))
             (filename-test (concat "test" filename-extension))
             (filename-impl (concat "index" filename-extension)))
        ;; (if (equal path "/Users/william/scm/sd/sd-playground/test/controllers/reconcile.test.js")
        ;;     (debug))
        ;; (debug)
        (if (equal filename-base "test")
            (list :impl (concat dir filename-impl))
          (list :test (concat dir filename-test))))))

(projectile-register-project-type 'npm '("package.json")
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
          :src-dir "src"
				  :test-dir "test"
          :related-files-fn (list (projectile-related-files-fn-test-with-suffix "js" "test")
                                  (projectile-related-files-fn-test-with-suffix "jsx" "test")
                                  #'wjb/related-files-corresponding-path-playground
                                  #'wjb/related-files-same-dir-components
                                  #'wjb/related-files-corresponding-path-po-intake
                                  #'wjb/related-files-corresponding-path)
          )

;; (projectile-register-project-type 'yarn '("yarn.lock")
;; 				  :compile "yarn"
;; 				  :test "npm test"
;; 				  :run "npm start"
;;           :src-dir "src"
;; 				  :test-dir "test"
;;           )

(provide 'setup-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-projectile.el ends here
