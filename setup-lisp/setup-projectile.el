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
;; Package-Requires: ((emacs "25.1"))
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

(setq projectile-project-search-path '("~/scm/wjb" "~/scm/sd"))

;; fd is supposedly faster than find, but it might not be installed,
;; so better to use it via dir-locals. However, it keeps bugging me
;; about it being a risky variable...
(setq projectile-git-command "fd . -0")

(defalias 'find-file-in-project 'projectile-find-file-dwim)
;;
(eval-after-load 'projectile-mode
  ;; projectile-find-file-dwim is more generalized than projectile-find-file
  (progn
    (define-key projectile-command-map (kbd "g") 'projectile-find-file-dwim)
    (define-key projectile-command-map (kbd "a") 'projectile-find-related-file)))




;; Project types.

;; Check project type with:
;; M-: (projectile-project-type)

;; How to specify project-type using dir-locals:
;; ((nil . ((projectile-project-type . use-async-queue))))

;; Reset project types, good for testing:
(defun wjb/reset-projectile-project-types ()
  (interactive)
  (setq projectile-project-types '()))


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
;; test/controller/file.db.test.js -- not handled yet
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

(defun projectile-find-impl-file ()
  "Open impl file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name) :impl)))

(defun projectile-find-test-file ()
  "Open test file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name) :test)))

(defun projectile-find-stories-file ()
  "Open stories file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name) :stories)))

(defun projectile-find-translations-file ()
  "Open translations file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name) :translations)))

(defun projectile-find-styles-file ()
  "Open styles file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name) :styles)))

;; Can also use H-p for C-c p
;; C-c p s o stories
;; C-c p s y styles
;; C-c p s t tests
;; C-c p s r translations
;; C-c p s i impl/index
(eval-after-load 'projectile-mode
  ;; projectile-find-file-dwim is more generalized than projectile-find-file
  (progn
    (define-key projectile-command-map (kbd "s i") 'projectile-find-impl-file)
    (define-key projectile-command-map (kbd "s t") 'projectile-find-test-file)
    (define-key projectile-command-map (kbd "s o") 'projectile-find-stories-file)
    (define-key projectile-command-map (kbd "s y") 'projectile-find-styles-file)
    (define-key projectile-command-map (kbd "s r") 'projectile-find-translations-file)
    ))


;; neodarwin components
;; src/components/test.js
;; src/components/test.jsx
(defun wjb/related-files-same-dir-components (path)
  (if (string-match (rx (group (+? anything))
                        (group (1+ (not (any "/"))) (or ".js" ".jsx" ".less"))) path)
      (let* ((dir (match-string 1 path))
             (filename (match-string 2 path))
             (filename-base (file-name-base filename))
             (filename-extension (file-name-extension filename t))
             (filename-extension-usable (cond
                                         ((equal filename-base "translations") ".jsx")
                                         ((equal filename-base "stories") ".jsx")
                                         ((equal filename-extension ".less") ".jsx")
                                         (t filename-extension)))
             (filename-impl (concat "index" filename-extension-usable))
             (filename-test (concat "test" filename-extension-usable))
             (filename-translations "translations.js")
             (filename-styles "styles.less")
             (filename-stories "stories.jsx"))

        ;; non-destructive
        ;; (setq wjb/tt (list :a 1 :b 2))
        ;; (org-plist-delete wjb/tt :a)

        (let ((relations (list :impl (concat dir filename-impl)
                               :test (concat dir filename-test)
                               :translations (concat dir filename-translations)
                               :styles (concat dir filename-styles)
                               :stories (concat dir filename-stories))))
          (cond
           ((equal filename-base "test")
            (org-plist-delete relations :test))
           ((equal filename-base "translations")
            (org-plist-delete relations :translations))
           ((equal filename-base "index")
            (org-plist-delete relations :impl))
           ((equal filename-base "stories")
            (org-plist-delete relations :stories))
           ((equal filename-base "styles")
            (org-plist-delete relations :styles))
           (t relations
            ))))))

(defun wjb/related-files-same-dir-components-orig (path)
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

;; use-async-queue
;; pairs are in same directory:
;; use.js
;; use.test.js
(projectile-register-project-type
 'use-async-queue '("use-async-queue.js")
 :configure "yarn"
 :compile "yarn build"
 :test "yarn test"
 :src-dir "."
 :test-dir "."
 :test-suffix ".test")


(projectile-register-project-type 'neodarwin '("use-dir-locals")
				  :configure "yarn"
          :compile "yarn run watch:build:dev"
				  :test "npm test"
          :src-dir "src"
				  :test-dir "test"
          :related-files-fn (list (projectile-related-files-fn-test-with-suffix "js" ".test")
                                  (projectile-related-files-fn-test-with-suffix "jsx" ".test")
                                  #'wjb/related-files-same-dir-components
                                  #'wjb/related-files-corresponding-path-po-intake
                                  #'wjb/related-files-corresponding-path)
          )

;; separate src and test
;; sd-auth, sd-playground, atalanta, neodarwin server (except for db test)
(projectile-register-project-type
 'sd-auth '("use-dir-locals")
 :configure "yarn"
 :test "yarn test"
 :run "yarn start"
 :related-files-fn #'wjb/related-files-corresponding-path)

;; Generic fallback
(projectile-register-project-type 'npm '("package.json")
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
          :src-dir "src"
				  :test-dir "test"
          :related-files-fn (list (projectile-related-files-fn-test-with-suffix "js" "test")
                                  (projectile-related-files-fn-test-with-suffix "jsx" "test")
                                  (projectile-related-files-fn-test-with-suffix "js" "test.js")
                                  #'wjb/related-files-corresponding-path-playground
                                  (projectile-related-files-fn-test-with-suffix "js" "db.test.js")
                                  #'wjb/related-files-same-dir-components
                                  #'wjb/related-files-corresponding-path-po-intake
                                  #'wjb/related-files-corresponding-path)
          )

;; Generic, for when source and test files are in same dir.
;; More generic should come later.
;; use-async-queue, sort of equivalency
(projectile-register-project-type
 'yarn-same-dir '("yarn.lock")
 :configure "yarn"
 :test "yarn test"
 :run "yarn start"
 :test-suffix ".test")

(projectile-register-project-type
 'po-intake '("set-with-dir-locals")
 :configure "yarn"
 :test "yarn test"
 :run "yarn start"
 :src-dir "src"
 :test-dir "test/server"
 :test-suffix ".test")

;; a.el and a_test.el
;; ext is el
;; suffix is _test
;; a.js and a.db.test.js
;; ext is js
;; suffix is .db.test, or
;; suffix is db.test.
;; suffix is .db.test.
(projectile-register-project-type
 'sd-playground '("set-with-dir-locals")
 :configure "yarn"
 :test "yarn test"
 :run "yarn start"
 :src-dir "src"
 :test-dir "test"
 :related-files-fn (list (projectile-related-files-fn-test-with-suffix "js" ".test")
                         ;; this seems like it should work, but it does not:
                         (projectile-related-files-fn-test-with-suffix "js" ".db.test")))

(provide 'setup-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-projectile.el ends here
