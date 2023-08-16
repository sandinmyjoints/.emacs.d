;;; services.el --- Define servics and commands for working with them.
;;
;; Filename: services.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Sat May 11 11:49:04 2019 (-0700)
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

(defvar wjb/sd-services
  '(
    "sd-gimme-db"
    "atalanta"
    "darwin"
    "sd-auth"
    "sd-leaderboards"
    "sd-scribe"
    "sd-playground"
    "sd-spelling"
    "neodarwin"
    "hegemone"
    "sd-router"
    "word-of-the-day"))

(defvar wjb/wjb-projects
  '(
    ".emacs.d"
    "git-mine"))

(defvar wjb/projects (list
                      "adhoc"
                      "sd-router"
                      "neodarwin"
                      "atalanta"
                      "sd-auth"
                      "sd-scribe"
                      "sd-leaderboards"
                      "hegemone"
                      "sd-playground"
                      "sd-gimme-db"
                      ".emacs.d"
                      "equivalency"
                      "use-async-queue"
                      "git-mine"
                      "word-of-the-day"
                      ))

(use-package hydra
  :config
  (setq hydra--work-around-dedicated nil
        hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        '(
          :internal-border-width 2
          ;; :internal-border-color "red"
          :poshandler posframe-poshandler-frame-above-center)))

;; TODO is this needed, or is setting the hint-display-type to posframe enough?
(use-package hydra-posframe
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-poshandler 'posframe-poshandler-frame-above-center))

(defun hydra-posframe-show (str)
  "HACK: redefining in order to use the poshandler I want."
  (require 'posframe)
  (posframe-show
   " *hydra-posframe*"
   :string str
   :poshandler #'posframe-poshandler-frame-above-center
   :internal-border-width 3
   ;; :internal-border-color "light gray"
   :internal-border-color "dark gray"
   :left-fringe 10
   :right-fringe 10
   :min-height 2
   :min-width 50))


;; TODO: ideal would be a hydra that first selects a project, then selects an action (vc, vterm).
(defhydra wjb/projects/hydra (:color blue :columns 3)
   "Switch to project"
        ("a" (wjb/switch-to-project-vterm "/Users/william") "adhoc")
        ("r" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-router") "sd-router")
        ("n" (projectile-switch-project-by-name "/Users/william/scm/sd/neodarwin") "neodarwin")
        ("t" (projectile-switch-project-by-name "/Users/william/scm/sd/atalanta") "atalanta")
        ("w" (projectile-switch-project-by-name "/Users/william/scm/sd/word-of-the-day") "wotd")
        ("u" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-auth") "sd-auth")
        ("c" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-scribe") "sd-scribe")
        ("l" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-leaderboards") "sd-leaderboards")
        ("h" (projectile-switch-project-by-name "/Users/william/scm/sd/hegemone") "hegemone")
        ("p" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-playground") "sd-playground")
        ("g" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-gimme-db") "sd-gimme-db")
        ("e" (projectile-switch-project-by-name "/Users/william/.emacs.d") "emacs.d")
        ("s" (projectile-switch-project-by-name "/Users/william/scm/wjb/silver-mind") "silver-mind")
        ("i" (projectile-switch-project-by-name "/Users/william/scm/wjb/nicer-email-extension") "nicer.email")
        ("y" (projectile-switch-project-by-name "/Users/william/scm/wjb/defoxify-extension") "defoxify")
        ("q" (projectile-switch-project-by-name "/Users/william/scm/sd/equivalency") "equivalency"))
(global-set-key (kbd "H-p") 'wjb/projects/hydra/body) ;; analogous to C-c C-p

(defun wjb/switch-to-project-vterm (proj-dir)
  (let ((projectile-switch-project-action #'projectile-run-vterm))
    (if (equal proj-dir "/Users/william")
        (let ((default-directory "/Users/william"))
          (if (get-buffer "*vterm adhoc*")
              (switch-to-buffer "*vterm adhoc*")
            (vterm "*vterm adhoc*")))
      (projectile-switch-project-by-name proj-dir))))

(defun wjb/switch-to-vterm ()
  (interactive)
  (push-mark)
  (let ((project (projectile-project-name)))
    (when (member project wjb/projects)
      (progn
        (setq hydra-deactivate t)
        (projectile-run-vterm)))))

(defhydra wjb/projects/hydra/shell (:color blue :columns 3)
   "Shell in project"
        ("a" (wjb/switch-to-project-vterm "/Users/william") "adhoc")
        ("r" (wjb/switch-to-project-vterm "/Users/william/scm/sd/sd-router") "sd-router")
        ("n" (wjb/switch-to-project-vterm "/Users/william/scm/sd/neodarwin") "neodarwin")
        ("d" #'wjb/switch-to-vterm "current" :exit nil)
        ("w" (wjb/switch-to-project-vterm "/Users/william/scm/sd/word-of-the-day") "wotd")
        ("u" (wjb/switch-to-project-vterm "/Users/william/scm/sd/sd-auth") "sd-auth")
        ("c" (wjb/switch-to-project-vterm "/Users/william/scm/sd/sd-scribe") "sd-scribe")
        ("l" (wjb/switch-to-project-vterm "/Users/william/scm/sd/sd-leaderboards") "sd-leaderboards")
        ("h" (wjb/switch-to-project-vterm "/Users/william/scm/sd/hegemone") "hegemone")
        ("p" (wjb/switch-to-project-vterm "/Users/william/scm/sd/sd-playground") "sd-playground")
        ("g" (wjb/switch-to-project-vterm "/Users/william/scm/sd/sd-gimme-db") "sd-gimme-db")
        ("e" (wjb/switch-to-project-vterm "/Users/william/.emacs.d") "emacs.d")
        ("i" (wjb/switch-to-project-vterm "/Users/william/scm/wjb/nicer-email-extension") "nicer")
        ("y" (wjb/switch-to-project-vterm "/Users/william/scm/wjb/defoxify-extension") "defoxify")
        ("q" (wjb/switch-to-project-vterm "/Users/william/scm/sd/equivalency") "equivalency"))
(global-set-key (kbd "H-d") 'wjb/projects/hydra/shell/body)

;; TODO: rewrite using defhydra+
;; see https://github.com/abo-abo/hydra/issues/185

(defhydra hydra-projects (:color blue :columns 2)
  "Projects."
  ("_" list "nop"))

;; (defhydra+ hydra-projects ()
;;   ,@(mapcar (lambda (x)
;;               (list (car x) (cadr x) (caddr x)))
            ;; wjb/projects/hydra))

;; (mapc (lambda (project)
;;         (defhydra+ hydra-projects () "docstring"
;;           (list (car project) (cadr project) (caddr project))))
;;         wjb/projects/hydra)

;; (mapc (lambda (project)
;;         (eval `(defhydra+ hydra-projects () "docstring"
;;                  (list (car project) (cadr project) (caddr project))))
;;         wjb/projects/hydra))

;; (head-binding head-command head-hint head-plist)
;; TODO: compute
;; (defvar wjb/projects/hydra '())
;; (setq wjb/projects/hydra
;;       '(
;;         ("r" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-router") "sd-router")
;;         ("n" (projectile-switch-project-by-name "/Users/william/scm/sd/neodarwin") "neodarwin")
;;         ("a" (projectile-switch-project-by-name "/Users/william/scm/sd/atalanta") "atalanta")
;;         ("d" (projectile-switch-project-by-name "/Users/william/scm/sd/darwin") "darwin")
;;         ("h" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-auth") "sd-auth")
;;         ("p" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-playground") "sd-playground")
;;         ("s" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-spelling") "sd-spelling")
;;         ("g" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-gimme-db") "sd-gimme-db")
;;         ("e" (projectile-switch-project-by-name "/Users/william/.emacs.d") "emacs")
;;         ("q" (projectile-switch-project-by-name "/Users/william/scm/sd/equivalency") "equivalency")
;;         ))
;; ;; recreates the hydra when activated, picking up new services. Based on
;; ;; https://github.com/abo-abo/hydra/issues/164
;; (bind-keys ("H-i" .
;;             (lambda ()
;;               (interactive)
;;               (call-interactively
;;                (eval `(defhydra hydra-projects (:color blue :columns 3)
;;                         "Projects"
;;                         ,@(mapcar (lambda (x)
;;                                     (list (car x) (cadr x) (caddr x)))
;;                                   wjb/projects/hydra)))))))

(defvar wjb/sd-services/prodigy '(
                                  ("sd-gimme-db" . 'docker)
                                  ("atalanta" . 'docker-express)
                                  ("darwin" . 'docker)
                                  ("sd-auth" . 'docker-express)
                                  ("sd-playground" . 'docker-express)
                                  ("sd-spelling" . 'docker-express)
                                  ("neodarwin" . 'docker-express)
                                  ("sd-router" . 'docker)
                                  ))

;; (prodigy-define-service
;;   :name "webpack"
;;   :cwd "~/project/"
;;   :init-async (lambda (done) (nvm-use-for "~/project/" done))
;;   :command "npm"
;;   :args '("run" "build" "--" "--watch")
;;   :ready-message "Build Finished")
;; (prodigy-define-service
;;   :name "tsc"
;;   :cwd "~/project/"
;;   :init-async (lambda (done) (nvm-use-for "~/project/" done))
;;   :command "npm"
;;   :args '("run" "compile" "--watch")
;;   :ready-message "Watching for file changes.")
;; (prodigy-define-service
;;   :name "server"
;;   :env '(("PORT" "6789")
;;          )
;;   :cwd "~/project/"
;;   :init-async (lambda (done) (nvm-use-for "~/project/" done))
;;   :command "npm"
;;   :args '("run" "serve-dev")
;;   :ready-message "Listening on 6789")

(use-package prodigy
  :disabled
  :config
  ;; from https://github.com/jhirn/emacs-prelude/blob/master/personal/prodigy.el
  (prodigy-define-tag
    :name 'docker
    :ready-message "Attaching to \\.*")

  (prodigy-define-tag
    :name 'docker-express
    :ready-message "listening on http://\\.*")

  (prodigy-define-tag
    :name 'docker-detached
    :ready-message "Starting \\.* ... done")

  ;;
  (defun prodigy-define-docker-compose (project-cons)
    (let ((project (car project-cons))
          (tags (cdr project-cons)))
      (prodigy-define-service
        :name (concat project)
        :command "docker-compose"
        :args '("up")
        :cwd (format "~/scm/sd/%s" project)
        :tags tags)))

  (let (wjb/sd-services/prodigy)
    (mapc #'prodigy-define-docker-compose wjb/sd-services/prodigy))

  (global-set-key (kbd "C-x q") #'prodigy)

  ;; (let ((project "atalanta")
  ;;   (prodigy-define-docker-compose project)))

  ;; (prodigy-define-service
  ;;   :name "neodarwin-webpack-build"
  ;;   :cwd "~/scm/sd/neodarwin"
  ;;   :init-async (lambda (done) (nvm-use-for "~/scm/sd/neodarwin" done))
  ;;   :command "nice"
  ;;   :args '("npm" "run" "build:webpack:dev")
  ;;   :ready-message "Webpack done.")
  ;; (prodigy-define-service
  ;;   :name "neodarwin-webpack-watch"
  ;;   :cwd "~/scm/sd/neodarwin"
  ;;   :init-async (lambda (done) (nvm-use-for "~/scm/sd/neodarwin" done))
  ;;   :command "nice"
  ;;   :args '("npm" "run" "watch:build:webpack:dev")
  ;;   :ready-message "webpack is watching the files")
  )

(provide 'services)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; services.el ends here
