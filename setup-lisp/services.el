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
    "sd-playground"
    "sd-spelling"
    "neodarwin"
    "sd-router"))

;; (head-binding head-command head-hint head-plist)
;; TODO: compute
(defvar wjb/projects/hydra
  '(
    ("r" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-router") "sd-router")
    ("n" (projectile-switch-project-by-name "/Users/william/scm/sd/neodarwin") "neodarwin")
    ("a" (projectile-switch-project-by-name "/Users/william/scm/sd/atalanta") "atalanta")
    ("d" (projectile-switch-project-by-name "/Users/william/scm/sd/darwin") "darwin")
    ("h" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-auth") "sd-auth")
    ("p" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-playground") "sd-playground")
    ("s" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-spelling") "sd-spelling")
    ("g" (projectile-switch-project-by-name "/Users/william/scm/sd/sd-gimme-db") "sd-gimme-db")
    ("e" (projectile-switch-project-by-name "/Users/william/.emacs.d") "emacs")
    ))

;; static implementation -- doesn't pick up changes to the list of services.
;; (global-set-key (kbd "H-i")
;;                 (defhydra hydra-projects (:color blue)
;;                   "Manage SD services."
;;                   ("g" (projectile-switch-project-by-name "sd-gimme-db") "sd-gimme-db")
;;                   ("a" (projectile-switch-project-by-name "/Users/william/scm/sd/atalanta") "atalanta")
;;                   ("d" (projectile-switch-project-by-name "darwin") "darwin")
;;                   ("h" (projectile-switch-project-by-name "sd-auth") "sd-auth")
;;                   ("p" (projectile-switch-project-by-name "sd-playground") "sd-playground")
;;                   ("s" (projectile-switch-project-by-name "sd-spelling") "sd-spelling")
;;                   ("n" (projectile-switch-project-by-name "neodarwin") "neodarwin")
;;                   ("r" (projectile-switch-project-by-name "sd-router") "sd-router")
;;                   ("q" nil nil :exit t)
;;                  ))

(defun hydra-posframe-show (str)
  "HACK: redefining in order to use the poshandler I want."
  (require 'posframe)
  (posframe-show
   " *hydra-posframe*"
   :string str
   :poshandler #'posframe-poshandler-frame-above-center
   :internal-border-width 1
   :internal-border-color "light gray"
   :left-fringe 10
   :right-fringe 10
   :min-height 2
   :min-width 50))

;; recreates the hydra when activated, picking up new services. Based on
;; https://github.com/abo-abo/hydra/issues/164
(bind-keys ("H-i" .
            (lambda ()
              (interactive)
              (call-interactively
               (eval `(defhydra hydra-projects (:color blue :columns 2)
                        "Projects"
                        ,@(mapcar (lambda (x)
                                    (list (car x) (cadr x) (caddr x)))
                                  wjb/projects/hydra)))))))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


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
    (mapc #'prodigy-define-docker-compose services))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; services.el ends here
