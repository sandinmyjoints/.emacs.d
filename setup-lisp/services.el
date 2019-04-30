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

  (let ((services '(
                    ("sd-gimme-db" . 'docker)
                    ("atalanta" . 'docker-express)
                    ("darwin" . 'docker)
                    ("sd-auth" . 'docker-express)
                    ("sd-playground" . 'docker-express)
                    ("sd-spelling" . 'docker-express)
                    ("neodarwin" . 'docker-express)
                    ("sd-router" . 'docker)
                    )))
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
