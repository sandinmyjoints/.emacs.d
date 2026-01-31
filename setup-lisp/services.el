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
    "atalanta"
    "cicero"
    "hegemone"
    "neodarwin"
    "sd-auth"
    "sd-scribe"
    "sd-gimme-db"
    "sd-leaderboards"
    "sd-playground"
    "sd-router"
    "sd-scribe"
    "sd-spelling"
    "word-of-the-day"
    "sd-reword"
))

(defvar wjb/projects (list
                      ".emacs.d"
                      "adhoc"
                      "amcat-ext"
                      "atalanta"
                      "cicero"
                      "equivalency"
                      "git-mine"
                      "hegemone"
                      "neodarwin"
                      "sd-auth"
                      "sd-scribe"
                      "sd-gimme-db"
                      "sd-leaderboards"
                      "sd-playground"
                      "sd-router"
                      "sd-scribe"
                      "word-of-the-day"
                      "sd-reword"
                      ))

(use-package hydra
  :config
  (setq hydra--work-around-dedicated nil)
  ;; (setq hydra-hint-display-type 'posframe)
  ;; (setq hydra-posframe-show-params
  ;;       '(
  ;;         :internal-border-width 2
  ;;         ;; :internal-border-color "red"
  ;;         :poshandler posframe-poshandler-frame-above-center))
)

;; TODO: ideal would be a hydra that first selects a project, then selects an action (vc, vterm).
(defhydra wjb/projects/hydra (:color blue :columns 3)
   "Switch to project"
        ("a" (wjb/switch-to-project-vterm (expand-file-name "~")) "adhoc")
        ("e" (projectile-switch-project-by-name (home-subdir ".emacs.d")) "emacs.d")
        ("t" (projectile-switch-project-by-name (home-subdir "scm/sd/atalanta")) "atalanta")
        ("i" (projectile-switch-project-by-name (home-subdir "scm/sd/cicero")) "cicero")
        ("q" (projectile-switch-project-by-name (home-subdir "scm/sd/equivalency")) "equivalency")
        ("h" (projectile-switch-project-by-name (home-subdir "scm/sd/hegemone")) "hegemone")
        ("n" (projectile-switch-project-by-name (home-subdir "scm/sd/neodarwin")) "neodarwin")
        ("u" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-auth")) "sd-auth")
        ("m" (projectile-switch-project-by-name (home-subdir "scm/sandinmyjoints/amcat-ext")) "amcat-ext")
        ("g" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-gimme-db")) "sd-gimme-db")
        ("l" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-leaderboards")) "sd-leaderboards")
        ("p" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-playground")) "sd-playground")
        ("r" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-router")) "sd-router")
        ("c" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-scribe")) "sd-scribe")
        ("w" (projectile-switch-project-by-name (home-subdir "scm/sd/word-of-the-day")) "wotd")
        ("o" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-reword")) "sd-reword")
)
(global-set-key (kbd "H-p") 'wjb/projects/hydra/body) ;; analogous to C-c C-p

(defun wjb/switch-to-project-vterm (proj-dir)
  (let ((projectile-switch-project-action #'projectile-run-vterm))
    (if (equal proj-dir wjb/home)
        (let ((default-directory wjb/home))
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
        ("d" #'wjb/switch-to-vterm "current" :exit nil)
        ("a" (wjb/switch-to-project-vterm wjb/home) "adhoc")
        ("e" (wjb/switch-to-project-vterm (home-subdir ".emacs.d")) "emacs.d")
        ("i" (wjb/switch-to-project-vterm (home-subdir "scm/sd/cicero")) "cicero")
        ;; ("i" (wjb/switch-to-project-vterm (home-subdir "scm/wjb/nicer-email-extension")) "nicer")
        ("q" (wjb/switch-to-project-vterm (home-subdir "scm/sd/equivalency")) "equivalency")
        ("h" (wjb/switch-to-project-vterm (home-subdir "scm/sd/hegemone")) "hegemone")
        ("n" (wjb/switch-to-project-vterm (home-subdir "scm/sd/neodarwin")) "neodarwin")
        ("m" (wjb/switch-to-project-vterm (home-subdir "scm/sandinmyjoints/amcat-ext")) "amcat-ext")
        ("r" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-router")) "sd-router")
        ("u" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-auth")) "sd-auth")
        ("c" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-scribe")) "sd-scribe")
        ("l" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-leaderboards")) "sd-leaderboards")
        ("p" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-playground")) "sd-playground")
        ("g" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-gimme-db")) "sd-gimme-db")
        ("w" (wjb/switch-to-project-vterm (home-subdir "scm/sd/word-of-the-day")) "wotd")
        ("o" (wjb/switch-to-project-vterm (home-subdir "scm/sd/sd-reword")) "sd-reword")
)
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
;;         ("r" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-router") "sd-router")
;;         ("n" (projectile-switch-project-by-name (home-subdir "scm/sd/neodarwin") "neodarwin")
;;         ("a" (projectile-switch-project-by-name (home-subdir "scm/sd/atalanta") "atalanta")
;;         ("d" (projectile-switch-project-by-name (home-subdir "scm/sd/darwin") "darwin")
;;         ("h" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-auth") "sd-auth")
;;         ("p" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-playground") "sd-playground")
;;         ("s" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-spelling") "sd-spelling")
;;         ("g" (projectile-switch-project-by-name (home-subdir "scm/sd/sd-gimme-db") "sd-gimme-db")
;;         ("e" (projectile-switch-project-by-name (home-subdir ".emacs.d") "emacs")
;;         ("q" (projectile-switch-project-by-name (home-subdir "scm/sd/equivalency") "equivalency")
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

(provide 'services)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; services.el ends here
