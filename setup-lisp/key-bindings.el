;; Available to use:
;; * C-z
;; * C-,
;; * C-.
;; * C-'
;; * C-x C-c
;; * C-z
;; * C-x C-|
;; * C-c C-f
;; * C-x C-r
;;
;;; Code:

;; From https://gist.github.com/cataska/b1875754128853bfb139
;; Enables doing something like this, so H-x does the same thing as C-x:
;; (defkbalias (kbd "C-x") (kbd "H-x"))
;; But in practice, it turned out I didn't really use that...
(defmacro defkbalias (old new)
  `(define-key (current-global-map) ,new
     (lookup-key (current-global-map) ,old)))

(global-unset-key (kbd "C-x ."))  ;; unset set-fill-prefix

;; Used by Alfred.
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-g g"))
(global-unset-key (kbd "M-g M-g"))

;; js2-refactor uses either H-c, H-r, or C-c C-r.

(global-set-key (kbd "H-x H-e") 'eval-print-last-sexp)

(global-set-key (kbd "H-v") #'scroll-up-command)
(global-set-key (kbd "H-_") 'undo)
(global-set-key (kbd "H-k") 'kill-sexp)
;; H-d should be backward-kill-subword, but that doesn't exist
(global-set-key (kbd "H-d") 'backward-kill-word) ;; or backward-kill-sexp?
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "H-s") 'save-buffer)

(global-set-key (kbd "H-f") #'forward-word)
(global-set-key (kbd "H-b") #'backward-word)

(global-set-key (kbd "M-f") #'wjb/forward-symbol)
(global-set-key (kbd "M-b") #'wjb/backward-symbol)

;; C-g runs whatever command it is bound to, and now H-g runs a keyboard macro
;; that consists of C-g, so when I hit it, I get the message related to quitting
;; after using a keyboard macro.
(global-set-key (kbd "H-g") (kbd "C-g"))
(global-set-key (kbd "H-a") #'wjb/switch-to-dirtree)

(global-set-key (kbd "C-/") 'hippie-expand) ;; clobbers undo, but I never use it at this binding anyway

(global-set-key (kbd "C-<return>") 'goto-address-at-point)

;; Only works in Emacs >=25.1. Default for this is C-x C-;.
(global-set-key (kbd "C-;") #'comment-line)

(global-set-key (kbd "C-x 7") #'describe-char)

(global-set-key [H-up] 'beginning-of-defun)
(global-set-key (kbd "H-1") 'beginning-of-defun)
(global-set-key [H-down] 'end-of-defun)
(global-set-key (kbd "H-9") 'end-of-defun)

(global-set-key (kbd "C-x C-c") nil)
(global-unset-key (kbd "C-z")) ;; Don't suspend that easily.

(global-set-key (kbd "C-|") 'align-regexp)
(global-set-key (kbd "C-:") 'align-on-colon)
(global-set-key (kbd "C-+") 'align-on-equal)

(global-set-key (kbd "C-c f") 'find-file-at-point)

(global-set-key "\M-c" 'endless/capitalize)
(global-set-key "\M-l" 'endless/downcase)
(global-set-key "\M-u" 'endless/upcase)

(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space))))
        (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'subword-capitalize)))

(defun endless/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))

(global-set-key (kbd "H-l") 'goto-line)

;; (global-set-key (kbd "C-x C-b") 'ibuffer) using helm-buffers-list now

(global-set-key (kbd "C-x C-\\") 'save-buffers-kill-terminal)

(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x l") 'ace-window)

(global-set-key (kbd "C-x C-l") 'other-window-reverse) ; Clobbers downcase-region. Too easy to hit accidentally.
;(global-set-key (kbd "C-x C-o") 'other-window) ; Clobbers delete-blank-lines.

(global-set-key (kbd "C-x p") 'bury-buffer)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c s") 'ansi-term)

;; (global-set-key (kbd "C-c r") 'query-replace-regexp)
;; (global-set-key (kbd "C-c C-r") 're-builder)
(global-set-key (kbd "C-c q") 'query-replace)

(global-set-key (kbd "C-c b") 'rename-buffer)

(global-set-key (kbd "C-c v") 'describe-variable)

(global-set-key (kbd "C-c i") 'indent-relative)

(global-set-key (kbd "C-c d") 'dirtree)

(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c C-SPC") 'just-one-space)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c h") 'whack-whitespace)

(global-set-key (kbd "C-!") 'shell-command-on-buffer)

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "C-c C-b") 'browse-at-remote)

(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c C-j") 'join-line)

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "M-=") 'mark-sexp) ; Clobbers count-words-region.

(global-set-key (kbd "C-0") 'multi-occur-in-this-mode)
(global-set-key (kbd "C-c 0") 'multi-occur-in-mode-string)
;; Example: \\*.org
(global-set-key (kbd "C-c C-0") 'multi-occur-in-matching-buffers)

;; (global-set-key (kbd "H-t") 'toggle-boolean) ;; using H-t for Tern
(global-set-key (kbd "C-c ! !") 'toggle-boolean) ;; flycheck uses C-c !
(global-set-key (kbd "C-c ! t") 'toggle-only) ;; flycheck uses C-c !

;;(global-set-key (kbd "C-x f") 'recentf-open-files)
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; superceded by helm

;;(global-set-key (kbd "C-x f") 'find-file-in-project)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(global-set-key (kbd "C-c C-v") 'wjb-toggle-invert-in-buffer)

;(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(global-set-key (kbd "C-x w") 'prepare-for-email)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "H-x n e") 'next-error)
(global-set-key (kbd "H-x p e") 'previous-error)


(global-set-key (kbd "M-;") #'comment-dwim-2)

;; move lines of text
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement

;; these seem like useful commands, but I didn't use them in practice, and I want these bindings for wrapping in square bracket
;; (global-set-key (kbd "M-[") 'switch-to-prev-buffer)
;; (global-set-key (kbd "M-]") 'switch-to-next-buffer)

;; Shift-arrow moves around windows, by default, which is fine.
;; 'windmove-left ;;  Shift-left arrow is default, that's good
;; 'windmove-right ;; Shift-right arrow is default, that's good
;; (global-set-key (kbd "S-<up>") 'windmove-up)
;; (global-set-key (kbd "S-<down>") 'windmove-down)

;; (global-set-key (kbd "<M-up>") 'scroll-down)
;; (global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "ESC <up>") 'scroll-down)
(global-set-key (kbd "ESC <down>") 'scroll-up)


;; (global-set-key (kbd "C-x C-d") #'wjb/insert-date)
(global-set-key (kbd "C-x C-d") #'dired-jump)  ;; was C-x j, but I want that for grepping
(global-set-key (kbd "C-c ,") #'wjb/switch-to-last-compilation-buffer)
(global-set-key (kbd "C-c C-,") #'wjb/switch-to-last-compilation-buffer)
(global-set-key (kbd "C-c .") #'wjb/switch-to-last-grep-buffer)
(global-set-key (kbd "C-c C-.") #'wjb/switch-to-last-grep-buffer)

(global-set-key (kbd "<f5>") #'compile)
(global-set-key [f6] #'recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wjb-map
;;
;; Custom prefix-map:
;; TODO: use a minor mode, see https://stackoverflow.com/a/683575/599258
;; TODO: more #'wjb/ defuns:
;; - switch-to-restclient -- local_notes/<project name>.rest
;; - deploy-project
(defvar wjb-map nil "Custom prefix map.")
(define-prefix-command 'wjb-map)
(add-hook 'after-init-hook
          (lambda ()
            (global-set-key (kbd "H-0") 'wjb-map)
            (global-set-key (kbd "H-c") 'wjb-map)))

(defun wjb/switch-to-clock ()
  "Switch to last clock buffer."
  (interactive)
  (switch-to-buffer "clock.org"))

(define-key wjb-map (kbd ",") #'wjb/switch-to-last-compilation-buffer)
(define-key wjb-map (kbd ".") #'wjb/switch-to-last-grep-buffer)
(define-key wjb-map (kbd "d") #'wjb/switch-to-dirtree)
;; want to prefer yas-snippet-expand, then fall back to company-complete, then indent-for-tab-command
;; this is what's in yasnippet itself: yas-maybe-expand is a conditional variable?!
;; (define-key yas-minor-mode-map (kbd "<tab>") yas-maybe-expand)
;; (define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand)

(define-key wjb-map (kbd "/") #'hippie-expand)
(define-key wjb-map (kbd "<tab>") #'company-complete)
(define-key wjb-map (kbd "0") #'wjb/switch-to-clock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile section
;;
;; #+BEGIN_SRC
;; C-c p C  = Configure project = ?
;; C-c p c  = Compile project = build artifacts = npm run dev:build
;; C-c p P  = Test Project = run tests = npm run test --colors --watch
;; C-c p u  = Run project = start server = npm start
;; C-c p t  = projectile-toggle-between-implementation-and-test ** set up and figure out how to use these!
;; C-c p T  = projectile-find-test-file
;; C-c p f  = projectile-find-file
;; C-c p g  = projectile-find-file-dwim
;; ?        = projectile-repeat-last-command
;;
;; C-c n v  = package.json
;; #+END_SRC

(define-key wjb-map (kbd "t") #'projectile-test-project)
;; - projectile-configure-project
;; - projectile-run-project
;; what does compile typically do, vs run?
(define-key wjb-map (kbd "c") #'projectile-compile-project)
;; run = start
(define-key wjb-map (kbd "r") #'projectile-run-project)
(define-key wjb-map (kbd "s") (lambda ()
                                (interactive)
                                (pop-to-buffer "sd-standup.md")))

(provide 'key-bindings)
