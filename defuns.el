;; Useful in fullscreen:
;; http://osxdaily.com/2012/03/27/remove-auto-hide-dock-delay-mac-os-x/
(defun toggle-fullscreen ()
  "Toggle full screen. Especially useful on laptops."
  (interactive)
  (let ((fs (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
    (set-frame-parameter nil 'fullscreen fs)
    (if (equal fs 'fullboth)
        (display-time-mode)
      (display-time-mode -1))))

(defun margin-0 ()
  "Give current window a left margin of 0 columns."
  (interactive)
  (set-window-margins
   (get-buffer-window (current-buffer)) 0 0))

(defun margin-x ()
  "Give current window a left margin of x columns."
  (interactive)
  (set-window-margins
   (get-buffer-window (current-buffer)) 12 0))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(defun find-in-project (path grep-string)
  "rgrep in current project dir."
  (interactive (list (read-directory-name "path: " find-in-project-default-dir)
                     (read-from-minibuffer "find: ")))
  (let ((default-directory path))
    (grep-find
     (concat "find . " find-args grep-string))))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "'%s' is dedicated"
     "'%s' is normal")
   (current-buffer)))

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun fix-html ()
  "Fix HTML."
  (interactive)
  (html-mode)
  (sgml-pretty-print (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun fix-json ()
  "Fix JSON."
  ;; TODO: Strip leading and trailing quotes.
  (interactive)
  (json-mode)
  (json-pretty-print-buffer))

(defun wjb-get-marker-replacer (marker)
  "Returns a marker-replacer function for `marker`."
  (lambda ()
    (interactive)
    (save-excursion
      (while (search-forward marker nil t)
        (replace-match "" nil t)))))

;; Shortcut to clear marker from test config file.  TODO: When
;; turning on, if grep is unsuccessful, insert new grep under all
;; greps (most specific).
;;;
(defun wjb-toggle-marker-in-buffer (arg marker)
  "Toggle `marker` on or off in `wjb-test-config-buffer`."
  (interactive "P\nsMarker: ")
  (with-current-buffer wjb-test-config-buffer
    (let ((commented (concat " grep: \"" marker "\""))
          (uncommented (concat " #grep: \"" marker "\"")))
      (progn
        (beginning-of-buffer)
        (cond ((null arg)
               ;; Null arg. Turn on.
               (unless (null (re-search-forward uncommented (point-max) t))
                 (replace-match commented)))
              (t
               ;; Non-null arg. Turn off.
               (unless (null (re-search-forward commented (point-max) t))
                 (replace-match uncommented))))
        (back-to-indentation)
        (save-buffer)))))

(defun wjb-toggle-marker (arg marker &optional handle-in-current-buffer)
  (interactive "P\nsMarker: ")
  (cond
   ((null arg)  ;; Turn on marker.
    (progn
      ;; Insert marker in this buffer.
      (unless (null handle-in-current-buffer)
        (insert marker)
        (save-buffer))
      ;; Turn on marker in test config.
      (wjb-toggle-marker-in-buffer nil marker)
      (message (concat marker " tests on."))))
   (t           ;; Turn off marker.
    (progn
      (unless (null handle-in-current-buffer)
        ;; Get a marker-replacer and use it in this buffer.
        (command-execute
         (wjb-get-marker-replacer marker))
        (save-buffer))
      ;; Turn off the marker in test config.
      (wjb-toggle-marker-in-buffer t marker)
      (message (concat marker " tests off."))))))

(defun wjb-toggle-invert-in-buffer (arg)
  "Toggle `invert` on or off in `wjb-test-config-buffer`."
  (interactive "P")
  (with-current-buffer wjb-test-config-buffer
    (let ((verted "invert: true")
          (inverted "invert: false"))
      (progn
        (beginning-of-buffer)
        (cond ((null arg)
               ;; Null arg. Turn on.
               (unless (null (re-search-forward inverted (point-max) t))
                 (progn
                   (replace-match verted)
                   (setq msg "Tests inverted."))))
              (t
               ;; Non-null arg. Turn off.
               (unless (null (re-search-forward verted (point-max) t))
                 (progn
                   (replace-match inverted)
                   (setq msg "Tests un-inverted.")))))
        (save-buffer)
        (message msg)
        (back-to-indentation)))))

(defun wjb-toggle-it-only (arg)
  "Toggle `only` on / off for the current test."
  (interactive "P")
  (let ((onlyed "it.only \"")
        (unonlyed "it \""))
    (save-excursion
      (progn
        (cond ((null arg)
               ;; Null arg. Turn on.
               (unless (null (re-search-backward unonlyed (point-min) t))
                 (progn
                   (replace-match onlyed)
                   (setq msg "Test only'ed."))))
              (t
               ;; Non-null arg. Turn off.
               (unless (null (re-search-backward onlyed (point-min) t))
                 (progn
                   (replace-match unonlyed)
                   (setq msg "Tests un-only'ed.")))))))
    (save-buffer)
    (message msg)))

;; Journal command.
(defun journal ()
	"Start journaling"
	(interactive)
	(switch-to-buffer "journal")
	(text-mode)
	(auto-fill-mode 1)
	(set-fill-column 80))

(defun sudo-find-file (file-name)
  (interactive "Find file (sudo): ")
  (find-file (concat "/sudo::" file-name)))

(defun go-to-doc (base-url)
    (let ((default (symbol-name (symbol-at-point))))
      (browse-url (concat base-url (read-string "Search term: " nil nil default)))))

(defun search-so ()
  (interactive)
  (go-to-doc "http://stackoverflow.com/search?q="))

(defun search-mdn ()
  (interactive)
  (go-to-doc "https://developer.mozilla.org/en-US/search?q="))

(defun search-npm ()
  (interactive)
  (go-to-doc "https://npmjs.org/search?q="))

(defun search-gh ()
  (interactive)
  (go-to-doc "https://github.com/search?q="))

(defun shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command t))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; In json-mode, prettifies one line and leaves cursor at beginning of next.
(fset 'json-prettify-one-line
   [?\C-a ?\C-  ?\C-e ?\C-c ?\C-f ?\C-u ?\C-  ?\C-n])

;; Inserts a log of "test " on newline after "test:"
(fset 'insert-test-counter
   "\C-stest:\C-e\C-j\C-x\C-k\C-i\355console.log \"test \C-e\"\C-d")


(provide 'defuns)
