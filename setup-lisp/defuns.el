;;; defuns.el --- Custom defuns.
;;
;; Filename: defuns.el
;; Description:
;; Author: William
;; Maintainer:
;; Created: Wed Oct  8 16:55:59 2014 (-0700)
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

(defun home-subdir (subdir)
  "Helper for subdirectories of $HOME."
  (expand-file-name subdir "~"))

(defun mine-goto-symbol-at-point ()
  "Will navigate to the symbol at the current point of the cursor."
  (interactive)
  (ido-goto-symbol (thing-at-point 'symbol)))

;; Useful in fullscreen:
;; http://osxdaily.com/2012/03/27/remove-auto-hide-dock-delay-mac-os-x/
(defun wjb/toggle-fullscreen ()
  "Toggle full screen. Especially useful on laptops."
  (interactive)
  (let ((fs (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
    (set-frame-parameter nil 'fullscreen fs)
    ;; (if (equal fs 'fullboth)
    ;;     (display-time-mode)
    (display-time-mode 1)
  ))

(defun wjb/margin-0 ()
  "Give current window a left margin of 0 columns."
  (interactive)
  (set-window-margins
   (get-buffer-window (current-buffer)) 0 0))

(defun wjb/margin-x ()
  "Give current window a left margin of x columns."
  (interactive)
  (set-window-margins
   (get-buffer-window (current-buffer)) 12 0))

;; TODOs:
;; - set this for a window configuration
;; - nice border
;; - also center modeline
;;
;; (defun wjb/center-column ()
;;   "Center column of current window. Resize window margins to allow for a single 120 column."
;;   (interactive)
;;   (let ((margin-size (/ (- (frame-width) 120) 2)))
;;     (set-window-margins nil margin-size margin-size))
;;     ;; (set-window-margins
;;     ;;  (get-buffer-window (current-buffer)) margin-size margin-size)
;;   ;; https://stackoverflow.com/a/8349940/599258
;;   )

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

;; this is now builtin in emacs 30.1
;; (defun toggle-window-dedicated ()
;;   "Toggle whether the current active window is dedicated or not"
;;   (interactive)
;;   (message
;;    (if (let (window (get-buffer-window (current-buffer)))
;;          (set-window-dedicated-p window
;;                                  (not (window-dedicated-p window))))
;;        "'%s' is dedicated"
;;      "'%s' is normal")
;;    (current-buffer)))

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
          (inverted "invert: false")
          (msg "Not found"))
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

;; describe('do only one thing')
;; context('do thing')
;; test('do thing')
;; it('do thing')
;; fn('do thing')
(defun wjb/toggle-only ()
  "Toggle .only in appropriate identifier on current line."
  (interactive)
  (let* ((unonlyable-identifiers-re "describe.only\\|context.only\\|test.only\\|it.only")
         (onlyable-identifiers-re "describe\\|context\\|test\\|it"))
    (save-excursion
      (save-restriction
        (call-interactively 'select-current-line)
        (call-interactively 'narrow-to-region)
        (goto-char (point-min))
        (if (re-search-forward unonlyable-identifiers-re nil t)
            (let* ((bounds (bounds-of-thing-at-point 'word))
                   (end (cdr bounds))
                   (middle (- end 5)))
              (delete-region middle end)
              (save-buffer))
          (progn
            (condition-case nil
                (progn
                  (re-search-forward onlyable-identifiers-re nil)
                  (let* ((thing2 (thing-at-point 'word))
                         (bounds (bounds-of-thing-at-point 'word))
                         (pos1 (car bounds))
                         (pos2 (cdr bounds))
                         (replacement (format "%s.only" thing2)))
                    (replace-with pos1 pos2 replacement)
                    (save-buffer)))
              (error nil))))))))

(defalias 'toggle-only #'wjb/toggle-only)

(defun wjb-toggle-it-only-coffee (arg)
  "Toggle `only` on / off for the current test."
  (interactive "P")
  (let ((onlyed-re "it\.only \\([\"']\\)")
        (unonlyed-re "it \\([\"']\\)")
        (onlyed "it.only \\1")
        (unonlyed "it \\1")
        (msg "No test found"))
    (save-excursion
      (progn
        (cond ((null arg)
               ;; Null arg. Turn on.
               (unless (null (re-search-backward unonlyed-re (point-min) t))
                 (progn
                   (replace-match onlyed nil nil)
                   (setq msg "Test only'ed."))))
              (t
               ;; Non-null arg. Turn off.
               (unless (null (re-search-backward onlyed-re (point-min) t))
                 (progn
                   (replace-match unonlyed nil nil)
                   (setq msg "Tests un-only'ed.")))))))

    (save-buffer)
    (message msg)))

(defun wjb-toggle-it-only-js (arg)
  "Toggle `only` on / off for the current test."
  (interactive "P")
  ;; use highlight-regexp to test
  (let ((onlyed-re "\\(?:^\\|[[:space:]]\\)\\(?:it\\|test\\)\.only\\([(]\\)")
        (unonlyed-re "\\(?:^\\|[[:space:]]\\)\\(?:it\\|test\\)\\([(]\\)")
        (onlyed "it.only(")
        (unonlyed "it(")
        (msg "No test found"))
  ;; (let ((onlyed-re "\\(?:test\\|it\\)\.only\\([(]\\)")
  ;;       (unonlyed-re "\\(?:test\\|it\\)\\([(]\\)")
  ;;       (onlyed "\\(?:test\\|it\\).only(")
  ;;       (unonlyed "\\(?:test\\|it\\)(")
  ;;       (msg "No test found"))
    (save-excursion
      (progn
        (cond ((null arg)
               ;; Null arg. Turn on.
               (unless (null (re-search-backward unonlyed-re (point-min) t))
                 (progn
                   (replace-match onlyed nil nil)
                   (setq msg "Test focused."))))
              (t
               ;; Non-null arg. Turn off.
               (unless (null (re-search-backward onlyed-re (point-min) t))
                 (progn
                   (replace-match unonlyed nil nil)
                   (setq msg "Test unfocused.")))))))

    (save-buffer)
    (message msg)))

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
  (shell-command-on-region (point-min) (point-max) command nil t))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Useful kbd macros.

;; HOWTO add a named kbd macro to the kmacro-ring
;;(kmacro-push-ring (list 'remove-api-version-log 0 "%d"))
;;(kmacro-pop-ring)

;; In json-mode, prettifies one line and leaves cursor at beginning of next.
(fset 'json-prettify-one-line
      [?\C-a ?\C-  ?\C-e ?\C-c ?\C-f ?\C-u ?\C-  ?\C-n])

;; Inserts a log of "test " on newline after "test:"
(fset 'insert-test-counter
      "\C-stest:\C-e\C-j\C-x\C-k\C-i\355console.log \"test \C-e\"\C-d")

;; Search and delete a console.log statement.
;; TODO update these to call isearch directly
(fset 'remove-console-log-coffee
      "\C-sconsole.log \"DEBUG:\C-a\C-k\C-k\C-x\C-s")

(fset 'remove-console-log-js
      "\C-sconsole.log('DEBUG:\C-a\C-a\C-k\C-k\C-x\C-s")

(defun wjb/insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defalias #'insert-date #'wjb/insert-date)

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

;; (defun wjb/camelize (s)
;;   "Convert under_score string S to CamelCase string."
;;   (mapconcat 'identity (mapcar
;;                         #'(lambda (word) (capitalize (downcase word)))
;;                         (split-string s "_")) ""))

;; (defalias #'camelize #'wjb/camelize)

;; (defun wjb/camelize-method (s)
;;   "Convert under_score string S to camelCase string."
;;   (mapconcat 'identity (mapcar-head
;;                         #'(lambda (word) (downcase word))
;;                         #'(lambda (word) (capitalize (downcase word)))
;;                         (split-string s "_")) ""))

;; (defalias #'camelize-method #'wjb/camelize-method)

;; (defun wjb/camelize-thing-at-point ()
;;   "Camelize thing at point."
;;   (interactive)
;;   (let ((thing (thing-at-point 'word)))
;;     (setq bounds (bounds-of-thing-at-point 'word))
;;     (setq pos1 (car bounds))
;;     (setq pos2 (cdr bounds))
;;     (save-excursion
;;       (delete-region pos1 pos2)
;;       (goto-char pos1)
;;       (insert (camelize-method thing)))))

;; (defalias #'camelize-thing-at-point #'wjb/camelize-thing-at-point)

;; Working:
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun lowercase-first-character (s)
  (format "%s%s" (downcase (cadr (split-string s ""))) (s-join "" (cddr (split-string s "")))))

(defun camelcase  (s) (lowercase-first-character (mapconcat 'capitalize (split-name s) "")))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore (s)
  (cond ((string-match-p "-" s) (underscore s))
        ((string-match-p "_" s)	(camelcase s))
        (t   (dasherize s)) ))

;; Entrypoint:
(defun wjb/camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

(defalias 'wjb/cycle-case #'wjb/camelscore-word-at-point)

;; TODO: camelize-all-like-thing-at-point


;;; Chef / tramp stuff.
;;; To get started: M-: (wjb-query-chef-refresh-all)
;;; Then open a file with tramp: /william@aws-whatever:/path
;;; If get ControlPath too long, then do:
;;; before starting Tramp: M-x setenv TMPDIR /tmp.
;;; see https://trac.macports.org/ticket/29794

;; (defun wjb-query-chef-handle-line (line)
;;   (if (s-starts-with? "aws-" line)
;;       (let* ((parts (s-split " " line))
;;              (node (pop parts))
;;              (info (cons (first parts) (second parts))))
;;         ;; Save parts as node-name, public-hostname, public-ipv4
;;         (list node info))))

;; (defun wjb-query-chef-callback (process content)
;;   "Callback for data from chef node queries.

;; Puts results in plist (node-name (hostname . ip))"
;;   ;; Split on newline.
;;   (let* ((splits (s-split "\n" content)))
;;     (-each (-remove 'null (-map 'wjb-query-chef-handle-line splits))
;;            (lambda (item)
;;              (setq wjb-chef-node-plist (lax-plist-put wjb-chef-node-plist (car item) (second item)))))))

;; (defun wjb-query-chef-ips (&optional node-pattern &optional env-pattern)
;;   (let ((process-connection-type nil) ;; Use a pipe.
;;         (node-pattern (if node-pattern node-pattern "*"))
;;         (env-pattern (if env-pattern env-pattern "*")))
;;     ;; TODO: Short-circuit return if the shell script doesn't exist.
;;     (setq wjb-process (start-process "query-chef-ips" "*query-ip-results*" "query_ip" node-pattern env-pattern))
;;     ;; Attach filter function as process output callback.
;;     (set-process-filter wjb-process 'wjb-query-chef-callback)))

;; ;; TODO: Could use a sentinel to get notified when process status changes.
;; ;; (when (not (process-live-p wjb-process))
;; ;;   (message "Process not live")
;; ;;   (with-current-buffer (process-buffer wjb-process)
;; ;;     (let* ((start (search-backward "Using"))
;; ;;            (results (buffer-substring start (process-mark wjb-process))))
;; ;;       (message (concat "Results: " results))
;; ;;       (setq results-perm results))))

;; (defun wjb-query-chef-refresh-all ()
;;   (setq wjb-chef-node-plist ())
;;   (wjb-query-chef-ips))

;; ;;(wjb-query-chef-refresh-all)

;; (defun wjb-chef-node-ip (node-name)
;;   (cdr (lax-plist-get wjb-chef-node-plist node-name)))

;; (defun wjb-chef-node-hostname (node-name)
;;   (car (lax-plist-get wjb-chef-node-plist node-name)))

;; (defadvice tramp-dissect-file-name (after lookup-chef-hostnames)
;;   "Lookup and replace Chef nodes with their hostnames."
;;   ;; If ad-return-value is a property in the plist, return its public hostname.
;;   ;;(message "Checking %s, got %s" (aref ad-return-value 2) (lax-plist-get wjb-chef-node-plist (aref ad-return-value 2)))
;;   (if (lax-plist-get wjb-chef-node-plist (aref ad-return-value 2))
;;       (aset ad-return-value 2 (first (lax-plist-get wjb-chef-node-plist (aref ad-return-value 2))))
;;       ad-return-value))

;; ;; TODO: activate this automatically.
;; ;;(ad-activate 'tramp-dissect-file-name)
;; ;;(ad-update 'tramp-dissect-file-name)

;; (unless (boundp 'wjb-chef-node-plist)
;;   (setq wjb-chef-node-plist ()))

;(wjb-chef-node-hostname "aws-prod-platform-oneiric-c1m-01")
;(wjb-chef-node-ip "aws-prod-platform-oneiric-c1m-01")

(defun wjb/long-lines ()
  "Set fill-column to a very large number."
  (interactive)
  (setq fill-column 100000))

(defalias #'long-lines #'wjb/long-lines)

;; From http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; TODO: Doesn't work well in org-mode.
;;
;; TODO: Could do org-to-md that kills region and puts it in a new markdown
;; buffer.
;;
;; TODO: Sometimes undoes the last thing that you did, instead of the fill.
;;
(defun prepare-for-email (beg end)
  "Unfills the current region or buffer, kills it, restores."
  (interactive "*r")
  (save-excursion
    (if (use-region-p)
        (progn
          (call-interactively 'unfill-region)
          (kill-ring-save (region-beginning) (region-end))
          (call-interactively 'fill-region))
      (progn
        (mark-whole-buffer)
        (call-interactively 'unfill-region)
        (kill-ring-save (point-min) (point-max))
        (call-interactively 'fill-region)))))

;; TODO:
;; * fill-region with prefix
;; * copy whatever is filled to kill ring for convenient pasting
;;
(defun fill-for-email ()
  "Fill paragraph with very long fill-column."
  (interactive)
  (let ((fill-column-stashed fill-column))
    (setq fill-column 10000)
    (fill-paragraph)
    (setq fill-column fill-column-stashed)))

;; Journal command. Deprecated by wjb/olivetti.
(defun wjb/journal ()
  "Start journaling"
  (interactive)
  (switch-to-buffer "journal")
  (text-mode)
  (auto-fill-mode 1)
  (set-fill-column 80))

(fset 'fix-js-indent
   [?\M-x ?j ?s ?- ?m ?o ?d ?e return ?\C-x ?h tab ?\M-x ?j ?s ?2 ?- ?m ?o ?d ?e return])

;; (fset 'update-deploy-data-bag
;;    [?\C-s ?r ?e ?v ?\M-f ?\M-f ?\M-b ?\H-k ?\C-x ?r ?i ?c ?\C-s ?\C-s ?\M-f ?\M-f ?\M-b ?\H-k ?\C-x ?r ?i ?c ?\H-s])

(fset 'wjb/add-standup-entry
   [?\C-s ?# ?\S-  ?T ?o ?d ?a ?y ?\C-a ?\C-f ?\C-f ?\C-k ?P ?r ?e ?v ?i ?o ?u ?s ?l ?y ?\M-> return ?# ?\S-  ?T ?o ?d ?a ?y return ?- ? ])

(defun wjb/do-standup ()
  "Do standup."
  (interactive)
  (wjb/switch-to-standup)
  ;; removed save-excursion
  (goto-char (point-min))
  (call-interactively 'next-line)
  (insert (format "* %s\n** Work\n   - [ ] \n" (format-time-string "%Y-%m-%d")))
  (forward-line -1)
  (move-end-of-line nil))
  ;; (execute-kbd-macro 'wjb/add-standup-entry))

;; The regexp for this is:
;; .*?:\(\s-*\)
(fset 'align-on-colon
   [?\C-u ?\C-| ?\C-a ?. ?* ?? ?: ?\C-e return return return ?y])

(fset 'align-on-equal
   [?\C-| ?  ?= return])

(fset 'align-on-hash
   [?\C-| ?  ?# return])

(defun copy-and-comment ()
  "Comment active region and paste uncommented text on the
following line."
  (interactive)
  (kill-new
   (buffer-substring
    (region-beginning)
    (region-end)))
  (comment-region (region-beginning)
                  (region-end))
  (goto-char (region-end))
  (yank)
  ;(delete-blank-lines)
  ;(newline 1)
  (goto-char (point-at-eol)) ;; why doesn't this work?
  (set-mark-command t))

;; Swap region/lines of text. See:
;; https://groups.google.com/forum/#!msg/gnu.emacs.help/dd2R_UV0LVQ/F06ihLb7hKcJ
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

;; etags / ctags
(cond ((eq system-type 'gnu/linux)
       (setq path-to-ctags "/usr/local/bin/ctags")))
(cond ((eq system-type 'darwin)
       (setq path-to-ctags "/usr/local/bin/ctags")))

;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   ;; (message
;;   ;;  (format "%s -f %s/tags -eR %s" path-to-ctags (directory-file-name dir-name) (directory-file-name dir-name)))
;;   (shell-command
;;    (format "%s -f %s/tags -eR %s" path-to-ctags
;;            (directory-file-name dir-name) (directory-file-name dir-name))))


(defun replace-with (pos1 pos2 replacement)
  ""
  (delete-region pos1 pos2)
  (insert replacement))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

;; var a = true;
;; { a: false }
;; False;
;; yo
(defun wjb/toggle-boolean ()
  "Toggle any booleans found on the current line."
  (interactive)
  (let* ((booleans (ht ("true" "false")
                      ("false" "true")
                      ("True" "False")
                      ("False" "True")))
         (toggle-boolean-re
          (-reduce
           (lambda (memo item)
             (format "%s\\|%s" memo item))
           (ht-keys booleans))))
    (save-excursion
      (save-restriction
        (call-interactively 'select-current-line)
        (call-interactively 'narrow-to-region)
        (goto-char (point-min))
        (re-search-forward toggle-boolean-re nil t))
      (let* ((thing2 (thing-at-point 'word))
             (bounds (bounds-of-thing-at-point 'word))
             (pos1 (car bounds))
             (pos2 (cdr bounds)))
        (setq replacement (ht-get booleans thing2 nil))
        (when replacement
          (replace-with pos1 pos2 replacement))))))

(defalias 'toggle-boolean #'wjb/toggle-boolean)

(defun comment-box-better (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

;; From https://www.reddit.com/r/emacs/comments/6qpbka/elisp_for_text_processing_in_buffers/
(defun sed-region (program)
  "Run one line of sed code on every line of the current region."
  (interactive "sSed code: ")
  (let ((tmpfile (make-temp-name "/tmp/esed"))
        sed-exit-status)
    (write-region program nil tmpfile)
    (setq sed-exit-status
          (call-process-region (region-beginning)
                               (region-end)
                               "sed"
                               t            ; delete old text?
                               t            ; put new text directly in buffer
                               nil          ; display as we go
                               "-f"         ; command line arg 1
                               tmpfile      ; command line arg 2
                               ))
    (if (not (= sed-exit-status 0))
        (message "sed returned non-zero exit status"))
    (delete-file tmpfile)))

(defun perl-region (program)
  "Run one line of perl code on every line of the current region."
  (interactive "sPerl code: ")
  (let ((tmpfile (make-temp-name "/tmp/eperl"))
        perl-exit-status)
    (write-region program nil tmpfile)
    (setq perl-exit-status
          (call-process-region (region-beginning)
                               (region-end)
                               "perl"
                               t            ; delete old text?
                               t            ; put new text directly in buffer
                               nil          ; display as we go
                               "-p"        ; command line arg 1
                               tmpfile      ; command line arg 2
                               ))
    (if (not (= perl-exit-status 0))
        (message "perl returned non-zero exit status"))
    (delete-file tmpfile)))

(defun python-region (program)
  "Run one line of python code on every line of the current region.
Example: import sys; sys.stdout.write(sys.stdin.read())"
  (interactive "sPython code: ")
  (let ((tmpfile (make-temp-name "/tmp/epython"))
        python-exit-status)
    (write-region program nil tmpfile)
    (setq python-exit-status
          (call-process-region (region-beginning)
                               (region-end)
                               "python"
                               t            ; delete old text?
                               t            ; put new text directly in buffer
                               nil          ; display as we go
                               "-B"        ; command line arg 1
                               tmpfile      ; command line arg 2
                               ))
    (if (not (= python-exit-status 0))
        (message "python returned non-zero exit status"))
    (delete-file tmpfile)))


(defun wjb-mark-region (beg end)
  (set-mark beg)
  (goto-char end)
  (activate-mark))

(defun wjb-mark-node (node leave-point-at-start)
  (let* ((start-fn (if leave-point-at-start #'js2-node-abs-end #'js2-node-abs-pos))
         (end-fn (if leave-point-at-start #'js2-node-abs-pos #'js2-node-abs-end))
         (start (funcall start-fn node))
         (end (funcall end-fn node)))
    (wjb-mark-region start end)))

(defun wjb-kill-node (node)
  (let ((start (js2-node-abs-pos node))
        (end (js2-node-abs-end node)))
    (kill-region start end)
    (goto-char start)))

(defun wjb-mark-this-node (leave-point-at-start)
  ;; By default, leave point at end.
  (interactive "P")
  (wjb-mark-node (js2r--next-node) leave-point-at-start))

(defun wjb-kill-this-node ()
  (interactive)
  ;; TODO:
  ;; - use js2-node-at-point instead
  ;; - if node is root node, then instead delete node at point-1? or point+1? or next node that is found?
  ;;(wjb-kill-node (js2r--next-node))
  (wjb-kill-node (js2-node-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; occur

;; Following based on
;; https://masteringemacs.org/article/searching-buffers-occur-mode
(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-mode (mode)
  "Show all lines matching REGEXP in buffers with major mode MODE."
  (multi-occur
   (get-buffers-matching-mode mode)
   (car (occur-read-primary-args))))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur-in-mode major-mode))

(defun mode-from-string (mode-string)
  "Return a mode from a string."
  (intern-soft mode-string))

(defun multi-occur-in-mode-string (mode-string)
  "Show all lines matching REGEXP in buffers with major mode MODE-STRING."
   (interactive "Cmajor-mode: ")
   (multi-occur-in-mode (mode-from-string mode-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; repls

;; run-js?
;; indium-run-node
;; js-comint-repl
(defun run-node (cwd)
  (interactive "DDirectory: ")
  (unless (executable-find "node")
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (pop-to-buffer (make-comint (format "node-repl-%s" cwd) "node" nil "--interactive"))))

(defalias 'node-repl 'run-node)
(defun run-coffee (cwd)
  (interactive "DDirectory: ")
  (unless (and (executable-find "node") (executable-find "coffee"))
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (call-interactively 'coffee-repl)))

;; Needs Node to really honor NODE_NO_READLINE. See:
;; https://github.com/joyent/node/issues/5344
(defun run-coffee-someday (cwd)
  "Run Coffeescript."
  (interactive "DDirectory: ")
  (unless (and (executable-find "node") (executable-find "coffee"))
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
    (pop-to-buffer
     (apply 'make-comint (format "coffee-repl-%s" cwd)
            "env"
            nil
            "NODE_NO_READLINE=1"
            "coffee"
            (list "--interactive")))))

;; Format markdown with prettier (>1.8.0)
(fset 'format-markdown
   [?\C-x ?h ?\C-u ?\M-| ?~ ?/ ?. ?y ?r ?n backspace backspace ?a ?r ?n ?/ ?b ?i ?n ?/ ?p ?r ?e ?t ?t ?i ?e ?r ?  ?- ?- ?p ?a ?r ?s ?e ?r ?  ?m ?a ?r ?k ?d ?o ?w ?n return ?\C-x ?\C-s])

;; from https://emacs.stackexchange.com/a/35683/2163
(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True, wrap_after=10))'"
   t t))

;; (defun wjb/switch-to-dirtree ()
;;   "Switch to dirtree buffer."
;;   (interactive)
;;   ;; (pop-to-buffer "*dirtree*")

;;   ;; see display-buffer docs:
;;   ;; action preserve-size
;;   ;; (display-buffer-reuse-window . ((preserve-size . (t . t))))
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Action-Functions.html

;;   ;; why doesn't this work??
;;   ;; this may be why: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dedicated-Windows.html
;;   ;; TODO: undedicate dirtree window while this runs, then re-dedicate it
;;   ;; (pop-to-buffer "*dirtree*" '(display-buffer-reuse-window . ((preserve-size . (t . t)))) t)

;;   ;; with window-fixed-size set on dirtree window, this works unless
;;   ;; there are 3+ windows, so TODO undedicate dirtree window
;;   (unless (s-equals? (buffer-name) "*dirtree*")
;;     (progn
;;       (with-current-buffer "*dirtree*"
;;         (setq-local window-size-fixed t))
;;       (switch-to-buffer-other-window "*dirtree*" t)
;;       (with-current-buffer "*dirtree*"
;;         (setq-local window-size-fixed nil)))))

(defun mw-lisp-butt-display ()
  "Function to produce nicer lisp butts.
This function can be hooked into the modes of interest.  E.g.
(add-hook 'emacs-lisp-mode-hook #'mw-lisp-butt-display)
(add-hook 'lisp-mode-hook #'mw-lisp-butt-display)"
  (font-lock-add-keywords
   nil
   '((")\\())+\\))"
      (1 (compose-region
          (match-beginning 1) (match-end 1)
          ".")
         nil)))))

(defun wjb/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;; Reload dir-locals. From https://emacs.stackexchange.com/a/13096/2163
(defun wjb/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;; TODO: make this reload them for all buffers in the project.
(defun wjb/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (wjb/reload-dir-locals-for-current-buffer)))))

(global-set-key (kbd "H-r") #'wjb/reload-dir-locals-for-all-buffer-in-this-directory)

(defun wjb/revert-without-query ()
  (interactive)
  (revert-buffer t t))

(defun yank-no-excursion (&optional arg)
  "`yank' wrapped by `save-excursion'. Useful when pasting very long lines."
  (interactive "P")
  (save-excursion (yank arg)))

;; TODO:
;; Mixpanel event parser
;; - extract data query param
;; - url decode it
;; - base64 decode it to utf-8

(defun wjb/soft-wrap-text ()
  "Soft wrap: sets fill-column to 10000. Doesn't auto-fill;
instead, wraps at screen edge, thanks to visual-line-mode."
  (setq fill-column 10000)
  (auto-fill-mode -1)
  (visual-line-mode 1))

(defun wjb/hard-wrap-text ()
  "Hard wrap: sets fill-column to 80 and auto-fills."
  (setq fill-column 78)
  (auto-fill-mode 1)
  (visual-line-mode -1))

(defun wjb/switch-to-clock ()
  "Switch to last clock buffer."
  (interactive)
  (switch-to-buffer "clock.org"))

(defun tinyurl (beg end)
  "Get a tinyurl.com URL from the contents of the region.
The result is pushed onto the kill ring."
  (interactive "r")

  (let ((old (buffer-substring beg end))
	      new)
    (unless old
      (error "No region"))
    (with-current-buffer (url-retrieve-synchronously
			                    (format "https://tinyurl.com/api-create.php?url=%s"
				                          (urlenc:encode-string old 'utf-8)))
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	      (setq new (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer)))
    (unless new
      (error "No response from tinyurl"))
    (kill-new new)
    (message "Copied %s" new)))

;; from https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
;; Good for raw Travis logs.
;; Not ideal b/c it modifies the file, but other solutions give do not.
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defalias 'convert-md-link-to-org
   (kmacro "C-s \\ [ C-f C-s \\ ( <backspace> <backspace> ( C-SPC C-s ) C-b C-w C-r \\ [ C-f C-SPC C-s \\ ] C-b C-c C-l C-y <return> <return> C-d C-d C-d C-r \\ [ C-d"))

(defalias 'fix-org-to-md-sub-tag
   (kmacro "C-s < s u b > <return> <backspace> <backspace> <backspace> <backspace> <backspace> _ C-s < / s u b > <return> <backspace> <backspace> <backspace> <backspace> <backspace> <backspace>"))

(defun md-to-org-region (start end)
  "Convert region from markdown to org, replacing selection"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

(defun wjb/org-gfm-export-and-copy ()
  "Export Org to GFM Markdown (no TOC), copy to kill-ring, silent and restoring windows."
  (interactive)
  (require 'ox-gfm)
  (let ((org-export-with-toc nil))             ;; This disables TOC globally during export
    (save-window-excursion
      (let ((export-buffer (org-export-to-buffer
                            'gfm "*Org GFM Export*" nil t t nil)))
        (with-current-buffer export-buffer
          (kill-new (buffer-string)))
        (kill-buffer export-buffer))))
  (message "Markdown copied to kill-ring."))

(provide 'defuns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defuns.el ends here
