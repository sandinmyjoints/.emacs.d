;; based on https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (yas-expand)
      (company-complete-common)))

(defun rk/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (company-yasnippet-or-completion)
      (indent-for-tab-command)))

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

;; (defun rk/copilot-complete-if-active (next-func n)
;;   (let ((completed (when copilot-mode (copilot-accept-completion))))
;;     (unless completed (funcall next-func n))))
(defun rk/copilot-complete-if-active (next-func n)
  (let ((completed (copilot-accept-completion)))
    (unless completed (funcall next-func n))))

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode
                              compilation-mode
                              flutter-mode
                              jest-mode
                              text-mode
                              minibuffer-mode)
  "Modes in which copilot is inconvenient.")


(defvar rk/copilot-manual-mode t
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defvar rk/copilot-enable-for-org nil
  "Should copilot be enabled for org-mode buffers?")

(defun rk/copilot-enable-predicate ()
  ""
  (and
   (eq (get-buffer-window) (selected-window))))

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (and (not rk/copilot-enable-for-org) (eq major-mode 'org-mode))
      (company--active-p)))

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(defun rk/copilot-toggle-for-org ()
  "Toggle copilot activation in org mode. It can sometimes be
annoying, sometimes be useful, that's why this can be handly."
  (interactive)
  (setq rk/copilot-enable-for-org (not rk/copilot-enable-for-org))
  (message "copilot for org is %s" (if rk/copilot-enable-for-org "enabled" "disabled")))

(use-package copilot
  ;; :disabled ;; wondering if this causes random hangs
  :load-path ("elisp/copilot.el")
  :diminish
  :config
  ;; (setq copilot-indent-warning-suppress t)
  (setq copilot-enable-predicates '(copilot--buffer-changed))

  (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)

  (define-key global-map (kbd "C-c <TAB>") #'rk/copilot-complete-or-accept)
  ;; (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

  ;; Do copilot-quit when pressing C-g
  (advice-add 'keyboard-quit :before #'rk/copilot-quit)

  ;; complete by pressing right or tab but only when copilot completions are
  ;; shown. This means we leave the normal functionality intact.
  (advice-add 'right-char :around #'rk/copilot-complete-if-active)
  (advice-add 'indent-for-tab-command :around #'rk/copilot-complete-if-active)

  ;; deactivate copilot for certain modes
  (add-to-list 'copilot-enable-predicates #'rk/copilot-enable-predicate)
  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)
  (global-copilot-mode))

(use-package copilot-chat
  :config
  (define-key global-map (kbd "C-c o") #'copilot-chat-transient)
  (setq copilot-chat-frontend 'org))

(setq copilot-chat-org-prompt "You are an experienced software engineer who has worked extensively with Javascript, Typescript, React, Node, MySQL, Python, Django, Bash. Please minimize explanations and assume a high level of knowledge. When asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\n\nUse only Emacs org-mode formatting in your answers.\nWhen using heading to structure your answer, please start at level 3 (i.e with 3 stars or more)\nMake sure to include the programming language name at the start of the org-mode code blocks.\nThis is an example of python code block in emacs org-mode syntax:\n#+BEGIN_SRC python\ndef hello_world():\n	print('Hello, World!')\n#+END_SRC\nAvoid wrapping the whole response in the block code.\n\nDon't forget the most important rule when you are formatting your response: use emacs org-mode syntax only.\n\nThe user works in an IDE called Emacs which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up a developer.\nProvide clear and relevant examples aligned with any provided context.\n")

(provide 'setup-copilot)
