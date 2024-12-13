(defvar after-kill-new-hook nil
  "Hook run after text is added to the kill ring.")

(defadvice kill-new (after run-after-kill-new-hook activate)
  "Run `after-kill-new-hook` after text is added to the kill ring."
  (run-hooks 'after-kill-new-hook))

(define-minor-mode kill-dollar-mode
  "A minor mode to remove leading $ from lines when killing text
 in org or markdown code blocks."
  :lighter " Kill-$"
  :global nil

  (if kill-dollar-mode
      (add-hook 'after-kill-new-hook #'remove-dollar-on-kill nil t)
    (remove-hook 'after-kill-new-hook #'remove-dollar-on-kill t)))

(defun remove-dollar-on-kill ()
  "Remove leading $ from each line of killed text when inside org
 or markdown code blocks."
  (when (and (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
             (save-excursion
               (or
                (and (derived-mode-p 'org-mode)
                     (eq (org-element-type (org-element-context)) 'src-block))
                (and (derived-mode-p 'markdown-mode)
                     (markdown-code-block-at-point)))))
      (let ((processed-text
             (with-temp-buffer
               (insert (current-kill 0))
               (goto-char (point-min))
               (while (not (eobp))
                 (when (looking-at "^\\s-*\\$\\s-*")
                   (replace-match "" nil nil))
                 (forward-line 1))
               (buffer-string))))
        (kill-new processed-text))))

(provide 'kill-dollar-mode)
