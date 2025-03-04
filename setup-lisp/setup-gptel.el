(cl-defun my/clean-up-gptel-refactored-code (beg end)
  "Clean up the code responses for refactored code in the current buffer.

The response is placed between BEG and END.  The current buffer is
guaranteed to be the response buffer."
  (when gptel-mode          ; Don't want this to happen in the dedicated buffer.
    (cl-return-from my/clean-up-gptel-refactored-code))
  (when (and beg end)
    (save-excursion
      (let ((contents
             (replace-regexp-in-string
              "\n*``.*\n*" ""
              (buffer-substring-no-properties beg end))))
        (delete-region beg end)
        (goto-char beg)
        (insert contents))
      ;; Indent the code to match the buffer indentation if it's messed up.
      (indent-region beg end)
      (pulse-momentary-highlight-region beg end))))

(defun my/gptel-mode-auto ()
  "Ensure that this file opens with `gptel-mode' enabled."
  (save-excursion
    (let ((enable-local-variables t))  ; Ensure we can modify local variables
      (if (and (save-excursion
                 (goto-char (point-min))
                 (looking-at ".*-\\*-")))  ; If there's a -*- line
          ;; First remove any existing eval, then add the new one
          (modify-file-local-variable-prop-line
           'eval nil 'delete))
      ;; Always add our eval
      (add-file-local-variable-prop-line
       'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))

(add-hook 'gptel-save-state-hook #'my/gptel-mode-auto)

(use-package gptel
  ;; :disabled
  :bind ((:map markdown-mode-map ("C-<return>" . gptel-send))
         (:map gfm-mode-map ("C-<return>" . gptel-send)))
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (remove-hook 'gptel-post-response-functions #'my/clean-up-gptel-refactored-code)
  (remove-hook 'gptel-save-state-hook #'my/gptel-mode-auto)

  (add-to-list 'gptel-directives '(tech-lead . "You are a tech lead and software engineer with experience working with AWS, relational databases, http servers, and web projects."))

  ;; (setq deepseek-r1
  ;;       (gptel-make-ollama "Ollama"
  ;;         :host "localhost:11434"
  ;;         :stream t
  ;;         :models '(deepseek-r1:7b)
  ;;         :request-params '(:options (:num_ctx 131072 :num_predict 8192))))
  ;; (setq
  ;;  gptel-model 'deepseek-r1:7b
  ;;  gptel-backend 'deepseek-r1)

  (setq ollama-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(hf.co/sugatoray/DeepSeek-Coder-V2-Lite-Base-Q4_K_M-GGUF
                    hf.co/sm54/Mistral-Small-24B-Instruct-2501-Q4_K_M-GGUF:latest)
          :request-params '(:keep_alive -1 :options (:num_ctx 4096 :num_predict 8192))))
  (setq gptel-backend ollama-backend)
  (setq gptel-model 'hf.co/sm54/Mistral-Small-24B-Instruct-2501-Q4_K_M-GGUF:latest)

  ;; (gptel-api-key-from-auth-source "api.openai.com" "apikey")
  (setq gptel-model 'gpt-4o-mini)
  ;; (setq gptel-model 'o3-mini)
  )

(provide 'setup-gptel)
