(defun api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host host
                    :user (or user "apikey")
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `whatever-key' found in the auth source")))

(use-package aider
  :load-path ("elisp/aider.el")
  :config
  ;; Aider wants chat models (not coder/FIM).
  (define-key global-map (kbd "C-c a") #'aider-transient-menu)
  ;; (setenv "OPENAI_API_KEY" (api-key-from-auth-source "api.openai.com" "apikey-aider"))
  (setq openai-apikey-aider (api-key-from-auth-source "api.openai.com" "apikey-aider"))
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))

  ;; https://github.com/Aider-AI/aider/blob/main/aider/resources/model-settings.yml
  ;; See ~/.aider.conf.yml
  ;; "--no-gitignore"

  ;; $0.15 (4o is $2.50)
  ;; (setq aider-args `("--model" "gpt-4o-mini" "--api-key" ,(format "openai=%s" openai-apikey-aider)))

  ;; $1.10
  (setq aider-args `("--model" "o3-mini" "--api-key" ,(format "openai=%s" openai-apikey-aider)
                     "--reasoning-effort" "high"))

  ;; (setq aider-args '("--model" "ollama_chat/hf.co/sm54/Mistral-Small-24B-Instruct-2501-Q4_K_M-GGUF:latest"
  ;;                    "--edit-format" "diff"
  ;;                    "--editor-edit-format" "editor-diff"))
  ;; (setq aider-args '("--model" "ollama_chat/deepseek-r1:7b"
  ;;                    "--edit-format" "diff"
  ;;                    "--editor-model" "ollama_chat/hf.co/sm54/Mistral-Small-24B-Instruct-2501-Q4_K_M-GGUF:latest"
  ;;                    "--editor-edit-format" "editor-diff"))

  )

(provide 'setup-aider)
