;; aider:
(use-package aider
  :load-path ("elisp/aider.el")
  :config
  ;; Aider wants chat models (not coder/FIM).
  (define-key global-map (kbd "C-c a") #'aider-transient-menu)
  ;; (setenv "OPENAI_API_KEY" (gptel-api-key-from-auth-source "api.openai.com" "apikey-aider"))
  (setq openai-apikey-aider (gptel-api-key-from-auth-source "api.openai.com" "apikey-aider"))
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))

  ;; https://github.com/Aider-AI/aider/blob/main/aider/resources/model-settings.yml
  ;; --no-gitignore
  (setq aider-args `("--model" "o3-mini" "--api-key" ,(format "openai=%s" openai-apikey-aider)))
  (setq aider-args `("--model" "gpt-4o-mini" "--api-key" ,(format "openai=%s" openai-apikey-aider)
                     "--edit-format" "diff"
                     "--editor-edit-format" "editor-diff"))
  ;; (setq aider-args '("--model" "ollama_chat/hf.co/sm54/Mistral-Small-24B-Instruct-2501-Q4_K_M-GGUF:latest"
  ;;                    "--edit-format" "diff"
  ;;                    "--editor-edit-format" "editor-diff"))
  ;; (setq aider-args '("--model" "ollama_chat/deepseek-r1:7b"
  ;;                    "--edit-format" "diff"
  ;;                    "--editor-model" "ollama_chat/hf.co/sm54/Mistral-Small-24B-Instruct-2501-Q4_K_M-GGUF:latest"
  ;;                    "--editor-edit-format" "editor-diff"))

  )

(provide 'setup-aider)