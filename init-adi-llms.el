;; package --- init-adi-llms.el -*- lexical-binding: t -*-
;;; Separate Configuration for LLMs.

(defvar adi/llms-api-configs-file
  (expand-file-name "init-env-secrets.el" adi/dotemacs-dir))

(defun adi/get-env-secret (secret-env-key-name)
  (getenv secret-env-key-name))

(use-package gptel
  ;; ref: https://github.com/karthink/gptel?tab=readme-ov-file
  :config
  (load-file adi/llms-api-configs-file)
  (gptel-make-openai "Groq" ;; Groq offers an OpenAI compatible API
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (lambda () (adi/get-env-secret "GROQ_API_KEY")))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(smollm2:135m
              qwen2.5:1.5b
              qwen2.5-coder:1.5b)))

(provide 'adi-init-llms)
;;; adi-init-llms.el ends here

