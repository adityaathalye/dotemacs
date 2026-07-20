;; package --- init-adi-llms.el -*- lexical-binding: t -*-
;;; Separate Configuration for LLMs.

(defvar adi/llms-api-configs-file
  (expand-file-name "init-env-secrets.el" adi/dotemacs-dir))

(defun adi/get-env-secret (secret-env-key-name)
  (getenv secret-env-key-name))

(use-package gptel
  ;; ref: https://github.com/karthink/gptel?tab=readme-ov-file
  :config
  (setq
   gptel-model 'qwen3-coder:30b
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(;; NAME                  ID              SIZE    ; ollama list
                             qwen3.6:27b           ;; a50eda8ed977    17 GB
                             qwen3.6:latest        ;; 07d35212591f    23 GB
                             qwen3-coder:30b       ;; 06c1097efce0    18 GB
                             smollm2:135m          ;; 9077fe9d2ae1    270 MB
                             qwen2.5:1.5b          ;; 65ec06548149    986 MB
                             qwen2.5-coder:1.5b    ;; d7372fd82851    986 MB
                             )))

  ;; OPTIONAL configuration
  (load-file adi/llms-api-configs-file)
  (gptel-make-openai "Groq" ;; Groq offers an OpenAI compatible API
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (lambda () (adi/get-env-secret "GROQ_API_KEY"))))

(provide 'adi-init-llms)
;;; adi-init-llms.el ends here
