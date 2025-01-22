(use-package gptel
  :config (setq
           gptel-model 'llama3.3:latest
           gptel-backend (gptel-make-ollama "Ollama"
                           :host "localhost:11434"
                           :stream t
                           :models '(llama3.2:latest qwen2.5-coder:latest))))
