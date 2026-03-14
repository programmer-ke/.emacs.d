;; Open Router as the default
(setq gptel-model   'deepseek/deepseek-v3.2
      gptel-backend
      (gptel-make-openai "openrouter"               ;Any name you want
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key (dot-env-get 'OPENROUTER_API_KEY)
        :models '(deepseek/deepseek-v3.2
		  deepseek/deepseek-v3.2-speciale
		  x-ai/grok-4.1-fast
		  x-ai/grok-code-fast-1
		  google/gemini-3-pro-preview)))


(gptel-make-openai "akash-ml"           ;Any name you want
  :host "api.akashml.com"
  :key (dot-env-get 'AKASH_ML_API_KEY)
  :endpoint "v1/chat/completions"
  :stream t
  :models '(meta-llama/Llama-3.3-70B-Instruct
	    Qwen/Qwen3-30B-A3B
	    MiniMaxAI/MiniMax-M2.5
	    deepseek-ai/DeepSeek-V3.2))

;; mistral ai
(gptel-make-openai "mistral-ai"           ;Any name you want
  :host "api.mistral.ai"
  :key (dot-env-get 'MISTRAL_API_KEY)              ;can be a function that returns the key
  :endpoint "/v1/chat/completions"
  :stream t
  :models '(
            open-mistral-nemo
	    codestral-latest))

(setq
 gptel-default-mode 'org-mode
 gptel-track-media t)
