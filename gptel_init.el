;; Open Router as the default
(setq gptel-model   'deepseek/deepseek-v4-flash
      gptel-backend
      (gptel-make-openai "openrouter"               ;Any name you want
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key (dot-env-get 'OPENROUTER_API_KEY)
        :models '(deepseek/deepseek-v4-flash
		  deepseek/deepseek-v4-pro
		  deepseek/deepseek-v3.2-speciale
		  x-ai/x-ai/grok-4.20
		  x-ai/x-ai/grok-4.3
		  google/gemini-3-pro-preview)))


(gptel-make-openai "akash-ml"           ;Any name you want
  :host "api.akashml.com"
  :key (dot-env-get 'AKASH_ML_API_KEY)
  :endpoint "v1/chat/completions"
  :stream t
  :models '(deepseek-ai/DeepSeek-V4-Flash
	    meta-llama/Llama-3.3-70B-Instruct
	    Qwen/Qwen3-30B-A3B
	    MiniMaxAI/MiniMax-M2.5))

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
