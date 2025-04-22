;; Akash Chat as the default
(setq
 gptel-model 'Meta-Llama-4-Maverick-17B-128E-Instruct-FP8
 gptel-backend
 (gptel-make-openai "akash-chat"           ;Any name you want
   :host "chatapi.akash.network"
   :key (dot-env-get 'AKASH_CHAT_API_KEY)
   :endpoint "/api/v1/chat/completions"
   :stream t
   :models '(Meta-Llama-4-Maverick-17B-128E-Instruct-FP8
	     DeepSeek-R1-Distill-Llama-70B
	     DeepSeek-R1)))


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
