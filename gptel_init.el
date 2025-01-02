;; Akash Chat as the default
(setq
 gptel-model 'Meta-Llama-3-1-405B-Instruct-FP8
 gptel-backend
 (gptel-make-openai "akash-chat"           ;Any name you want
   :host "chatapi.akash.network"
   :key (getenv "AKASH_CHAT_API_KEY")
   :endpoint "/api/v1/chat/completions"
   :stream t
   :models '(
	     Meta-Llama-3-1-405B-Instruct-FP8)))


;; mistral ai
(gptel-make-openai "MistralAI"           ;Any name you want
  :host "api.mistral.ai"
  :key (getenv "MISTRAL_API_KEY")              ;can be a function that returns the key
  :endpoint "/v1/chat/completions"
  :stream t
  :models '(
            open-mistral-nemo))
