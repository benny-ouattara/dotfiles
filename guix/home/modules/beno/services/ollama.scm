(define-module (beno services ollama)
  #:use-module (guix gexp)
  #:use-module (gnu home services)            ; for service-type
  #:use-module (gnu home services shepherd)   ; for home-shepherd-service-type
  #:use-module (px packages ai)               ; for ollama
  #:export (home-ollama-service-type))

;; "OLLAMA_KEEP_ALIVE=24h"
(define (home-ollama-shepherd-service config)
  (list (shepherd-service
          (provision '(ollama))
          (documentation "Start the ollama server")
          (start #~(make-forkexec-constructor
                    (list "/home/ben/.nix-profile/bin/ollama" "serve")
                    #:environment-variables (list "HOME=/home/ben"
                                                  "OLLAMA_HOST=0.0.0.0"
                                                  "OLLAMA_DEBUG=4"
                                                  "HSA_OVERRIDE_GFX_VERSION=11.0.2"
                                                  "OLLAMA_MODELS=/home/ben/.ollama/models")
                    #:log-file "/home/ben/.local/state/log/ollama.logs"))
          (stop #~(make-kill-destructor)))))

(define-public home-ollama-service-type
  (service-type (name 'ollama)
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   home-ollama-shepherd-service)))
                (default-value #f)
                (description
                 "Launch the ollama server so running ollama works out of the box.")))
