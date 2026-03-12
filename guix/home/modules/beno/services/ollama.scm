(define-module (beno services ollama)
  #:use-module (guix gexp)
  #:use-module (guix records)                 ; for match-record
  #:use-module (gnu home services)            ; for service-type
  #:use-module (gnu home services shepherd)   ; for home-shepherd-service-type
  #:use-module (px packages ai)               ; for ollama
  #:export (home-ollama-service-type
            ollama-configuration
            ollama-test))

(define-record-type* <ollama-configuration>
  ollama-configuration
  make-ollama-configuration
  ollama-configuration?
  (package ollama-package
           (default "/home/ben/.nix-profile/bin/ollama"))
  (env ollama-env
       (default #~(list "HOME=/home/ben"
                      "OLLAMA_HOST=0.0.0.0"
                      "OLLAMA_DEBUG=4"
                      "OLLAMA_KEEP_ALIVE=6h"
                      "HSA_OVERRIDE_GFX_VERSION=11.0.2"
                      "OLLAMA_MODELS=/home/ben/.ollama/models")))
  (log ollama-log
       (default "/home/ben/.local/state/log/ollama.logs")))

(define (home-ollama-shepherd-service config)
  (match-record config <ollama-configuration> (package env log)
    (list (shepherd-service
            (provision '(ollama))
            (documentation "Start the ollama server")
            (start #~(make-forkexec-constructor
                      (list #$package "serve")
                      #:environment-variables #$env
                      #:log-file #$log))
            (stop #~(make-kill-destructor))))))

(define-public home-ollama-service-type
  (service-type (name 'ollama)
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   home-ollama-shepherd-service)))
                (default-value (ollama-configuration))
                (description
                 "Launch the ollama server so running ollama works out of the box.")))
