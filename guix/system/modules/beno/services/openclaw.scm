(define-module (beno services openclaw)
  #:use-module (guix gexp)
  #:use-module (gnu services)            ; for service-type
  #:use-module (gnu services shepherd)   ; for home-shepherd-service-type
  #:use-module (gnu packages containers) ; for podman
  #:export (openclaw-service-type))

(define (openclaw-shepherd-service config) 
  (list
   (shepherd-service
     (provision '(openclaw))
     (documentation "Run openclaw")
     (requirement '(networking user-homes))
     (start #~(make-forkexec-constructor
               (list
                #$(file-append podman "/bin/podman") ; Absolute path to podman
                "run" 
                "--rm"                  ; Clean up container on stop
                "--replace"
                "--name" "openclaw"
                "--init"
                "--env-file" "/home/openclaw/.openclaw/.env"
                "--userns" "keep-id"   ; Important for rootless file permissions
                "--user" "978:970"     ; Map openclaw UID within container
                "-e" "HOME=/home/node"
                "-e" "TERM=xterm-256color"
                "-v" "/home/openclaw/.openclaw:/home/node/.openclaw:rw,z"
                "-p" "18789:18789"
                "-p" "18790:18790"
                "openclaw:local")
               #:user "openclaw"
               #:group "openclaw"
               #:log-file "/var/log/openclaw.log"
               #:environment-variables 
               (list "HOME=/home/openclaw"
                     (string-append "PATH=" (getenv "PATH")))))
     (stop #~(lambda _
               (format #t "Stopping openclaw container...\n")
               (system* #$(file-append podman "/bin/podman") "kill" "openclaw")
               #f)))))

(define-public openclaw-service-type
  (service-type
    (name 'openclaw)
    (extensions
     (list (service-extension shepherd-root-service-type
                              openclaw-shepherd-service)))
    (default-value #f)
    (description "Launch the openclaw gateway server")))
