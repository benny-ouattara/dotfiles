(define-module (beno services openwebui)
  #:use-module (guix gexp)
  #:use-module (guix records)            ; for match-record
  #:use-module (guix store)              ; for text-file
  #:use-module (gnu services)            ; for service-type
  #:use-module (gnu services shepherd)   ; for shepherd-service-type
  #:use-module (gnu packages containers) ; for podman
  #:use-module (gnu packages shells)     ; for zsh
  #:use-module (gnu system shadow)       ; for account-service-type
  #:use-module (gnu system accounts)     ; for user-account, user-group
  #:export (openwebui-service-type))

(define %openwebui-accounts
  (list (user-group
          (name "openwebui")
          (system? #t))
        (user-account
          (name "openwebui")
          (comment "Openwebui Podman User")
          (system? #t)
          (group "openwebui")
          (home-directory "/home/openwebui")
          (shell (file-append zsh "/bin/zsh"))
          (supplementary-groups '("cgroup" "wheel" "netdev")))))

(define-record-type* <openwebui-configuration>
  openwebui-configuration
  make-openwebui-configuration
  openwebui-configuration?
  (runner openwebui-runner
          (default podman))
  (package openwebui-package
           (default "ghcr.io/open-webui/open-webui:main"))
  (group openwebui-group
         (default "openwebui"))
  (user openwebui-user
        (default "openwebui"))
  (log openwebui-log
       (default "/var/log/openwebui.log"))
  (name openwebui-name
        (default "openwebui"))
  (volume openwebui-volume
          (default "openwebui-data-stable:/app/backend/data:rw,z"))
  (port openwebui-port
        (default "5000:8080"))
  (env-file openwebui-env-file
            (default (plain-file "openwebui-env"
                                 "OLLAMA_BASE_URL=http://host.containers.internal:11434"))))

(define (openwebui-shepherd-service config) 
  (match-record config <openwebui-configuration> (runner package group user name volume port log env-file)
    (list
     (shepherd-service
       (provision '(openwebui))
       (documentation "Run openwebui")
       (requirement '(networking user-homes))
       (start #~(make-forkexec-constructor
                 (list
                  #$(file-append runner "/bin/podman") ; Absolute path to podman
                  "run" 
                  "--rm"                ; Clean up container on stop
                  "--replace"
                  "--name" #$name
                  "--init"
                  "--env-file" #$env-file
                  ;; "-e" "OLLAMA_BASE_URL=http://host.containers.internal:11434"
                  "-v" #$volume
                  "-p" #$port
                  #$package)
                 #:user #$user
                 #:group #$group
                 #:log-file #$log
                 #:environment-variables (list "HOME=/home/openwebui")))
       (stop #~(lambda _
                 (format #t "Stopping openwebui container...\n")
                 (system* #$(file-append runner "/bin/podman") "kill" #$name)
                 #f))))))

(define-public openwebui-service-type
  (service-type
    (name 'openwebui)
    (extensions
     (list (service-extension shepherd-root-service-type
                              openwebui-shepherd-service)
           (service-extension account-service-type
                              (const %openwebui-accounts))))
    (default-value (openwebui-configuration))
    (description "Launch the openwebui server")))
