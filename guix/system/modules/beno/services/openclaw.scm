(define-module (beno services openclaw)
  #:use-module (guix gexp)
  #:use-module (guix records)            ; for match-record
  #:use-module (gnu services)            ; for service-type, activation-service-type
  #:use-module (gnu services shepherd)   ; for home-shepherd-service-type
  #:use-module (gnu packages containers) ; for podman
  #:use-module (gnu system shadow)       ; for account-service-type
  #:use-module (gnu packages admin)      ; for shadow
  #:export (openclaw-service-type
            openclaw-configuration))

(define %openclaw-accounts
  (list (user-group
          (name "openclaw")
          (system? #t))
        (user-account
          (name "openclaw")
          (comment "Openclaw Podman User")
          (system? #t)
          (group "openclaw")
          (home-directory "/home/openclaw")
          (shell (file-append shadow "/sbin/nologin"))
          (supplementary-groups '("cgroup" "wheel" "netdev")))))

(define-record-type* <openclaw-configuration>
  openclaw-configuration
  make-openclaw-configuration
  openclaw-configuration?
  (runner openclaw-runner
          (default podman))
  (package openclaw-package
           (default "ghcr.io/openclaw/openclaw:latest"))
  (name openclaw-name
        (default "openclaw"))
  (group openclaw-group
         (default "openclaw"))
  (user openclaw-user
        (default "openclaw"))
  (log openclaw-log
       (default "/var/log/openclaw.log"))
  (volume openclaw-volume
          (default "/home/openclaw/.openclaw:/home/node/.openclaw:rw,z"))
  (gateway-port openclaw-gateway-port
                (default "18789:18789"))
  (bridge-port openclaw-bridge-port
               (default "18790:18790"))
  (env-file openclaw-env-file
            (default "/home/openclaw/.openclaw/.env"))
  (env-vars openclaw-env-vars
            (default #~(list "HOME=/home/openclaw"
                             (string-append "PATH=" (getenv "PATH"))))))

(define (openclaw-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "openclaw")))
        (mkdir-p "/home/openclaw/.openclaw/workspace")
        ;; Set ownership to the openclaw user (UID/GID)
        (chown "/home/openclaw/.openclaw" (passwd:uid user) (passwd:gid user))
        (chown "/home/openclaw/.openclaw/workspace" (passwd:uid user) (passwd:gid user))
        ;; Only openclaw owner can rwx
        (chmod "/home/openclaw/.openclaw" #o700)
        ;; Allow members of group "users" to also rwx
        (system* "setfacl" "-R" "-m" "g:users:rwx" "/home/openclaw")
        (system* "setfacl" "-d" "-m" "g:users:rwx" "/home/openclaw"))))

(define (openclaw-shepherd-service config) 
  (match-record config <openclaw-configuration>
                (runner package name group user log volume gateway-port bridge-port env-file env-vars)
    (list
     (shepherd-service
       (provision '(openclaw))
       (documentation "Run openclaw")
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
                  "--userns" "keep-id" ; Important for rootless file permissions
                  "--user" "978:970"   ; Map openclaw UID within container
                  "-e" "HOME=/home/node"
                  "-e" "TERM=xterm-256color"
                  "-v" #$volume
                  "-p" #$gateway-port
                  "-p" #$bridge-port
                  #$package)
                 #:user #$user
                 #:group #$group
                 #:log-file #$log
                 #:environment-variables #$env-vars))
       (stop #~(lambda _
                 (format #t "Stopping openclaw container...\n")
                 (system* #$(file-append podman "/bin/podman") "kill" "openclaw")
                 #f))))))

(define-public openclaw-service-type
  (service-type
    (name 'openclaw)
    (extensions
     (list (service-extension shepherd-root-service-type
                              openclaw-shepherd-service)
           (service-extension activation-service-type
                              openclaw-activation)
           (service-extension account-service-type
                              (const %openclaw-accounts))))
    (default-value (openclaw-configuration))
    (description "Launch the openclaw gateway server")))
