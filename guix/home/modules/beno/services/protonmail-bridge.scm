(define-module (beno services protonmail-bridge)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:export (home-protonmail-bridge-service-type
            protonmail-bridge-configuration))

(define-record-type* <protonmail-bridge-configuration>
  protonmail-bridge-configuration
  make-protonmail-bridge-configuration
  protonmail-bridge-configuration?
  (package protonmail-bridge-package
           (default "/home/ben/.nix-profile/bin/protonmail-bridge"))
  (log protonmail-bridge-log
       (default "/home/ben/.local/state/log/protonmail-bridge.log")))

(define (home-protonmail-bridge-shepherd-service config)
  (match-record config <protonmail-bridge-configuration> (package log)
    (list (shepherd-service
            (provision '(protonmail-bridge))
            (documentation "Run Proton Mail Bridge")
            (start #~(make-forkexec-constructor
                      (list "/bin/sh" "-c"
                            (string-append
                             "DBUS_ADDR=$(ss -xlp 2>/dev/null"
                             " | grep dbus-daemon"
                             " | grep -oP '/tmp/dbus-\\S+'"
                             " | head -1); "
                             "export DBUS_SESSION_BUS_ADDRESS=\"unix:path=$DBUS_ADDR\"; "
                             "exec " #$package " --noninteractive"))
                      #:log-file #$log))
            (stop #~(make-kill-destructor))))))

(define-public home-protonmail-bridge-service-type
  (service-type (name 'protonmail-bridge)
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   home-protonmail-bridge-shepherd-service)))
                (default-value (protonmail-bridge-configuration))
                (description
                 "Run Proton Mail Bridge for IMAP/SMTP access")))
