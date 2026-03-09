;; NOTE: otter uses (px services networking) definition of tailscale
;; This service is copied from jaza definition and simply a reference
(define-module (beno services tailscale)
  #:use-module (guix gexp)
  #:use-module (gnu services)               ; for service-type
  #:use-module (gnu services shepherd)      ; for home-shepherd-service-type
  #:use-module (px packages networking)     ; for tailscaled
  #:use-module (gnu services configuration) ; for define-configuration
  #:use-module (guix records)               ; for match-record-lambda
  #:use-module (gnu packages linux)         ; for iptables-nft
  #:export (tailscale-service-type))

(define-configuration tailscale-configuration
  (tailscale
   (file-like tailscale)
   "The tailscale package to use.")

  (iptables
   (file-like iptables-nft)
   "The iptables package to use.")

  (log-file
   (string "/var/log/tailscaled.log")
   "Path to log file.")

  (socket
   (string "/var/run/tailscale/tailscaled.sock")
   "Path of the service UNIX socket.")

  (state-directory
   (string "/var/lib/tailscale")
   "Path to directory for storage of config state, TLS certs, temporary incoming
Taildrop files, etc.  If empty, it's derived from @code{state-file} when
possible.")

  (upload-log?
   (boolean #f)
   "Whether to upload logs or not, technical support is also disabled when set
to #f.")

  (verbosity
   (integer 0)
   "Log verbosity level; 0 is default, 1 or higher are increasingly verbose.")

  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define tailscale-shepherd-service
  (match-record-lambda <tailscale-configuration>
      (tailscale iptables log-file socket state-directory
                 upload-log? verbosity extra-options)
    (let ((environment
           #~(list (string-append "PATH="
                                  (string-join
                                   '(#$(file-append iptables "/sbin")
                                     #$(file-append iproute "/sbin"))
                                   ":")))))
      (list (shepherd-service
              (documentation "Run tailscaled")
              (provision '(tailscaled))
              (requirement '(user-processes))
              (start
               #~(make-forkexec-constructor
                  (list
                   #$(file-append tailscale "/bin/tailscaled")
                   #$@(if upload-log?
                          '()
                          '("-no-logs-no-support"))
                   "-socket" #$socket
                   "-statedir" #$state-directory
                   "-verbose" #$(number->string verbosity)
                   #$@extra-options)
                  #:environment-variables #$environment
                  #:log-file #$log-file))
              (stop #~(make-kill-destructor)))))))

(define-public tailscale-service-type
  (service-type
   (name 'tailscaled)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-shepherd-service)
          (service-extension profile-service-type
                             (compose list tailscale-configuration-tailscale))))
   (default-value (tailscale-configuration))
   (description "Run tailscaled.")))
