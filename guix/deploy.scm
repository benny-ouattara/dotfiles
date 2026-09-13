(use-modules (gnu) (guix deployments) (guix remote))

;; Driven by `make deploy REMOTE_HOST=... DEPLOY_OS=...'
(define (required-env name)
  (or (getenv name)
      (error (string-append name " is not set (see make deploy)"))))

(list (machine
        (operating-system (load (required-env "DEPLOY_OS")))
        (environment managed-host-environment-type)
        (configuration (machine-ssh-configuration
                         (host-name (required-env "DEPLOY_HOST"))
                         (identity "/home/ben/.ssh/id_rsa")
                         (user "root")))))
