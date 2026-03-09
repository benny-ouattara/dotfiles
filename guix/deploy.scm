(use-modules (gnu) (guix deployments) (guix remote))
(use-service-modules networking ssh)

(list (machine
        (operating-system (load "otter-system.scm"))
        (environment managed-host-environment-type)
        (configuration (machine-ssh-configuration
                         (host-name "") ; Your remote IP
                         (identity "/home/ben/.ssh/id_rsa")
                         (user "root")))))
