(use-modules (gnu)
             (gnu artwork)
             (gnu packages fonts)
             (gnu packages shells)
             (gnu packages vim)
             (gnu packages package-management)
             (gnu system shadow)
             (gnu services databases)
             (guix gexp)
             (guix channels)
             (srfi srfi-1))
(use-service-modules
 desktop
 networking
 ssh
 virtualization
 cuirass
 mcron)

(define %motd
  (plain-file "motd" "Welcome to jaza cicd!\n\n"))

(define %tamzen-console-font
  (file-append font-tamzen "/share/kbd/consolefonts/Tamzen10x20.psf"))

(define %modified-desktop-services
  (modify-services %desktop-services
    (delete console-font-service-type)
    (login-service-type config =>
                        (login-configuration (inherit config)
                                             (motd %motd)))
    (guix-service-type config =>
                       (guix-configuration (inherit config)
                                           (substitute-urls
                                            (append (list "https://substitutes.nonguix.org")
                                                    %default-substitute-urls))
                                           (authorized-keys
                                            (append (list (local-file "./nonguix-key.pub")
                                                          (local-file "./cuirass-key.pub"))
                                                    %default-authorized-guix-keys))))))

(define garbage-collector-job
  #~(job '(next-hour) "guix gc --collect-garbage"))

(operating-system
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "ben")
  (users (cons* (user-account
                 (name "ben")
                 (comment "Ben Ouattara")
                 (group "users")
                 (shell (file-append zsh "/bin/zsh"))
                 (home-directory "/home/ben")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video")))
                (user-account
                 (name "guest")
                 (comment "Guest")
                 (password "")
                 (group "users")
                 (shell (file-append zsh "/bin/zsh"))
                 (home-directory "/home/guest")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video")))
                (user-account
                 (name "danfodio")
                 (comment "Dan Fodio")
                 (group "users")
                 (shell (file-append zsh "/bin/zsh"))
                 (home-directory "/home/danfodio")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=NOPASSWD: ALL\n"))
  (packages
   (append
    (list
     (specification->package "font-tamzen")
     (specification->package "neovim")
     (specification->package "emacs-next")
     (specification->package "nss-certs"))
    %base-packages))
  (services
   (append
    (list
     (simple-service 'system-cron-jobs
                     mcron-service-type
                     (list garbage-collector-job))
     (service console-font-service-type
              (map (lambda (tty)
                     (cons tty %tamzen-console-font))
                   '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
     (service openssh-service-type
              ;; (openssh-configuration
              ;;  (x11-forwarding? #t)
              ;;  (permit-root-login 'prohibit-password)
              ;;  (password-authentication? #f)
              ;;  ;; (authorized-keys
              ;;  ;;  `(("ben" ,(local-file "ben.pub"))))
              ;;  )
              ))
    %modified-desktop-services))
  (name-service-switch %mdns-host-lookup-nss) ;; Allow resolution of '.local' host names with mDNS.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sda"))
               (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                       (target (uuid
                                "e1da8300-e631-4435-a3f4-09d64a9bdbab")))))
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "27BA-B168"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "586e2f25-cb51-4d29-9282-bdca6b1ba29c"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
