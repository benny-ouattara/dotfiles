(use-modules (gnu)
             (nongnu packages linux)
             (gnu artwork)
             (gnu packages admin)
             (gnu packages fonts)
             (gnu packages lisp)
             (gnu packages guile-xyz)
             (gnu packages ssh)
             (gnu packages base)
             (gnu packages wm)
             (gnu packages fonts)
             (gnu packages shells)
             (gnu packages docker)
             (gnu packages xorg)
             (gnu packages emacs)
             (gnu packages wm)
             (gnu packages containers)
             (gnu packages tls)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages audio)
             (gnu packages vim)
             (gnu packages display-managers)
             (gnu packages package-management)
             (gnu packages pulseaudio)
             (gnu system setuid)
             (gnu system accounts)
             (gnu system shadow)
             (gnu services)
             (jazacash service)
             (guix gexp)
             (guix channels)
             (srfi srfi-1))
(use-service-modules
 shepherd
 cups
 desktop
 networking
 ssh
 sddm
 xorg
 syncthing
 monitoring
 pm
 containers
 virtualization
 cuirass
 mcron
 docker
 databases)

(define %channels
  (list
   (channel
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix")
     (branch "master")
     (commit "1980960f932063f42f97ad3be4b020f68d24e62b")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
     (name 'jazacash)
     (url  "git@github.com:jazafund/jazacash.git")
     (branch "develop"))
   (channel
     (name 'pantherx)
     (url "https://codeberg.org/gofranz/panther.git")
     (branch "master")
     (commit "9956fddff1e2f024d592ec8c4be37096bb16ea8b")
     (introduction
      (make-channel-introduction
       "54b4056ac571611892c743b65f4c47dc298c49da"
       (openpgp-fingerprint
        "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
   (channel
     (name 'guix)
     (url "https://git.savannah.gnu.org/git/guix.git")
     (branch "master")
     (commit
      "a29122743a67a453ca74042e00d521fffcbc3310")
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
  Option \"NaturalScrolling\" \"true\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define %beno-motd
  (plain-file "motd" "Hi Ben, welcome!\n\n"))

(define %beno-console-font
  (file-append font-tamzen "/share/kbd/consolefonts/Tamzen10x20.psf"))

(define %default-secrets `(("secrets.json" ,(local-file "/home/ben/Code/jazacash/aws/staging/secrets/staging.json"))))
(define secrets-config (jazacash-secrets-configuration (secret-files %default-secrets)))

(define %modified-desktop-services
  (modify-services %desktop-services
    (delete console-font-service-type) ;; provide other console fonts below
    (delete gdm-service-type)
    (login-service-type config =>
                        (login-configuration (inherit config)
                                             (motd %beno-motd)))
    (guix-service-type config =>
                       (guix-configuration (inherit config)
                                           (channels %channels)
                                           (guix (guix-for-channels %channels))
                                           (substitute-urls
                                            (append (list "https://substitutes.nonguix.org"
                                                          "https://substitutes.guix.gofranz.com"
                                                          ;; "http://substitutes.jazacash.com"
                                                          )
                                                    %default-substitute-urls))
                                           (authorized-keys
                                            (append (list (local-file "./nonguix-key.pub")
                                                          (local-file "./pantherx-key.pub")
                                        ;(local-file "./cuirass-key.pub")
                                                          )
                                                    %default-authorized-guix-keys))))
    (elogind-service-type config =>
                          (elogind-configuration (inherit config)
                                                 (handle-lid-switch-external-power 'suspend)))
    (udev-service-type config =>
                       (udev-configuration (inherit config)
                                           (rules (cons %backlight-udev-rule
                                                        (udev-configuration-rules config)))))))

(define openclaw-shepherd-service
  (shepherd-service
    (provision '(openclaw))
    (requirement '(networking user-homes))

    (start #~(make-forkexec-constructor
              (list #$(file-append podman "/bin/podman") ; Absolute path to podman
                    "run" 
                    "--rm"                       ; Clean up container on stop
                    "--replace"
                    "--name" "openclaw"
                    "--init"
                    "--env-file" "/home/openclaw/.openclaw/.env"
                    "--userns" "keep-id"         ; Important for rootless file permissions
                    "--user" "978:970"           ; Map openclaw UID within container
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
              #f))))

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (locale-libcs (list glibc-2.35 glibc-2.39 (canonical-package glibc)))
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "otter")
  (groups (cons* (user-group
                   (name "openclaw")
                   (system? #t))
                 %base-groups))
  (users (cons* (user-account
                  (name "ben")
                  (comment "Primary User")
                  (group "users")
                  (shell (file-append zsh "/bin/zsh"))
                  (home-directory "/home/ben")
                  (supplementary-groups '("cgroup" "wheel" "netdev" "audio" "video")))
                (user-account
                  (name "openclaw")
                  (comment "Openclaw Podman User")
                  (system? #t)
                  (group "openclaw")
                  (home-directory "/home/openclaw")
                  ;; (shell (file-append shadow "/sbin/nologin"))
                  (shell (file-append zsh "/bin/zsh"))
                  (supplementary-groups '("cgroup" "wheel" "netdev")))
                %base-user-accounts))
(sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
ben ALL=(openclaw) NOPASSWD: ALL\n"))
  (setuid-programs
   (append (list (setuid-program
                  (program (file-append stumpwm+slynk "/bin/stumpwm"))))
           %setuid-programs))
  (packages (cons*
	         xf86-input-libinput
	         emacs-next
             neovim
	         sbcl
	         xterm
             openssl
             stumpwm+slynk
             sbcl-stumpwm-swm-gaps
             sbcl-stumpwm-screenshot
             sbcl-stumpwm-pass
             sbcl-stumpwm-pamixer
	         stumpish
             podman
             podman-compose
             guile-gnutls
             guile-gcrypt
             guile-git
             guile-fibers
             guile-jwt
	         sugar-light-sddm-theme
             dexy-color-sddm-theme
             chili-sddm-theme
	         %base-packages))
  (services
   (append (list
            (service jazacash-secrets-service-type secrets-config)
            (service jazacash-ci-service-type)
            (service mysql-service-type)
            (service containerd-service-type)
            ;; (service docker-service-type)
            (service iptables-service-type) ; required for podman-service
            (simple-service 'openclaw-file-setup
                            activation-service-type
                            #~(begin
                                (use-modules (guix build utils))
                                (let ((user (getpwnam "openclaw")))
                                  (mkdir-p "/home/openclaw/.openclaw/workspace")
                                  ;; Set ownership to the openclaw user (UID/GID)
                                  (chown "/home/openclaw/.openclaw" (passwd:uid user) (passwd:gid user))
                                  (chown "/home/openclaw/.openclaw/workspace" (passwd:uid user) (passwd:gid user))
                                  (chmod "/home/openclaw/.openclaw" #o700))))
            (simple-service 'openclaw-service 
                            shepherd-root-service-type 
                            (list openclaw-shepherd-service))
            ;; (simple-service 'openclaw-log-rotation
            ;;                 rottlog-service-type
            ;;                 (list (log-rotator
            ;;                        (files (list "/var/log/openclaw.log"))
            ;;                        (frequency 'weekly)
            ;;                        (keep 4) 
            ;;                        (options '("sharedscripts" "compress")))))
            (service rootless-podman-service-type
                     (rootless-podman-configuration
                       (subgids
                        (list (subid-range (name "ben"))
                              (subid-range (name "openclaw"))))
                       (subuids
                        (list (subid-range (name "ben"))
                              (subid-range (name "openclaw"))))))
            (service tailscale-service-type)
            (service syncthing-service-type
                     (syncthing-configuration (user "ben")))
            (service gnome-desktop-service-type)
            (service openssh-service-type
                     (openssh-configuration
                       (permit-root-login 'prohibit-password)
                       (password-authentication? #f)
                       (authorized-keys
                        `(("root" ,(local-file "./keys/mac.pub"))
                          ("ben"  ,(local-file "./keys/mac.pub"))))))
            (set-xorg-configuration
             (xorg-configuration
               (keyboard-layout keyboard-layout)
               (extra-config (list %xorg-libinput-config)))
             sddm-service-type)
            (service sddm-service-type
                     (sddm-configuration
                       ;; valid values are elarun, maldives or maya, chili, sugar-light
                       (theme "chili"))))
           %modified-desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "893cebe6-5e30-4588-9c99-1a03389facd8")))))

  ;; sudo blkid to list UUIDs
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "D47F-3FB9"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "d690f4db-ddfc-45e7-9b77-c4a2e08892b3"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
