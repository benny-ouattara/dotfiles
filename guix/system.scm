(use-modules (gnu)
             (nongnu packages linux)
             (gnu artwork)
             (gnu packages fonts)
             (gnu packages lisp)
             (gnu packages guile-xyz)
             (gnu packages ssh)
             (gnu packages wm)
             (gnu packages fonts)
             (gnu packages shells)
             (gnu packages package-management)
             (gnu packages pulseaudio)
             (gnu system setuid)
             (gnu system shadow)
             (gnu services databases)
             (gnu services)
             (jazacash service)
             (gnu packages audio)
             (guix gexp)
             (guix channels)
             (srfi srfi-1))
(use-service-modules
 cups
 desktop
 networking
 ssh
 sddm
 xorg
 syncthing
 monitoring
 pm
 virtualization
 cuirass
 mcron)

;; (define channels
;;   (list
;;    (channel
;;     (name 'nonguix)
;;     (url "https://gitlab.com/nonguix/nonguix")
;;     (branch "master")
;;     (commit "831f3ff14260e20d4da31b707515891eeb49e752")
;;     (introduction
;;      (make-channel-introduction
;;       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
;;       (openpgp-fingerprint
;;        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
;;    (channel
;;     (name 'jazacash)
;;     (url  "git@github.com:jazafund/jazacash.git")
;;     (branch "develop"))
;;    (channel
;;     (name 'guix)
;;     (url "https://git.savannah.gnu.org/git/guix.git")
;;     (commit "5217ea6d45bef053844d8360a06252b9436783b3")
;;     (branch "master"))))

;; Allow members of the "video" group to change the screen brightness.
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

(define %modified-desktop-services
  (modify-services %desktop-services
    (delete console-font-service-type)  ;; provide other console fonts below
    (delete gdm-service-type)
    (login-service-type config =>
                        (login-configuration (inherit config)
                                             (motd %beno-motd)))
    (guix-service-type config =>
                       (guix-configuration (inherit config)
                                           ;; (channels channels)
                                           ;; (guix (guix-for-channels channels))
                                           (substitute-urls
                                            (append (list "https://substitutes.nonguix.org"
                                                          "http://substitutes.jaza.cash")
                                                    %default-substitute-urls))
                                           (authorized-keys
                                            (append (list (local-file "./nonguix-key.pub")
                                                          (local-file "./cuirass-key.pub"))
                                                    %default-authorized-guix-keys))))
    (elogind-service-type config =>
                          (elogind-configuration (inherit config)
                                                 (handle-lid-switch-external-power 'suspend)))
    (udev-service-type config =>
                       (udev-configuration (inherit config)
                                           (rules (cons %backlight-udev-rule
                                                        (udev-configuration-rules config)))))))

(define garbage-collector-job
  #~(job '(next-hour) "guix gc --collect-garbage"))

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
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
                  '("wheel" "netdev" "audio" "video" "lp")))
                (user-account
                 (name "danfodio")
                 (comment "Dan Fodio")
                 (group "users")
                 (shell (file-append zsh "/bin/zsh"))
                 (home-directory "/home/danfodio")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "lp")))
                %base-user-accounts))
  (setuid-programs
   (append (list (setuid-program
                  (program (file-append stumpwm+slynk "/bin/stumpwm"))))
           %setuid-programs))
  (packages
   (append
    ;; (list (list stumpwm "lib"))
                                        ; use lib output of stumpwm package
    (list
     (specification->package "exfat-utils")
     ;; (specification->package "exfat-utils")
     (specification->package "xf86-input-libinput")
     (specification->package "sugar-light-sddm-theme")
     (specification->package "sugar-dark-sddm-theme")
     (specification->package "chili-sddm-theme")
     (specification->package "lightdm-gtk-greeter")
     (specification->package "font-tamzen")
     (specification->package "bluez")
     (specification->package "bluez-alsa")
     (specification->package "stumpwm-with-slynk")
     (specification->package "stow")
     (specification->package "pulseaudio")
     (specification->package "sbcl")
     (specification->package "awesome")
     (specification->package "i3-wm")
     (specification->package "i3status")
     ;; (specification->package "postgresql")
     (specification->package "dmenu")
     (specification->package "st")
     (specification->package "ratpoison")
     (specification->package "xterm")
     (specification->package "emacs-next")
     (specification->package "emacs-exwm")
     (specification->package "guile-jwt")
     (specification->package "guile-fibers")
     (specification->package "guile-sqlite3")
     (specification->package "emacs-desktop-environment"))
    %base-packages))
  (services
   (append
    (list
     (simple-service 'system-cron-jobs
                     mcron-service-type
                     (list garbage-collector-job))
     (simple-service 'jazacash-etc-files etc-service-type
                     `(("jazacash" ,(local-file (format #f "~a/secrets" (getenv "HOME"))
                                                "jaza-secrets"
                                                #:recursive? #t))))
     (service jazacash-ci-service-type)
     ;; (service postgresql-service-type)
     (service libvirt-service-type
              (libvirt-configuration
               (unix-sock-group "libvirt")
               (tls-port "16555")))
     (service console-font-service-type
              (map (lambda (tty)
                     (cons tty %beno-console-font))
                   '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
     (service thermald-service-type)
     (service darkstat-service-type
              (darkstat-configuration
               (interface "wlp0s20f3")))
     (bluetooth-service #:auto-enable? #t)
     (service syncthing-service-type
              (syncthing-configuration (user "ben")))
     (service openssh-service-type
              (openssh-configuration
               (x11-forwarding? #t)
               (permit-root-login 'prohibit-password)
               (password-authentication? #f)
               ;; (authorized-keys
               ;;  `(("ben" ,(local-file "ben.pub"))))
               ))
     (service tor-service-type)
     (service cups-service-type)
     (set-xorg-configuration
      (xorg-configuration
       (keyboard-layout keyboard-layout)
       (extra-config (list %xorg-libinput-config)))
      sddm-service-type)
     (service sddm-service-type
              (sddm-configuration
               ;; valid values are elarun, maldives or maya, chili, sugar-light, sugar-dark
               (theme "sugar-light")
               )))
    %modified-desktop-services))
  (name-service-switch %mdns-host-lookup-nss) ;; Allow resolution of '.local' host names with mDNS.
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets (list "/boot/efi"))
    (theme (grub-theme
            (resolution '(1920 . 1080))
            (image (file-append
                    %artwork-repository
                    "/grub/GuixSD-fully-black-16-9.svg"))))
    (keyboard-layout keyboard-layout)))
  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device
             (uuid "37d46512-763b-4c4a-a119-641eea889ff2"
                   'ext4))
            (type "ext4"))
          (file-system
            (mount-point "/boot/efi")
            (device (uuid "5F8E-0C2D" 'fat32))
            (type "vfat"))
          %base-file-systems)))
