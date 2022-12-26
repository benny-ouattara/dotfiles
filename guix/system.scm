;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (nongnu packages linux)
             (gnu packages lisp)
             (gnu packages wm)
             (gnu packages fonts)
             (gnu packages shells)
             (gnu packages package-management)
             (gnu packages pulseaudio)
             (gnu system setuid))
(use-service-modules
 cups
 desktop
 networking
 ssh
 ;; sddm
 xorg
 syncthing
 monitoring
 pm)

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %modified-desktop-services
  (modify-services %desktop-services
    (delete gdm-service-type)
    (guix-service-type config =>
                       (guix-configuration (inherit config)
                                           (substitute-urls
                                            (append (list "https://substitutes.nonguix.org")
                                                    %default-substitute-urls))
                                           (authorized-keys
                                            (append (list (local-file "./signing-key.pub"))
                                                    %default-authorized-guix-keys))))
    (slim-service-type config =>
                       (slim-configuration (inherit config)
                                           (default-user "ben")))
    (elogind-service-type config =>
                          (elogind-configuration (inherit config)
                                                 (handle-lid-switch-external-power 'suspend)))
    (udev-service-type config =>
                       (udev-configuration (inherit config)
                                           (rules (cons %backlight-udev-rule
                                                        (udev-configuration-rules config)))))))

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
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

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
    (list (list stumpwm "lib"))         ; use lib output of stumpwm package
    (list
     (specification->package "stumpwm-with-slynk")
     (specification->package "stow")
     (specification->package "pulseaudio")
     (specification->package "sbcl")
     (specification->package "awesome")
     (specification->package "i3-wm")
     (specification->package "i3status")
     (specification->package "dmenu")
     (specification->package "st")
     (specification->package "ratpoison")
     (specification->package "xterm")
     (specification->package "emacs-next")
     (specification->package "emacs-exwm")
     (specification->package
      "emacs-desktop-environment")
     (specification->package "nss-certs"))
    %base-packages))
  (services
   (append
    (list
     ;; (service slim-service-type)
     (service thermald-service-type)
     (service darkstat-service-type
              (darkstat-configuration
               (interface "wlp0s20f3")))
     (bluetooth-service #:auto-enable? #t)
     (service syncthing-service-type
              (syncthing-configuration (user "ben")))
     (service gnome-desktop-service-type)
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
       (keyboard-layout keyboard-layout))))
    %modified-desktop-services))
  (name-service-switch %mdns-host-lookup-nss) ;; Allow resolution of '.local' host names with mDNS.
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets (list "/boot/efi"))
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
