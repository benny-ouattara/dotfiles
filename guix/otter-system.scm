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
                                        ;(jazacash service)
             (guix gexp)
             (gnu packages audio)
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
                                                          (substitute-urls
                                                           (append (list "https://substitutes.nonguix.org"
                                        ; "http://substitutes.jazacash.com"
                                                                         )
                                                                   %default-substitute-urls))
                                                          (authorized-keys
                                                           (append (list ;(local-file "./nonguix-key.pub")
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

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/New_York")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "otter")
 (users (cons* (user-account
                (name "ben")
                (comment "Ben")
                (group "users")
                (shell (file-append zsh "/bin/zsh"))
                (home-directory "/home/ben")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (setuid-programs
  (append (list (setuid-program
                 (program (file-append stumpwm+slynk "/bin/stumpwm"))))
          %setuid-programs))
 (packages (append (list
                    (specification->package "xf86-input-libinput")
                    (specification->package "emacs-next")
                    (specification->package "sbcl")
                    (specification->package "xterm")
                    (specification->package "stumpwm-with-slynk")
                    (specification->package "sugar-light-sddm-theme")
                    (specification->package "guile-jwt")
                    (specification->package "guile-fibers")
                    (specification->package "guile-sqlite3"))
                   %base-packages))
 (services
  (append (list
           (service syncthing-service-type
                    (syncthing-configuration (user "ben")))
           (service gnome-desktop-service-type)
           (service openssh-service-type)
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
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "893cebe6-5e30-4588-9c99-1a03389facd8")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
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
