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
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "ben")

  ;; The list of user accounts ('root' is implicit).
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
  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list
		      (specification->package "xf86-input-libinput")
		      (specification->package "emacs-next")
		      (specification->package "sbcl")
		      (specification->package "xterm")
		      (specification->package "stumpwm-with-slynk")
                      (specification->package "sugar-light-sddm-theme")
		      ; (specification->package "nss-certs")
		      )
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service gnome-desktop-service-type)

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
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

           ;; This is the default list of services we
           ;; are appending to.
           %modified-desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "3676b4c2-2a54-4207-ad19-e5c2f1b3bc04")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "3AE1-807B"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "aff7c343-f045-42e2-84a1-e6cd48b440f7"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
