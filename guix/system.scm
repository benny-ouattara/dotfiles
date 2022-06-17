;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (nongnu packages linux)
             (gnu packages lisp)
             (gnu packages wm)
             (gnu packages fonts)
             (gnu packages shells))
(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg
 syncthing
 monitoring)

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "ben")
  (users (cons* (user-account
                 (name "ben")
                 (comment "main user account")
                 (group "users")
                 (shell (file-append zsh "/bin/zsh"))
                 (home-directory "/home/ben")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "lp")))
                (user-account
                 (name "danfodio")
                 (comment "dan fodio user account")
                 (group "users")
                 (shell (file-append zsh "/bin/zsh"))
                 (home-directory "/home/danfodio")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
   (append
    (list (list stumpwm "lib"))         ; use lib output of stumpwm package
    (list
     sbcl
     stumpwm+slynk
     (specification->package "awesome")
     (specification->package "i3-wm")
     (specification->package "i3status")
     (specification->package "dmenu")
     (specification->package "st")
     (specification->package "ratpoison")
     (specification->package "xterm")
     (specification->package "emacs")
     (specification->package "emacs-exwm")
     (specification->package
      "emacs-desktop-environment")
     (specification->package "nss-certs"))
    %base-packages))
  (services
   (append
    (list
     (geoclue-service)
     (service darkstat-service-type
              (darkstat-configuration
               (interface "wlp0s20f3")))
     (service bluetooth-service-type)
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
    %desktop-services))
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
