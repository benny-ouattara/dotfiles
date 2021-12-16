;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (nongnu packages linux)
             (gnu packages lisp))
(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)
(use-package-modules fonts wm)

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "ben")
  (users (cons* (user-account
                  (name "ben")
                  (comment "Ben")
                  (group "users")
                  (home-directory "/home/ben")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
     (list sbcl stumpwm+slynk `(,stumpwm "lib"))
     ;; sbcl-ttf-fonts font-dejavu
      (list (specification->package "openbox")
            (specification->package "awesome")
            ;; (specification->package "i3-wm")
            (specification->package "i3-gaps")
            (specification->package "i3status")
            (specification->package "dmenu")
            (specification->package "st")
            (specification->package "emacs")
            (specification->package "emacs-exwm")
            (specification->package
              "emacs-desktop-environment")
            (specification->package "nss-certs"))
      %base-packages))

  (services
    (append
      (list (service gnome-desktop-service-type)
            (service cups-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))

  ;; (services
  ;;  (modify-services %desktop-services
  ;;                   (guix-service-type config =>
  ;;                                      (guix-configuration
  ;;                                       (inherit config)
  ;;                                       (substitute-urls
  ;;                                        (append (list "https://substitutes.nonguix.org")
  ;;                                                %default-substitute-urls))
  ;;                                       (authorized-keys
  ;;                                        (append (list (local-file "./signing-key.pub"))
  ;;                                                %default-authorized-guix-keys))))))

  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "fea68479-d843-448e-9a28-1e9f45f6a519"
                     'ext4))
             (type "ext4"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "5F8E-0C2D" 'fat32))
             (type "vfat"))
           %base-file-systems)))
