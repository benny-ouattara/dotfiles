(define-module (config)
  #:use-module (gnu)
  #:use-module (nongnu packages linux)
  #:use-module (gnu artwork)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages base)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages networking)
  #:use-module (gnu system setuid)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system privilege)
  #:use-module (gnu services)
  #:use-module (px services networking)
  #:use-module (px packages networking)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (beno packages cli)
  #:use-module (beno services openclaw)
  #:use-module (beno services openwebui))

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
 nix
 containers
 virtualization
 cuirass
 mcron
 docker
 databases
 linux)

(define %token (getenv "GITHUB_TOKEN"))
(define %repo (string-append "https://jazafund:" %token "@github.com/jazafund/jazacash.git"))

(define nonguix
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (branch "master")
   (commit "a89286d75f8dcadeabcf807eb259203aff259a63")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define jazacash
  (channel
   (name 'jazacash)
   (url %repo)
   (branch "develop")))

(define panther
  (channel
   (name 'pantherx)
   (url "https://codeberg.org/gofranz/panther.git")
   (branch "master")
   (commit "77ed154bd927c014f8f77d14ba493b41a082f1d8")
   (introduction
    (make-channel-introduction
     "54b4056ac571611892c743b65f4c47dc298c49da"
     (openpgp-fingerprint
      "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB")))))

(define guix
  (channel
   (name 'guix)
   (url "https://git.guix.gnu.org/guix.git")
   (branch "master")
   (commit
    "e1f2750b3d265dfb3958ea07c91630b0f9b362c2")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define %channels (list nonguix guix panther jazacash))
(define %motd (plain-file "motd" "Hi Ben, welcome!\n\n"))
(define %console-font (file-append font-tamzen "/share/kbd/consolefonts/Tamzen10x20.psf"))
(define %sudoers (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
# Set ben's sudo session to last for 4 hours (240 mins)
Defaults:ben timestamp_timeout=240
ben ALL=(root) NOPASSWD: ALL\n"))

;; (define %default-secrets `(("secrets.json" ,(local-file "/home/ben/Code/jazacash/aws/staging/secrets/staging.json"))))
;; (define secrets-config (jazacash-secrets-configuration (secret-files %default-secrets)))

(define %modified-desktop-services
  (modify-services %desktop-services
                   (delete console-font-service-type) ;; provide other console fonts below
                   (delete gdm-service-type)
                   (login-service-type config =>
                                       (login-configuration (inherit config)
                                                            (motd %motd)))
                   (guix-service-type config =>
                                      (guix-configuration (inherit config)
                                                          (channels %channels)
                                                          (guix (guix-for-channels %channels))
                                                          (substitute-urls
                                                           (cons* "https://substitutes.nonguix.org"
                                                                  "https://substitutes.guix.gofranz.com"
                                                                  ;; "http://substitutes.jazacash.com"
                                                                  %default-substitute-urls))
                                                          (authorized-keys
                                                           (cons* (local-file "../keys/nonguix-key.pub")
                                                                  (local-file "../keys/pantherx-key.pub")
                                                                  (local-file "../keys/cuirass-key.pub")
                                                                  %default-authorized-guix-keys))))))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (locale-libcs (list glibc-2.35 glibc-2.39 (canonical-package glibc)))
 (timezone "America/New_York")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "otter")
 (users (cons* (user-account
                (name "ben")
                (comment "Primary User")
                (group "users")
                (shell (file-append zsh "/bin/zsh"))
                (home-directory "/home/ben")
                (supplementary-groups '("cgroup" "wheel" "netdev" "audio" "video" "lp"))) 
               %base-user-accounts))
 (sudoers-file %sudoers)
 (privileged-programs
  (cons*
   (privileged-program
    (program (file-append stumpwm+slynk "/bin/stumpwm"))
    (setuid? #t))
   %default-privileged-programs))
 (packages (cons*
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
            nix
            tailscale
            wireshark
            podman-compose
            otter-cli
            guile-gnutls
            guile-gcrypt
            guile-git
            guile-fibers
            guile-jwt
	        sugar-light-sddm-theme-qt5
            dexy-color-sddm-theme-qt5
            chili-sddm-theme-qt5
	        %base-packages))
 (services
  (cons* 
   ;; (service jazacash-secrets-service-type secrets-config)
   ;; (service jazacash-ci-service-type)
   (service nix-service-type
            (nix-configuration
             (sandbox #f)
             (extra-config
              '("experimental-features = nix-command flakes\n"
                "extra-platforms = i686-linux aarch64-linux\n"
                "keep-outputs = true\n"
                "substituters = https://cache.nixos.org/\n" 
                "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=\n"
                "keep-derivations = true\n"))))
   (simple-service 'profiles-files etc-profile-d-service-type
                   (list
                    (plain-file "mock.sh" "MOCK=1")
                    (file-append nix "/etc/profile.d/nix-daemon.sh")
                    (file-append nix "/etc/profile.d/nix.sh")))
   (service iptables-service-type)        ; required for podman-service
   (service openwebui-service-type)
   (service openclaw-service-type)
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subgids
              (list (subid-range (name "ben"))
                    (subid-range (name "openclaw"))
                    (subid-range (name "openwebui"))))
             (subuids
              (list (subid-range (name "ben"))
                    (subid-range (name "openclaw"))
                    (subid-range (name "openwebui"))))))
   (service tailscale-service-type)
   (service syncthing-service-type
            (syncthing-configuration (user "ben")))
   (service gnome-desktop-service-type)
   (service qemu-binfmt-service-type
            (qemu-binfmt-configuration
             (platforms (lookup-qemu-platforms "arm" "aarch64"))))
   (service bluetooth-service-type)
   (udev-rules-service 'brightness brightnessctl)
   (service openssh-service-type
            (openssh-configuration
             (permit-root-login 'prohibit-password)
             (password-authentication? #f)
             (authorized-keys
              `(("root" ,(local-file "../keys/mac.pub"))
                ("ben"  ,(local-file "../keys/mac.pub"))))))
   (set-xorg-configuration
    (xorg-configuration (keyboard-layout keyboard-layout))
    sddm-service-type)
   (service sddm-service-type
            (sddm-configuration
             ;; valid values are elarun, maldives or maya, chili, sugar-light
             (theme "chili")))
   %modified-desktop-services))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (uuid "893cebe6-5e30-4588-9c99-1a03389facd8")))))
 ;; sudo blkid to list UUIDs
 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "D47F-3FB9" 'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (uuid "d690f4db-ddfc-45e7-9b77-c4a2e08892b3" 'ext4))
                       (type "ext4")) %base-file-systems)))

