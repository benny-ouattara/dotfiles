(use-modules
 (gnu home)
 (gnu packages)
 (gnu packages networking)
 (gnu packages admin)
 (gnu packages ssh)
 (gnu packages gnupg)
 (gnu services)
 (gnu packages ci)
 (gnu packages ruby)
 (gnu packages guile-xyz)
 (guix gexp)
 (gnu home services shells)
 (gnu home services gnupg)
 (gnu home services)
 (gnu home services ssh)
 (gnu home services desktop)
 (gnu packages shells)
 (gnu packages shellutils))

(home-environment
 (packages
  (specifications->packages
   (list
    "fd@9.0.0"
    "nyxt@3.11.7"
    "alacritty@0.13.1"
    "picom@11.2"
    "feh@3.10.3"
    "xset@1.2.4"
    "setxkbmap@1.3.2"
    "xcape@1.2"
    "openssh@9.8p1"
    "git@2.45.2"
    "pcmanfm@1.3.2"
    "xbacklight@1.2.3"
    "brightnessctl@0.5.1"
    "neovim@0.9.5"
    "lxappearance@0.6.3"
    "adwaita-icon-theme@44.0"
    "font-iosevka@20.0.0"
    "font-iosevka-term@20.0.0"
    "font-jetbrains-mono@2.304"
    "font-awesome@4.7.0"
    "sbcl-slynk@1.0.43-9.9c43bf6"
    "sbcl-stumpwm-swm-gaps@0.0.1-5.4613a95"
    "sbcl-stumpwm-ttf-fonts@0.0.1-5.4613a95"
    "sbcl-stumpwm-stumptray@0.0.1-5.4613a95"
    "sbcl-stumpwm-kbd-layouts@0.0.1-5.4613a95"
    "sbcl-stumpwm-net@0.0.1-5.4613a95"
    "sbcl-stumpwm-wifi@0.0.1-5.4613a95"
    "curl@8.5.0"
    "rlwrap@0.46.1"
    "ripgrep@14.1.0"
    "xsetroot@1.1.3"
    "xdot@1.3"
    "net-tools@1.60-0.479bb4a"
    "clojure@1.11.1"
    "alsa-utils@1.2.4"
    "make@4.3"
    "network-manager-applet@1.36.0"
    "gnupg@2.2.39"
    "qemu@8.2.2"
    "polybar@3.7.1"
    "rofi@1.7.5"
    "unzip@6.0"
    "firefox@127.0.2"
    "the-silver-searcher@2.2.0"
    "ack@3.7.0"
    "lsof@4.94.0"
    "leiningen@2.10.0"
    "openjdk@17.0.10:jdk"
    "node@18.19.0"
    "nmap@7.93"
    "fzf@0.41.0"
    "ruby@3.3.3"
    "btop@1.3.2"
    "tree@2.1.1"
    "neofetch@7.1.0"
    "direnv@2.32.3"
    "pandoc@2.19.2"
    "python@3.10.7"
    "babashka@1.3.189"
    "just@1.23.0"

    ;; jazacash
    "rust-bore-cli@0.5.1"
    "jtools@0.0.0"
    "rust-bore@0.4.1"
    "github-cli@2.65.0"
    )))
 (services
  (list
   (simple-service 'environment-variables-service
                   home-environment-variables-service-type
                   `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                     ("EDITOR" . "emacs")
                     ("VISUAL" . "emacs")
                     ;; ("SHELL" . ,(file-append zsh "/bin/zsh"))
                     ))
   (service home-openssh-service-type
            (home-openssh-configuration
             (add-keys-to-agent "yes")
             (hosts
              (list (openssh-host (name "*")
                                  (extra-content "  StrictHostKeyChecking no"))
                    (openssh-host
                     (name "ci 35.231.53.45")
                     (host-name "35.231.53.45")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "app-dev 34.148.193.204")
                     (host-name "34.148.193.204")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "app-prod 34.35.8.94")
                     (host-name "34.35.8.94")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    ;; (openssh-host
                    ;;  (name "app0-prod 34.35.22.11")
                    ;;  (host-name "34.35.22.11")
                    ;;  (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                    ;;  (user "root"))
                    ;; (openssh-host
                    ;;  (name "app1-prod 34.35.37.186")
                    ;;  (host-name "34.35.37.186")
                    ;;  (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                    ;;  (user "root"))
                    ;; (openssh-host
                    ;;  (name "lb-prod 34.35.43.13")
                    ;;  (host-name "34.35.43.13")
                    ;;  (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                    ;;  (user "root"))
                    ;; (openssh-host
                    ;;  (name "db-prod 34.35.8.94")
                    ;;  (host-name "34.35.8.94")
                    ;;  (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                    ;;  (user "root"))
                    ;; (openssh-host
                    ;;  (name "terra 10.0.0.213")
                    ;;  (host-name "10.0.0.213")
                    ;;  (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                    ;;  (user "root"))
                    ))))
   (service home-ssh-agent-service-type
            (home-ssh-agent-configuration
             (extra-options '("-t" "1h30m"))))
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry"))
             (ssh-support? #f)))
   (service home-redshift-service-type)
   (service
    home-zsh-service-type
    (home-zsh-configuration
     (zshrc (list (local-file "./.zshrc" "zshrc")))
     (zprofile (list (local-file "./.zprofile" "zprofile")))))
   (service
    home-bash-service-type
    (home-bash-configuration
     (aliases
      '(("grep" . "grep --color=auto")
        ("ll" . "ls -l")
        ("ls" . "ls -p --color=auto")))
     (bashrc (list (local-file "./.bashrc" "bashrc")))
     (bash-profile
      (list (local-file "./.bash_profile" "bash_profile"))))))))
