(define-module (config)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu packages ci)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu home services ssh)
  #:use-module (px packages ai)
  #:use-module (px packages tools)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services desktop)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (beno services ollama)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (beno packages lnav))

(home-environment
 (packages
  (cons*
   (specifications->packages
    (list
     "file@5.46"
     "binutils@2.44"
     "acl@2.3.1"
     "ncurses@6.2.20210619"
     "hexyl@0.16.0"
     "broot@1.55.0"
     "restic@0.9.6"
     "lnav@0.12.4"
     "tldr@3.4.3"
     "tokei@12.1.2"
     "radeontop@1.4"
     "eza@0.23.4"
     "bat@0.24.0"
     "witr@0.3.0"
     "claude-code@2.1.71"
     "fd@10.3.0"
     "alacritty@0.16.1"
     "kitty@0.21.2"
     "ranger@1.9.4"
     "picom@13"
     "feh@3.10.3"
     "xset@1.2.5"
     "setxkbmap@1.3.4"
     "xcape@1.2"
     "openssh@10.2p1"
     "git@2.52.0"
     "pcmanfm@1.4.0"
     "xbacklight@1.2.4"
     "brightnessctl@0.5.1"
     "neovim@0.11.5"
     "lxappearance@0.6.4"
     "adwaita-icon-theme@46.2"
     "font-iosevka@33.3.0"
     "font-iosevka-term@33.3.0"
     "font-jetbrains-mono@2.304"
     "font-awesome@4.7.0"
     "sbcl-slynk@1.0.43-9.9c43bf6"
     "curl@8.6.0"
     "rlwrap@0.48"
     "ripgrep@15.1.0"
     "xsetroot@1.1.3"
     "xdot@1.4"
     "net-tools@1.60-0.479bb4a"
     "clojure@1.12.4"
     "alsa-utils@1.2.11"
     "make@4.4.1"
     "network-manager-applet@1.36.0"
     "gnupg@2.4.8"
     "qemu@10.2.0"
     "polybar@3.7.1"
     "rofi@2.0.0"
     "unzip@6.0"
     "firefox@147.0.4"
     "the-silver-searcher@2.2.0"
     "ack@3.7.0"
     "lsof@4.99.3"
     "leiningen@2.12.0"
     "openjdk@17.0.10:jdk"
     "node@22.14.0"
     "nmap@7.98"
     "fzf@0.67.0"
     "ruby@3.4.7"
     "btop@1.4.6"
     "tree@2.2.1"
     "fastfetch@2.57.0"
     "direnv@2.37.1"
     "pandoc@2.19.2"
     "python@3.11.14"
     "babashka@1.12.214"
     "awscli@1.43.11"
     "podman-compose@1.5.0"
     "bind@9.19.24"
     "jtools@0.0.0"
     "sops@3.9.4"
     "github-cli@2.65.0"
     "zoxide@0.9.8"
     "starship@1.21.1"
     "xdotool@3.20211022.1"
     "xclip@0.13"
     "font-google-material-design-icons@4.0.0"
     "font-nerd-symbols@3.4.0"
     "sbcl-stumpwm-ttf-fonts@0.0.1-7.c4f077b"
     "font-atui-feather@1.1.0-1.2ac7161"
     "clipmenu@6.2.0-1.7c34ace"))))
 (services
  (cons*
   (simple-service 'environment-variables-service
                   home-environment-variables-service-type
                   `(("EDITOR" . "emacsclient -t") ("VISUAL" . "emacsclient -c") ("CLIPMENULAUNCHER" . "rofi")))
   (service home-ollama-service-type)
   (service home-openssh-service-type
            (home-openssh-configuration
             (add-keys-to-agent "yes")
             (hosts
              (list (openssh-host
                     (name "*")
                     (extra-content "  StrictHostKeyChecking no"))
                    (openssh-host
                     (name "ops 10.0.0.86")
                     (host-name "10.0.0.86")
                     (identity-file (string-append (getenv "HOME") "/.ssh/jazacash"))
                     (user "root")
                     (proxy (proxy-command "ssh -W %h:%p root@44.201.64.217")))
                    (openssh-host
                     (name "dev 44.201.64.217")
                     (host-name "44.201.64.217")
                     (identity-file (string-append (getenv "HOME") "/.ssh/jazacash"))
                     (user "root"))
                    (openssh-host
                     (name "prod 13.244.104.50")
                     (host-name "13.244.104.50")
                     (identity-file (string-append (getenv "HOME") "/.ssh/jazacash"))
                     (user "root"))))))
   (service home-ssh-agent-service-type
            (home-ssh-agent-configuration
             (extra-options '("-t" "1h30m"))))
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program (file-append pinentry-emacs "/bin/pinentry"))
             (ssh-support? #f)))
   (service
    home-zsh-service-type
    (home-zsh-configuration
     (zshrc (list (local-file "../shells/.zshrc" "zshrc")))
     (zprofile (list (local-file "../shells/.zprofile" "zprofile")))))
   (service
    home-bash-service-type
    (home-bash-configuration
     (aliases
      '(("grep" . "grep --color=auto")
        ("ll" . "ls -l")
        ("ls" . "ls -p --color=auto")))
     (bashrc (list (local-file "../shells/.bashrc" "bashrc")))
     (bash-profile (list (local-file "../shells/.bash_profile" "bash_profile")))))
   %base-home-services)))
