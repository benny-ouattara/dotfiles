;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

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
   (list "recutils"
         "xterm"
         "volumeicon"
         "pavucontrol"
         "arandr"
         "autorandr"
         "xrandr"
         "fd"
         "nyxt"
         "kitty"
         "alacritty"
         "picom"
         "feh"
         "xset"
         "setxkbmap"
         "xcape"
         "syncthing"
         "openssh"
         "git"
         "xfce4-screenshooter"
         "thunar"
         "lxappearance"
         "pcmanfm"
         "xbacklight"
         "brightnessctl"
         "neovim"
         "arc-icon-theme"
         "matcha-theme"
         "hicolor-icon-theme"
         "adwaita-icon-theme"
         "gnome-backgrounds"
         "papirus-icon-theme"
         "breeze-icons"
         "font-iosevka"
         "font-fira-code"
         "font-jetbrains-mono"
         "font-abattis-cantarell"
         "font-dejavu"
         "font-google-noto"
         "font-liberation"
         "font-awesome"
         "font-google-material-design-icons"
         "sbcl-stumpwm-swm-gaps"
         "sbcl-stumpwm-ttf-fonts"
         "sbcl-stumpwm-stumptray"
         "sbcl-stumpwm-kbd-layouts"
         ;; "stumpwm-with-slynk"
         "sbcl-stumpwm-net"
         "sbcl-stumpwm-wifi"
         "sbcl-slynk"
         ;; "stumpwm:lib"
         "xmodmap"
         "curl"
         "rlwrap"
         "ripgrep"
         "autoconf"
         "xsetroot"
         "nss-certs"
         ;; "stumpish"
         "xdot"
         ;; "qutebrowser"
         "net-tools"
         ;; "dstat"
         "clojure"
         "clojure-tools"
         "alsa-utils"
         "make"
         "cmake"
         "network-manager-applet"
         "gnupg"
         "keychain"
         "qemu"
         ;; "qmpbackup"
         ;; "virt-manager"
         "polybar"
         "rofi"
         "unzip"
         ;; "font-nerd-iosevka"
         ;; "font-nerd-jetbrains"
         ;; "font-nerd-meslo"
         ;; "font-nerd-victor"
         ;; "font-material-icons"
         "ungoogled-chromium"
         "firefox"
         ;; "dunst"
         ;; "mpv"
         ;; "ncmpcpp"
         ;; "mpd"
         ;; "cava"
         ;; "sqlite"
         ;; "sqlitebrowser"
         ;; "gcc"
         "the-silver-searcher"
         "ack"
         "lsof"
         "gcc-toolchain"
         "leiningen"
         "tmux"
         "openjdk@17.0.10:jdk"
         "node"
         "nmap"
         "pinentry-emacs"
         "maven"
         "fzf"
         "ruby"
         "ruby-jwt"
         "guile-jwt"
         ;; "iptables"
         ;; "ipcalc"
         ;; "sipcalc"
         "btop"
         ;; "glances"
         ;; "dfc"
         ;; "facter"
         ;; "jnettop"
         ;; "clusterssh"
         ;; "dmidecode"
         "tree"
         ;; "smartmontools"
         ;; "fdupes"
         ;; "autojump"
         ;; "fasd"
         ;; "iftop"
         ;; "di"
         "neofetch"
         ;; "lynis"
         ;; "ngrep"
         ;; "jtbl"
         ;; "rex"
         ;; "doctl"
         ;; "du-dust"
         "direnv"
         ;; "nushell"
         "pandoc"
         ;; "bat"
         "python"
         ;; "strace"
         ;; "cuirass"
         ;; "youtube-dl"
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
                     (name "lb 35.231.94.98")
                     (host-name "35.231.94.98")
                     (forward-agent? #t)
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "jumpbox 10.122.0.2")
                     (host-name "10.122.0.2")
                     (forward-agent? #t)
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root")
                     (proxy (proxy-command "ssh -W %h:%p root@35.231.94.98")))
                    (openssh-host
                     (name "cicd 34.139.146.192")
                     (host-name "34.139.146.192")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root")
                     ;; (proxy (proxy-command "ssh -W %h:%p root@35.231.94.98"))
                     )
                    (openssh-host
                     (name "db 35.227.116.128")
                     (host-name "35.227.116.128")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root")
                     ;; (proxy (proxy-command "ssh -W %h:%p root@35.231.94.98"))
                     )
                    (openssh-host
                     (name "app1 35.231.142.92")
                     (host-name "35.231.142.92")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root")
                     ;; (proxy (proxy-command "ssh -W %h:%p root@35.231.94.98"))
                     )
                    (openssh-host
                     (name "app2 34.23.23.182")
                     (host-name "34.23.23.182")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root")
                     ;; (proxy (proxy-command "ssh -W %h:%p root@35.231.94.98"))
                     )
                    (openssh-host
                     (name "app1-prod 34.35.20.192")
                     (host-name "34.35.20.192")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "app2-prod 34.35.60.255")
                     (host-name "34.35.60.255")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "lb-prod 34.35.44.149")
                     (host-name "34.35.44.149")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "db-prod 34.35.31.72")
                     (host-name "34.35.31.72")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))
                    (openssh-host
                     (name "terra 10.0.0.213")
                     (host-name "10.0.0.213")
                     (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                     (user "root"))))))
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
