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
  #:use-module (beno services protonmail-bridge)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services fontutils)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (beno packages lnav))

(home-environment
 (packages
  (cons*
   (specifications->packages
    (list
     "file"
     "binutils"
     "acl"
     "ncurses"
     "hexyl"
     "broot"
     "restic"
     "lnav"
     "tldr"
     "tokei"
     "radeontop"
     "eza"
     "bat"
     "witr"
     "claude-code"
     "fd"
     "ranger"
     "picom"
     "feh"
     "xset"
     "setxkbmap"
     "xcape"
     "openssh"
     "git"
     "pcmanfm"
     "xbacklight"
     "brightnessctl"
     "neovim"
     "lxappearance"
     "adwaita-icon-theme"
     "font-iosevka"
     "font-iosevka-term"
     "font-jetbrains-mono"
     "sbcl-slynk"
     "curl"
     "rlwrap"
     "ripgrep"
     "xsetroot"
     "xdot"
     "net-tools"
     "clojure"
     "clojure-lsp"
     "alsa-utils"
     "make"
     "network-manager-applet"
     "gnupg"
     "qemu"
     "polybar"
     "rofi"
     "papirus-icon-theme"
     "unzip"
     "the-silver-searcher"
     "ack"
     "lsof"
     "leiningen"
     "openjdk:jdk"
     "node"
     "nmap"
     "fzf"
     "ruby"
     "btop"
     "tree"
     "fastfetch"
     "direnv"
     "pandoc"
     "python"
     "babashka"
     "awscli"
     "podman-compose"
     "bind"
     "jtools"
     "sops"
     "github-cli"
     "zoxide"
     "starship"
     "xdotool"
     "xclip"
     "font-google-material-design-icons"
     "font-nerd-symbols"
     "sbcl-stumpwm-ttf-fonts"
     "font-atui-feather"
     "clipmenu"
     "slock"
     "tmux"
     "maim"
     "ffmpeg"                   ; guix/scripts/record
     "slop"
     "libnotify"
     "dunst"
     "pavucontrol"
     "playerctl"
     "arandr"
     "blueman"
     "bluez"
     "jq"
     "ncdu"
     "git-delta"
     "mu"
     "isync"
     "msmtp"
     "notmuch"
     "afew"
     "password-store"))))
 (services
  (cons*
   (simple-service 'environment-variables-service
                   home-environment-variables-service-type
                   `(("EDITOR" . "emacsclient -t") ("VISUAL" . "emacsclient -c") ("CM_LAUNCHER" . "rofi")
                     ("PATH" . ,(string-append (getenv "HOME") "/.emacs.d/bin:"
                                               (getenv "HOME") "/.local/bin:"
                                               (getenv "HOME") "/.local/share/gem/ruby/3.4.0/bin:"
                                               (getenv "HOME") "/Code/dotfiles/guix/scripts:"
                                               (getenv "PATH")))))
   (service home-ollama-service-type)
   (service home-protonmail-bridge-service-type)
   (service home-mcron-service-type
            (home-mcron-configuration
             (jobs
              (list #~(job '(next-minute (range 0 60 5))
                           (string-append "mbsync -a"
                                          " && notmuch new"
                                          " && afew -n -t")
                           "sync-mail")))))
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
   (simple-service 'nix-fonts-service
                   home-fontconfig-service-type
                   (list "~/.nix-profile/share/fonts"))
   %base-home-services)))
