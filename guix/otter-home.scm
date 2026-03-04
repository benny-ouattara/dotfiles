(add-to-load-path (string-append (dirname (current-filename)) "/modules"))
(use-modules
 (gnu home)
 (gnu packages)
 (gnu packages networking)
 (gnu packages admin)
 (gnu packages ssh)
 (gnu packages gnupg)
 (gnu packages tmux)
 (gnu services)
 (gnu packages ci)
 (gnu packages dns)
 (gnu packages ruby)
 (gnu packages guile-xyz)
 (guix gexp)
 (gnu home services shells)
 (gnu home services gnupg)
 (gnu home services)
 (gnu home services ssh)
 (px packages ai)
 (gnu home services shepherd)
 (gnu home services desktop)
 (gnu packages shells)
 (gnu packages shellutils)
 (openclaw))

(define (home-ollama-shepherd-service config)
  (list (shepherd-service
          (provision '(ollama))
          (documentation "Start the ollama server")
          (start #~(make-forkexec-constructor
                    (list (string-append #$ollama "/bin/ollama") "serve")
                    #:environment-variables (list "HOME=/home/ben"
                                                  "OLLAMA_HOST=0.0.0.0"
                                                  "OLLAMA_MODELS=/home/ben/.ollama/models")
                    #:log-file "/home/ben/.ollama/logs"
                    ))
          (stop #~(make-kill-destructor)))))

(define-public home-ollama-service-type
  (service-type (name 'ollama)
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   home-ollama-shepherd-service)))
                (default-value #f)
                (description
                 "Launch the ollama server so running ollama works out of the box.")))

(home-environment
  (packages
   (cons*
    openclaw-scripts
    (specifications->packages
     (list
      "ollama@0.16.1"
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
      "qutebrowser@3.6.3"
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
      "neofetch@7.1.0"
      "direnv@2.37.1"
      "pandoc@2.19.2"
      "python@3.11.14"
      "babashka@1.12.214"
      "awscli@1.43.11"
      "tmux@3.6a"
      "docker-compose@1.29.2"
      "podman-compose@1.5.0"
      "bind@9.19.24"

      ;; jazacash
      ;; "rust-bore-cli@0.5.1"
      "jtools@0.0.0"
      "sops@3.9.4"
      "github-cli@2.65.0"))))
  (services
   (list
    (simple-service 'environment-variables-service
                    home-environment-variables-service-type
                    `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                      ("EDITOR" . "emacs")
                      ("VISUAL" . "emacs")))
    (service home-ollama-service-type)
    (service home-openclaw-service-type)
    (service home-openssh-service-type
             (home-openssh-configuration
               (add-keys-to-agent "yes")
               (hosts
                (list (openssh-host (name "*")
                                    (extra-content "  StrictHostKeyChecking no"))
                      (openssh-host
                        (name "ops 10.0.0.86")
                        (host-name "10.0.0.86")
                        (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                        (user "root")
                        (proxy (proxy-command "ssh -W %h:%p root@44.201.64.217")))
                      (openssh-host
                        (name "dev 44.201.64.217")
                        (host-name "44.201.64.217")
                        (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                        (user "root"))
                      (openssh-host
                        (name "prod 13.244.104.50")
                        (host-name "13.244.104.50")
                        (identity-file (format #f "~a/.ssh/jazacash" (getenv "HOME")))
                        (user "root"))))))
    (service home-ssh-agent-service-type
             (home-ssh-agent-configuration
               (extra-options '("-t" "1h30m"))))
    (service home-gpg-agent-service-type
             (home-gpg-agent-configuration
               (pinentry-program
                (file-append pinentry-emacs "/bin/pinentry"))
               (ssh-support? #f)))
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
