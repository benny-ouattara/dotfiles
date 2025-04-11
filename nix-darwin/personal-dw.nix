{ config, pkgs, ... }:
let
  home-dir = builtins.getEnv ("HOME");
  log-dir = home-dir + "/.logs";
in
{
  services.nix-daemon.enable = true;

  nixpkgs.overlays = let path = ./overlays;
                     in with builtins;
                       map (n: import (path + ("/" + n))) (filter (n:
                         match ".*\\.nix" n != null
                         || pathExists (path + ("/" + n + "/default.nix")))
                         (attrNames (readDir path)));

  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "x86_64-darwin";

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };
  };

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

  # users
  users.users.benouattara = {
    name = "benouattara";
    home = "/Users/benouattara";
  };

  system = {
    defaults = {
      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleKeyboardUIMode = 3;
        ApplePressAndHoldEnabled = false;
        InitialKeyRepeat = 7;
        KeyRepeat = 1;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;
        _HIHideMenuBar = true;
      };

      dock = {
        autohide = true;
        orientation = "bottom";
        showhidden = true;
        mineffect = "genie";
        launchanim = true;
        show-process-indicators = true;
        tilesize = 48;
        static-only = true;
        mru-spaces = false;
      };

      finder = {
        AppleShowAllExtensions = true;
        QuitMenuItem = true;
        FXEnableExtensionChangeWarning = false;
        CreateDesktop = false;
      };

      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  environment.systemPackages = [
    pkgs.sops
    pkgs.age
    pkgs.awscli
    pkgs.ejsonkms
    pkgs.packer
    pkgs.ollama
    pkgs.just
    pkgs.github-cli
    pkgs.babashka
    pkgs.bore-cli
    pkgs.mailcatcher
    pkgs.docker-compose
    pkgs.cmake
    pkgs.postgresql
    pkgs.scalafmt
    pkgs.sketchybar
    pkgs.foreman
    pkgs.yarn
    pkgs.ffmpeg
    pkgs.gnutls
    pkgs.mcron
    pkgs.afew
    pkgs.notmuch
    pkgs.stow
    pkgs.poppler
    pkgs.termshark
    pkgs.nmap
    pkgs.hugo
    pkgs.guile
    pkgs.micro
    pkgs.ammonite
    pkgs.automake
    pkgs.pkg-config
    pkgs.qemu
    pkgs.rlwrap
    pkgs.maven
    pkgs.pandoc
    pkgs.cask
    pkgs.mu
    pkgs.emacsPackages.mu4e
    pkgs.isync
    pkgs.cloc
    pkgs.overmind
    pkgs.metals
    pkgs.neofetch
    pkgs.ranger
    pkgs.zsh
    pkgs.skhd
    pkgs.ansible
    pkgs.tmux
    pkgs.direnv
    pkgs.neovim
    pkgs.pass
    pkgs.portaudio
    pkgs.curl
    pkgs.git
    pkgs.gnupg
    pkgs.htop
    pkgs.jq
    pkgs.ripgrep
    pkgs.silver-searcher
    pkgs.fd
    pkgs.nixfmt-classic
    pkgs.coreutils-full
    pkgs.clojure
    pkgs.clj-kondo
    pkgs.cljfmt
    # pkgs.leiningen
    (pkgs.leiningen.override { jdk = pkgs.jdk17; })
    pkgs.wireguard-tools
    pkgs.tree
    pkgs.tcpdump
    pkgs.mosh
    pkgs.m-cli
    pkgs.hydroxide
    pkgs.gcc
    pkgs.z
    pkgs.custom-scripts
    pkgs.fontconfig
    # pkgs.scala
    # "pkgs.sbt@1.8.0"
    pkgs.inetutils
  ];

  homebrew = {
    enable = true;
    caskArgs.require_sha = true;
    onActivation = {
      autoUpdate = false;
      cleanup = "uninstall";
      upgrade = false;
    };
    brews = [
      "choose-gui"
      "yabai"
      "sbcl"
      "btop"
      # "reroutingcli"
      # "mmp"
      # "kubectl-site"
      "jdtls"
      "metals"
      "openjdk"
      "yaml-language-server"
      "node"
    ];
    casks = let
      skipSha = name: {
        inherit name;
        args = { require_sha = false; };
      };
      noQuarantine = name: {
        inherit name;
        args = { no_quarantine = true; };
      };
    in [
      (skipSha "spotify")
      (noQuarantine "olive")
      "vlc"
      "appcleaner"
      "discord"
      #"gimp"
      # "blender"
      "utm"
      "maccy"
      "balenaetcher"
      "dmenu-mac"
      "protonvpn"
      "alacritty"
      "syncthing"
      "kitty"
      "font-victor-mono-nerd-font"
      "font-iosevka-nerd-font"
      "font-iosevka-term-nerd-font"
      "font-symbols-only-nerd-font"
      "sf-symbols"
      "wezterm"
      "monitorcontrol"
      "meetingbar"
      "corretto17"
      "corretto11"
      "tableplus"
      "background-music"
      "docker"
      "visualvm"
      "virtualbox"
      "vagrant"
    ];
    taps = [
      "homebrew/cask-versions"
      "homebrew/cask-fonts"
      "homebrew/bundle"
      "homebrew/services"
      "koekeishiya/formulae"
      "d12frosted/emacs-plus"
      "clojure/tools"
      # "flyteorg/tap"
      # "spotify/public"
      # "spotify/sptaps"
      # "spotify/mmptaps"
    ];
    extraConfig = ''
      brew "emacs-plus@29", args: ["with-imagemagick", "with-modern-sexy-v2-icon", "with-xwidgets"], link: true
    '';
  };

  services.skhd.enable = true;
  services.sketchybar.enable = true;
  services.skhd.skhdConfig =
    (builtins.readFile (pkgs.substituteAll { src = ../skhd/skhdrc; }));

  launchd.user.agents = {
    skhd = {
      serviceConfig = {
        RunAtLoad = true;
        EnvironmentVariables = { NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt"; };
        StandardErrorPath = log-dir + "/" + "skhd" + ".log";
        StandardOutPath = log-dir + "/" + "skhd" + ".log";
      };
    };

    mcron = {
      serviceConfig = {
        RunAtLoad = true;
        StandardErrorPath = log-dir + "/" + "mcron" + ".log";
        StandardOutPath = log-dir + "/" + "mcron" + ".log";
        EnvironmentVariables = { PATH = "${config.environment.systemPath}"; };
      };
      command = "/run/current-system/sw/bin/mcron --daemon";
    };
  };

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.max-jobs = 8;
  nix.settings.cores = 8;
}
