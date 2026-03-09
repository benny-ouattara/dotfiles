{ config, pkgs, ... }:
let
  home-dir = config.users.users.benouattara.home;
  log-dir = home-dir + "/.logs";
  # Define a simple backup script as a Nix string
  sync-mail-job = pkgs.writeShellScript "sync-mail" ''
    ${pkgs.isync}/bin/mbsync -a
    ${pkgs.notmuch}/bin/notmuch new
  '';
  watchdog-script = pkgs.writeShellScript "wm-watchdog" (builtins.readFile ./scripts/wm-watchdog.sh);
  key-benchmark   = pkgs.writeShellScript "key-bench"   (builtins.readFile ./scripts/key-bench.sh);
  wm-health-check = pkgs.writeShellScript "wm-health"   (builtins.readFile ./scripts/wm-health.sh);
in
{
  # services.nix-daemon.enable = true;

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };
  };
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "x86_64-darwin";

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

  networking.hostName = "kite";
  networking.computerName = "kite";

  # users
  users.users.benouattara = {
    name = "benouattara";
    home = "/Users/benouattara";
  };

  system.primaryUser = "benouattara";
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
        NSWindowShouldDragOnGesture = true; # Commmand+Control+Click to drag windows
        _HIHideMenuBar = true;
      };

      CustomUserPreferences = {
        "com.apple.WindowManager" = {
          EnableStandardClickToShowDesktop = 0; # Disables the annoying "click wallpaper to reveal desktop"
        };
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
    # pkgs.witr
    pkgs.nix-tree
    pkgs.nix-diff
    pkgs.nix-index
    pkgs.sshfs
    pkgs.ollama
    pkgs.just
    pkgs.github-cli
    pkgs.babashka
    pkgs.bore-cli
    pkgs.mailcatcher
    pkgs.docker-compose
    pkgs.google-cloud-sdk
    pkgs.cmake
    pkgs.postgresql
    pkgs.scalafmt
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
    pkgs.mu
    pkgs.emacsPackages.mu4e
    pkgs.isync
    pkgs.cloc
    pkgs.overmind
    pkgs.metals
    pkgs.neofetch
    pkgs.ranger
    pkgs.zsh
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

  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.iosevka-term
  ];

  homebrew = {
    enable = true;
    caskArgs.require_sha = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
      upgrade = true;
    };
    brews = [
      "choose-gui"
      "yabai"
      "sbcl"
      "btop"
      # "sshfs"
      # "reroutingcli"
      # "mmp"
      # "kubectl-site"
      "openjdk"
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
      "gimp"
      (noQuarantine "olive")
      "vlc"
      "appcleaner"
      "discord"
      "blender"
      "utm"
      "maccy"
      "balenaetcher"
      "dmenu-mac"
      "protonvpn"
      "alacritty"
      "syncthing"
      "kitty"
      "macfuse"
      "font-symbols-only-nerd-font"
      "sf-symbols"
      "wezterm"
      "monitorcontrol"
      "meetingbar"
      "corretto@17"
      "corretto@11"
      "tableplus"
      # "google-cloud-sdk"
      "background-music"
      "docker-desktop"
    ];
    taps = [
      # "homebrew/cask-versions"
      # "homebrew/cask-fonts"
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
      brew "emacs-plus@30", args: ["with-imagemagick", "with-modern-sexy-v2-icon", "with-xwidgets"], link: true
    '';
  };

  services.sketchybar.enable = true;
  services = {
    skhd = {
      enable = true;
      skhdConfig = builtins.readFile (pkgs.replaceVars ../skhd/skhdrc {
        # Add variables here if your skhdrc has @var@ placeholders
        # e.g., terminal = "${pkgs.kitty}/bin/kitty";
      });
    };
  };

  # We use Nix to generate the mcron configuration file
  environment.etc."mcron.d/jobs.guile".text = ''
    ;; Run mail sync every 15 minutes
    (job '(next-minute '(0 15 30 45)) "${sync-mail-job}")

    ;; Run a cleanup of the Downloads folder every day at 4am
    (job '(next-hour '(4)) "${pkgs.coreutils}/bin/rm -rf ~/Downloads/*")

    ;; Heartbeat: Check WM services every 10 minutes
    (job '(next-minute (range 0 60 10)) "${watchdog-script}")
  '';

  launchd.user.agents.mcron = {
    serviceConfig = {
      RunAtLoad = true;
      StandardErrorPath = log-dir + "/mcron.err.log";
      StandardOutPath = log-dir + "/mcron.out.log";
      EnvironmentVariables = { 
        PATH = "${config.environment.systemPath}"; 
      };
    };
    # Point mcron to the directory we created in /etc
    command = "${pkgs.mcron}/bin/mcron /etc/mcron.d/jobs.guile";
  };

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.max-jobs = 8;
  nix.settings.cores = 8;
}
