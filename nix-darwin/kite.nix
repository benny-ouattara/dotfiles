{ config, pkgs, ... }:
let
  home-dir = config.users.users.benouattara.home;
  log-dir = home-dir + "/.logs";
  # Define a simple backup script as a Nix string
  sync-mail-job = pkgs.writeShellScript "sync-mail" ''
    export PATH="${pkgs.pass}/bin:${pkgs.gnupg}/bin:$PATH"

    coreutils=${pkgs.coreutils}/bin
    lock_dir="$HOME/.cache/mail-sync.lock"
    # Kept outside ~/.mail so that notmuch does not index the dumps as mail.
    backup_dir="$HOME/.local/state/notmuch/tag-backups"

    # mcron fires every 5 minutes; skip if the previous run is still going,
    # otherwise two mbsync processes race on the same maildirs and state files.
    "$coreutils/mkdir" -p "$HOME/.cache"
    if ! "$coreutils/mkdir" "$lock_dir" 2>/dev/null; then
      if [ -r "$lock_dir/pid" ] && ! kill -0 "$("$coreutils/cat" "$lock_dir/pid")" 2>/dev/null; then
        "$coreutils/rm" -rf "$lock_dir"
        "$coreutils/mkdir" "$lock_dir" || exit 0
      else
        echo "mail-sync: previous run still active, skipping" >&2
        exit 0
      fi
    fi
    echo $$ > "$lock_dir/pid"
    trap '"$coreutils/rm" -rf "$lock_dir"' EXIT

    failed=""
    synced=0
    for group in jcash-support jcash-ops jcash-compliance jcash-info jcash-fraud jcash-hr jcash-sales jcash-system jgroup-ben jgroup-system jfund gmail protonmail; do
      err=$("$coreutils/mktemp")
      if ${pkgs.isync}/bin/mbsync "$group" 2>"$err"; then
        synced=$((synced + 1))
      else
        failed="$failed $group"
      fi
      # The Proton Bridge channels talk cleartext to 127.0.0.1 by design; that
      # warning alone accounted for ~32k lines of the error log.
      ${pkgs.gnugrep}/bin/grep -v 'Password is being sent in the clear' "$err" >&2 || true
      "$coreutils/rm" -f "$err"
      sleep 1
    done

    if [ -n "$failed" ]; then
      echo "mail-sync: FAILED groups:$failed" >&2
    fi

    if [ "$synced" -eq 0 ]; then
      echo "mail-sync: every group failed, skipping notmuch/afew" >&2
      exit 1
    fi

    ${pkgs.notmuch}/bin/notmuch new
    ${pkgs.afew}/bin/afew -n -t

    # Tags live only in the Xapian DB and are not re-downloadable; keep 14 days.
    today=$("$coreutils/date" +%F)
    "$coreutils/mkdir" -p "$backup_dir"
    if [ ! -f "$backup_dir/tags-$today.dump" ]; then
      ${pkgs.notmuch}/bin/notmuch dump --output="$backup_dir/tags-$today.dump"
      "$coreutils/ls" -1t "$backup_dir"/tags-*.dump | "$coreutils/tail" -n +15 | while read -r stale; do
        "$coreutils/rm" -f "$stale"
      done
    fi

    # launchd holds these open in append mode, so truncate in place rather than
    # renaming: a rotated-away inode would keep receiving all future output.
    for logf in "${log-dir}/mcron.err.log" "${log-dir}/mcron.out.log"; do
      if [ -f "$logf" ] && [ "$("$coreutils/stat" -c%s "$logf")" -gt 10485760 ]; then
        "$coreutils/tail" -c 2097152 "$logf" > "$logf.tmp" \
          && "$coreutils/cat" "$logf.tmp" > "$logf"
        "$coreutils/rm" -f "$logf.tmp"
      fi
    done

    [ -z "$failed" ]
  '';
  watchdog-script = pkgs.writeShellScript "wm-watchdog" (builtins.readFile ./scripts/wm-watchdog.sh);
  nix-gc = pkgs.writeShellScript "nix-gc" (builtins.readFile ./scripts/nix-gc.sh);
  key-benchmark = pkgs.writeShellScript "key-bench" (builtins.readFile ./scripts/key-bench.sh);
  wm-health-check = pkgs.writeShellScript "wm-health" (builtins.readFile ./scripts/wm-health.sh);
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

  system.activationScripts.postActivation.text = ''
    printf "\033[36m"
    echo "Welcome to KITE (Gen $(readlink /nix/var/nix/profiles/system | cut -d- -f2))"
    echo "Status: System is Healthy"
    printf "\033[0m"
  '';

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
    pkgs.himalaya
    pkgs.sops
    pkgs.opencode
    pkgs.codex
    pkgs.gemini-cli
    pkgs.claude-code
    pkgs.bruno
    pkgs.mitmproxy
    pkgs.lunar
    pkgs.hwatch
    pkgs.dust
    pkgs.xcp
    pkgs.procs
    pkgs.bottom
    pkgs.msmtp
    pkgs.emacs-lsp-booster
    pkgs.clojure-lsp
    pkgs.lima
    pkgs.awscli
    pkgs.jet
    pkgs.podman
    pkgs.podman-tui
    pkgs.podman-compose
    pkgs.wuzz
    pkgs.hexyl
    pkgs.broot
    pkgs.lazygit
    pkgs.glow
    pkgs.difftastic
    pkgs.bandwhich
    pkgs.zellij
    pkgs.watchexec
    pkgs.hyperfine
    pkgs.hey
    pkgs.restic
    pkgs.tokei
    pkgs.tldr
    pkgs.lnav
    pkgs.zoxide
    pkgs.eza
    pkgs.witr
    pkgs.bat
    pkgs.graphviz
    pkgs.nix-du
    pkgs.nix-tree
    pkgs.nix-diff
    pkgs.nix-index
    pkgs.nixfmt
    pkgs.sshfs
    pkgs.ollama
    pkgs.just
    pkgs.github-cli
    pkgs.babashka
    pkgs.bore-cli
    pkgs.mailcatcher
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
    pkgs.guile
    pkgs.micro
    pkgs.ammonite
    pkgs.automake
    pkgs.pkg-config
    pkgs.qemu
    pkgs.rlwrap
    pkgs.maven
    pkgs.pandoc
    pkgs.isync
    pkgs.overmind
    pkgs.metals
    pkgs.fastfetch
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
    pkgs.coreutils-full
    pkgs.clojure
    pkgs.clj-kondo
    pkgs.cljfmt
    (pkgs.leiningen.override { jdk = pkgs.jdk17; })
    pkgs.wireguard-tools
    pkgs.tree
    pkgs.tcpdump
    pkgs.mosh
    pkgs.m-cli
    pkgs.hydroxide
    pkgs.gcc
    pkgs.custom-scripts
    pkgs.fontconfig
    pkgs.inetutils
    pkgs.nodejs
  ];

  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.iosevka-term
  ];

  services.sketchybar.enable = true;
  services = {
    skhd = {
      enable = true;
      skhdConfig = builtins.readFile (
        pkgs.replaceVars ../skhd/skhdrc {
          # Add variables here if your skhdrc has @var@ placeholders
          # e.g., terminal = "${pkgs.kitty}/bin/kitty";
        }
      );
    };
  };

  # We use Nix to generate the mcron configuration file
  environment.etc."mcron.d/jobs.guile".text = ''
    ;; Run mail sync every 5 minutes
    (job '(next-minute (range 0 60 5)) "${sync-mail-job}")

    ;; Run a cleanup of the Downloads folder every day at 8am
    (job '(next-hour '(8)) "${pkgs.coreutils}/bin/ls ~/Downloads/*")

    ;; Heartbeat: Check WM services every 10 minutes
    (job '(next-minute (range 0 60 10)) "${watchdog-script}")

    ;; Run GC every day at 9 AM
    (job '(next-hour '(9)) "${nix-gc}")
  '';

  launchd.user.agents.mcron = {
    serviceConfig = {
      RunAtLoad = true;
      StandardErrorPath = log-dir + "/mcron.err.log";
      StandardOutPath = log-dir + "/mcron.out.log";
    };
    # Point mcron to the directory we created in /etc
    command = "${pkgs.mcron}/bin/mcron /etc/mcron.d/jobs.guile";
  };

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.max-jobs = 8;
  nix.settings.cores = 8;
}
