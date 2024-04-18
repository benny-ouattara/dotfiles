{ config, pkgs, ... }: {
  nix.package = pkgs.nix;

  environment.systemPackages = [
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
    pkgs.reattach-to-user-namespace
    pkgs.direnv
    pkgs.neovim
    pkgs.spotify-tui
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
    pkgs.nixfmt
    pkgs.coreutils-full
    pkgs.clojure
    pkgs.clj-kondo
    pkgs.leiningen
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

    pkgs.scala
    pkgs.sbt
    pkgs.inetutils
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
      "google-cloud-sdk"
      "background-music"
      "docker"
    ];
    taps = [
      "homebrew/cask-versions"
      "homebrew/cask-fonts"
      "homebrew/bundle"
      "homebrew/services"
      "koekeishiya/formulae"
      "d12frosted/emacs-plus"
      "clojure/tools"
      "flyteorg/tap"
      "spotify/public"
      # "spotify/sptaps"
      # "spotify/mmptaps"
    ];
    extraConfig = ''
      brew "emacs-plus@29", args: ["with-imagemagick", "with-modern-sexy-v2-icon", "with-xwidgets"], link: true
    '';
  };
}
