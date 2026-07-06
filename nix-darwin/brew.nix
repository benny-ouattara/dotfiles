{ config, pkgs, ... }:
{
  homebrew = {
    enable = true;
    caskArgs.require_sha = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      # 'zap' uninstalls anything not listed here—be careful!
      # Use 'uninstall' if you want to keep manual installs.
      # cleanup = "zap";
      cleanup = "uninstall";
    };

    taps = [
      "homebrew/bundle"
      "homebrew/services"
      "koekeishiya/formulae"
      "d12frosted/emacs-plus"
      "clojure/tools"
      # "homebrew/cask-versions"
      # "homebrew/cask-fonts"
      # "flyteorg/tap"
      # "spotify/public"
      # "spotify/sptaps"
      # "spotify/mmptaps"
    ];

    brews = [
      "choose-gui"
      "yabai"
      "sbcl"
      "btop"
      "openjdk"
      "node"
      "aider"
      # "podman"
      # "reroutingcli"
      # "mmp"
    ];

    casks = [
      "snapzy"
      "visualvm"
      "discord"
      "spotify"
      "vlc"
      "appcleaner"
      "blender"
      "utm"
      "maccy"
      "balenaetcher"
      "dmenu-mac"
      "protonvpn"
      "alacritty"
      "syncthing-app"
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
      "background-music"
      "podman-desktop"
      "karabiner-elements"
    ];

    extraConfig = ''
      brew "emacs-plus@31", args: ["with-xwidgets"], link: true
      cask "google-drive", args: { require_sha: false }
    '';
  };
}
