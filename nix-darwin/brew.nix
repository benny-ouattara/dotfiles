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
      "podman"
      # "reroutingcli"
      # "mmp"
    ];

    casks = [
      "discord"
      "spotify"
      "gimp"
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
      "podman-desktop"
    ];

    extraConfig = ''
      brew "emacs-plus@30", args: ["with-imagemagick", "with-modern-sexy-v2-icon", "with-xwidgets"], link: true
    '';
  };
}
