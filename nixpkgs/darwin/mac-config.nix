{config, pkgs, ...}:

let
    home-dir = builtins.getEnv("HOME");
    packages = pkgs.callPackage ./packages.nix {};
in
{
    nix.package = pkgs.nix;

    environment.systemPackages = packages.all;

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
        ];
        casks = let
            skipSha = name: {
                inherit name;
                args = {require_sha = false;};
            };
            noQuarantine = name: {
                inherit name;
                args = {no_quarantine = true;};
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
            #"qutebrowser"
            #"slack"
            #"docker"
            #"zoom"
            #"android-platform-tools"
        ];
        taps = [
            "homebrew/cask-versions"
            "homebrew/cask-fonts"
            "homebrew/cask"
            "homebrew/bundle"
            "homebrew/services"
            "koekeishiya/formulae"
            "d12frosted/emacs-plus"
            "clojure/tools"
            "flyteorg/tap"
            # "spotify/public"
            # "spotify/sptaps"
        ];
        extraConfig = ''
      # cask_args appdir: "${home-dir}/Applications"
      brew "emacs-plus@29", args: ["with-imagemagick", "with-modern-sexy-v2-icon"], link: true
    '';
    };

    # Note that the homebrew pkg overshadows system packages probably a bug in the mdule
    # Comment it out in order to have system packages be applied
    # homebrew = {
    #   enable = true;
    #   cleanup = "zap";
    #   taps = taps.all;
    #   brews = formulae.all;
    #   casks = casks.all;
    #    extraConfig = ''
    #    cask_args appdir: "${home-dir}/Applications"
    #    brew "emacs-plus@27", args: ["with-no-titlebar", "with-modern-sexy-v2-icon"], link: true
    #  '';
    # };

    # fonts = {
    #     enableFontDir = true;
    #     fonts = [
    #         pkgs.powerline-fonts
    #         pkgs.jetbrains-mono
    #         pkgs.roboto-mono
    #         pkgs.fira-code
    #         pkgs.iosevka
    #     ];
    # };
}
