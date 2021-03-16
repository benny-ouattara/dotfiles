{config, pkgs, ...}:

let
    home-dir = builtins.getEnv("HOME");
    packages = pkgs.callPackage ./packages.nix {};
    formulae = pkgs.callPackage ./brew-formulae.nix {};
    casks = pkgs.callPackage ./brew-casks.nix {};
    taps = pkgs.callPackage ./brew-taps.nix {};
in
{
    nix.package = pkgs.nix;

    environment.systemPackages = packages.all;

    homebrew = {
        enable = true;
        cleanup = "zap";
        taps = taps.all;
        brews = formulae.all;
        casks = casks.all;
        extraConfig = ''
        cask_args appdir: "${home-dir}/Applications"
        brew "emacs-plus@27", args: ["with-no-titlebar", "with-modern-sexy-v2-icon"], link: true
        '';
    };

    fonts = {
        enableFontDir = true;
        fonts = [
            pkgs.powerline-fonts
            pkgs.jetbrains-mono
        ];
    };
}
