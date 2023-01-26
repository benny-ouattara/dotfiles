{ config, pkgs, ... }:

let
  envs = import ../envs.nix pkgs;
in
{

  imports = [
    ./system-defaults.nix
    ./mac-config.nix
    ./window-management.nix
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  nixpkgs.overlays =
    let path = ../overlays; in with builtins;
          map (n: import (path + ("/" + n)))
            (filter (n: match ".*\\.nix" n != null ||
                        pathExists (path + ("/" + n + "/default.nix")))
              (attrNames (readDir path)));
  # ++ [ (import ./envs.nix) ];

  nix.nixPath =
    [ "darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix"
      "darwin=$HOME/.nix-defexpr/channels/darwin"
      "$HOME/.nix-defexpr/channels"
    ];

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.max-jobs = 1;
  nix.settings.cores = 1;
}
