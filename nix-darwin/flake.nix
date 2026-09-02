{
  description = "Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
    nixpkgs-legacy.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      nixpkgs-legacy,
      home-manager,
      homebrew,
      ...
    }:
    let
      system = "x86_64-darwin";
      legacy = import nixpkgs-legacy {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      # $ darwin-rebuild build --flake .#kite
      darwinConfigurations."kite" = nix-darwin.lib.darwinSystem {
        inherit system;
        modules = [
          ./kite.nix
          ./brew.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.benouattara = import ./home.nix;
          }
          homebrew.darwinModules.nix-homebrew
          {
            nix-homebrew = {
              enable = true;
              user = "benouattara";
              autoMigrate = true;
            };
          }
        ];
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (import ./overlays/custom-scripts.nix { scriptsPath = ./scripts; })
            (final: prev: {
              babashka = legacy.babashka;
              clojure-lsp = legacy.clojure-lsp;
              jet = legacy.jet;
              # sbcl = legacy.sbcl;
              clj-kondo = legacy.clj-kondo;
              cljfmt = legacy.cljfmt;
            })
          ];
        };
      };

      # $ darwin-rebuild build --flake .#onyx
      darwinConfigurations."onyx" = nix-darwin.lib.darwinSystem {
        modules = [
          ./onyx.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.benouattara = import ./home.nix;
          }
        ];
      };

      # Expose the package set, including overlays, for convenience.
      # darwinPackages = self.darwinConfigurations."kite".pkgs;
    };
}
