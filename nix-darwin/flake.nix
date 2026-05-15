{
  description = "Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, homebrew, ... }:
    let
      system = "x86_64-darwin"; # Or aarch64-darwin for Silicon
    in
      {
        # $ darwin-rebuild build --flake .#kite
        darwinConfigurations."kite" = nix-darwin.lib.darwinSystem {
          inherit system;
          modules = [ ./kite.nix
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
            ];
          };
        };

        # $ darwin-rebuild build --flake .#onyx
        darwinConfigurations."onyx" = nix-darwin.lib.darwinSystem {
          modules = [ ./onyx.nix
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
