{
  description = "Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-darwin";
    in
      {
        # $ darwin-rebuild build --flake .#beno
        darwinConfigurations."beno" = nix-darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          modules = [ ./personal-dw.nix
                      home-manager.darwinModules.home-manager
                      {
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        home-manager.users.benouattara = import ./personal-hm.nix;
                      }
                    ];
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (import ./overlays/custom-scripts.nix {
                scriptsPath = ./scripts;
              })
              (import ./overlays/z.nix)
            ];
          };
        };

        # $ darwin-rebuild build --flake .#zo
        darwinConfigurations."zo" = nix-darwin.lib.darwinSystem {
          modules = [ ./work-dw.nix
                      home-manager.darwinModules.home-manager
                      {
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        home-manager.users.benouattara = import ./work-hm.nix;
                      }
                    ];
        };

        # Expose the package set, including overlays, for convenience.
        # darwinPackages = self.darwinConfigurations."beno".pkgs;
      };
}
