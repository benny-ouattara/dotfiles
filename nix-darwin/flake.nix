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
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#Bens-MacBook-Pro
      darwinConfigurations."Bens-MacBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [ ./configuration.nix
                    home-manager.darwinModules.home-manager
                    {
                      home-manager.useGlobalPkgs = true;
                      home-manager.useUserPackages = true;
                      home-manager.users.benouattara = import ./benouattara.nix;
                    }
                  ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Bens-MacBook-Pro".pkgs;
    };
}
