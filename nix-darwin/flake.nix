{
  description = "Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.11";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-24.11";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ... }:
    {
      # $ darwin-rebuild build --flake .#Bens-MacBook-Pro
      darwinConfigurations."Bens-MacBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [ ./personal-dw.nix
                    home-manager.darwinModules.home-manager
                    {
                      home-manager.useGlobalPkgs = true;
                      home-manager.useUserPackages = true;
                      home-manager.users.benouattara = import ./personal-hm.nix;
                    }
                  ];
      };

      # $ darwin-rebuild build --flake .#zangao
      darwinConfigurations."zangao" = nix-darwin.lib.darwinSystem {
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
      # darwinPackages = self.darwinConfigurations."Bens-MacBook-Pro".pkgs;
    };
}
