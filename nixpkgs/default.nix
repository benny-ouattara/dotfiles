{ nixpkgs       ? import <nixpkgs>
, system        ? builtins.currentSystem
, pkgs          ? import <nixpkgs> { inherit system; }
, configuration ? <darwin-config>
, darwin        ? import <darwin> { inherit configuration system pkgs; }
# , home-manager  ? import ./home-manager/home-manager/home-manager.nix {
#     inherit pkgs;
#     confPath = ./config/home.nix;
#     confAttr = "";
#   }
}: {
  nix-darwin = darwin.system;
  # home-manager = home-manager.activationPackage;
}
