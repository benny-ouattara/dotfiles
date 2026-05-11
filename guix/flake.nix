{
  description = "Nix packages for Guix gaps (GPU support, missing packages)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.buildEnv {
        name = "nix-extras";
        paths = with pkgs; [
          ollama
        ];
      };
    };
}
