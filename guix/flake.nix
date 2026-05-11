{
  description = "Nix packages for Guix gaps (GPU support, missing packages)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      packages.${system}.default = pkgs.buildEnv {
        name = "nix-extras";
        paths = with pkgs; [
          ollama
          brave
          discord
        ];
      };
    };
}
