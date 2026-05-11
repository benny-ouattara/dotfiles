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
      guixGL = "/run/current-system/profile/lib";
      wrapWithGL = pkg: pkgs.symlinkJoin {
        name = pkg.pname or pkg.name;
        paths = [ pkg ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          for bin in $out/bin/*; do
            if [ -f "$bin" ] && [ ! -L "$bin" ]; then
              wrapProgram "$bin" --prefix LD_LIBRARY_PATH : "${guixGL}"
            elif [ -L "$bin" ]; then
              target=$(readlink "$bin")
              rm "$bin"
              makeWrapper "$target" "$bin" --prefix LD_LIBRARY_PATH : "${guixGL}"
            fi
          done
        '';
      };
    in
    {
      packages.${system}.default = pkgs.buildEnv {
        name = "nix-extras";
        paths = with pkgs; [
          ollama
          (wrapWithGL brave)
          (wrapWithGL discord)
          proton-vpn
          proton-pass
          lazygit
          yazi
          (wrapWithGL kitty)
        ];
      };
    };
}
