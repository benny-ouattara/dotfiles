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
      mesaDri = "/run/current-system/profile/lib/dri";
      wrapWithGL = pkg: pkgs.symlinkJoin {
        name = pkg.pname or pkg.name;
        paths = [ pkg ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          for bin in $out/bin/*; do
            if [ -f "$bin" ] && [ ! -L "$bin" ]; then
              wrapProgram "$bin" \
                --set LIBGL_DRIVERS_PATH "${mesaDri}"
            elif [ -L "$bin" ]; then
              target=$(readlink "$bin")
              rm "$bin"
              makeWrapper "$target" "$bin" \
                --set LIBGL_DRIVERS_PATH "${mesaDri}"
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
          brave
          discord
          proton-vpn
          proton-pass
          lazygit
          nerd-fonts.hack
          # Guix profile font dirs have mtime 1, so Nix's fontconfig never
          # notices new fonts; guix home runs this when fonts change
          (pkgs.writeShellScriptBin "nix-fc-cache" ''
            FONTCONFIG_FILE=${pkgs.fontconfig.out}/etc/fonts/fonts.conf \
              exec ${pkgs.fontconfig.bin}/bin/fc-cache -f "$@"
          '')
          yazi
          protonmail-bridge
          (pkgs.symlinkJoin {
            name = "kitty";
            paths = [ pkgs.kitty ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              for bin in $out/bin/*; do
                if [ -L "$bin" ]; then
                  target=$(readlink "$bin")
                  rm "$bin"
                  makeWrapper "$target" "$bin" \
                    --set LIBGL_DRIVERS_PATH "${mesaDri}" \
                    --prefix LD_LIBRARY_PATH : "/run/current-system/profile/lib"
                fi
              done
            '';
          })
        ];
      };
    };
}
