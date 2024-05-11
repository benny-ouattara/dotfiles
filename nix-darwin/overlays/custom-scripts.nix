self: super: {
  custom-scripts = with self;
    stdenv.mkDerivation {
      name = "custom-scripts";

      src = builtins.filterSource
        (path: type: type != "directory" || baseNameOf path != ".git")
        ~/.config/scripts;

      buildInputs = [ ];

      installPhase = ''
        mkdir -p $out/bin
        cp -R ./* $out/bin
      '';

      meta = with pkgs.lib; {
        description = "Ben O. custom scripts";
        homepage = "https://github.com/benny-ouattara";
        license = licenses.mit;
        maintainers = with maintainers; [ beno ];
        platforms = platforms.darwin;
      };
    };
}
