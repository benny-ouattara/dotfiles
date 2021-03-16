self: super: {

z = with super; stdenv.mkDerivation rec {
  name = "z-${version}";
  version = "125f4dc4";

  src = fetchFromGitHub {
    owner = "rupa";
    repo = "z";
    rev = "125f4dc47e15891739dd8262d5b23077fe8fb9ab";
    sha256 = "0ihs3m8czx2x735r968jl31vs47psbjqfarqpcn891wzd9ygzzm0";
  };

  phases = [ "unpackPhase" "installPhase" ];

  installPhase = ''
    mkdir -p $out/share
    cp -p z.sh $out/share/z.sh
  '';

  meta = with super.lib; {
    description = "Tracks your most used directories, based on 'frecency'.";
    homepage = https://github.com/rupa/z;
    license = licenses.mit;
    maintainers = with maintainers; [ beno ];
    platforms = platforms.unix;
  };
};

}
