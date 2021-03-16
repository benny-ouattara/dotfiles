{pkgs, ...}:

{
  java8 = pkgs.myEnvFun {
    name = "java8";
    buildInputs = [pkgs.jdk8];
  };

  java11 = pkgs.myEnvFun {
    name = "java11";
    buildInputs = [pkgs.jdk11];
  };

#  java14 = pkgs.myEnvFun {
#    name = "java14";
#    buildInputs = [pkgs.jdk14];
#  };

  python2 = pkgs.myEnvFun {
    name = "python2";
    buildInputs = [pkgs.python2];
  };

  python3 = pkgs.myEnvFun {
    name = "python3";
    buildInputs = [
      pkgs.python3
      pkgs.python37Packages.pip
      pkgs.python37Packages.requests
      pkgs.python37Packages.isort
      pkgs.python37Packages.pytest-black
      pkgs.python37Packages.pytest-flakes
      pkgs.python37Packages.flake8
      pkgs.pipenv
    ];
  };

  scala = pkgs.myEnvFun {
    name = "scala";
    buildInputs = [pkgs.scala];
  };

  dotty = pkgs.myEnvFun {
    name = "dotty";
    buildInputs = [pkgs.dotty];
  };

  clojure = pkgs.myEnvFun {
    name = "clojure";
    buildInputs = [pkgs.clojure pkgs.leiningen];
  };

  ghc88 = pkgs.myEnvFun {
    name = "ghc88";
    buildInputs = [pkgs.ghc];
  };

  # ghc86Env = pkgs.myEnvFun {
  #   name = "ghc86Env";
  #   buildInputs = [ pkgs.haskell.packages.ghc865 ];
  # };

  # ghc88Env = pkgs.myEnvFun {
  #   name = "ghc88";
  #   buildInputs = [ pkgs.haskell.packages.ghc883 ];
  # };

  # ghc810Env = pkgs.myEnvFun {
  #   name = "ghc810";
  #   buildInputs = [ pkgs.haskell.package.ghc8101 ];
  # };
}
