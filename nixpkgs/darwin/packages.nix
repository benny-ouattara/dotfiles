{ config, lib, pkgs, ... }:

{
  all = [
    pkgs.starship
    pkgs.rlwrap
    # stack
    pkgs.maven
    pkgs.pandoc
    pkgs.cask
    pkgs.mu
    pkgs.isync
    pkgs.protonmail-bridge
    # pkgs.kubectl
    # pkgs.kubectx
    # pkgs.kubetail
    pkgs.cloc
    pkgs.overmind
    pkgs.metals
    pkgs.fish
    pkgs.neofetch
    pkgs.ranger
    pkgs.zsh
    pkgs.skhd
    pkgs.ansible
    pkgs.tmux
    pkgs.reattach-to-user-namespace
    pkgs.direnv
    pkgs.neovim
    pkgs.spotify-tui
    pkgs.pass
    pkgs.portaudio
    pkgs.curl
    pkgs.fzf
    pkgs.git
    pkgs.gnupg
    pkgs.htop
    pkgs.jq
    pkgs.ripgrep
    pkgs.silver-searcher
    pkgs.youtube-dl
    pkgs.subversion
    pkgs.nodejs
    pkgs.fd
    pkgs.nixfmt
    pkgs.coreutils-full
    pkgs.tdlib
    # pkgs.sbcl
    pkgs.clojure
    pkgs.leiningen
    #pkgs.syncthing
    pkgs.wireguard-tools
    pkgs.tree
    pkgs.tcpdump
    pkgs.python37
    pkgs.python37Packages.pip
    pkgs.python37Packages.python-language-server
    pkgs.mosh
    pkgs.m-cli
    pkgs.hydroxide
    pkgs.gcc
    pkgs.z
    pkgs.custom-scripts
    pkgs.fontconfig

    # envs
    # envs.java8
    # envs.java11
    # envs.java14
    # envs.python2
    # envs.python3
    # envs.scala
    # envs.dotty
    # envs.clojure
    # envs.ghc88
  ];
}
