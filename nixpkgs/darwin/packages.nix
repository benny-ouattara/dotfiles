{ config, lib, pkgs, ... }:

{
  all = [
    pkgs.scalafmt
    pkgs.sketchybar
    pkgs.docker-compose
    pkgs.foreman
    pkgs.yarn
    pkgs.ffmpeg
    pkgs.gnutls
    pkgs.mcron
    # notmuch email
    pkgs.afew
    pkgs.notmuch
    # pkgs.texinfo
    pkgs.stow
    pkgs.poppler
    pkgs.termshark
    pkgs.nmap
    pkgs.hugo
    pkgs.guile
    pkgs.micro
    pkgs.ammonite
    pkgs.automake
    pkgs.pkg-config
    pkgs.qemu
    pkgs.rlwrap
    # stack
    pkgs.maven
    pkgs.pandoc
    pkgs.cask
    pkgs.mu
    pkgs.isync
    # pkgs.protonmail-bridge
    # pkgs.kubectl
    # pkgs.kubectx
    # pkgs.kubetail
    pkgs.cloc
    pkgs.overmind
    pkgs.metals
    # pkgs.fish
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
    pkgs.git
    pkgs.gnupg
    pkgs.htop
    pkgs.jq
    pkgs.ripgrep
    pkgs.silver-searcher
    # pkgs.youtube-dl
    # pkgs.subversion
    pkgs.fd
    pkgs.nixfmt
    pkgs.coreutils-full
    # pkgs.tdlib
    # pkgs.sbcl
    pkgs.clojure
    pkgs.clj-kondo
    pkgs.leiningen
    #pkgs.syncthing
    pkgs.wireguard-tools
    pkgs.tree
    pkgs.tcpdump
    # pkgs.python38
    # pkgs.python38Packages.pip
    # pkgs.python38Packages.python-language-server
    pkgs.mosh
    pkgs.m-cli
    pkgs.hydroxide
    pkgs.gcc
    pkgs.z
    pkgs.custom-scripts
    pkgs.fontconfig

    pkgs.scala
    pkgs.sbt
    pkgs.inetutils
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
