{ nixpkgs ? import <nixpkgs>, system ? builtins.currentSystem
, pkgs ? import <nixpkgs> { inherit system; }, configuration ? <darwin-config>
, darwin ? import <darwin> { inherit configuration system pkgs; } }: {
  nix-darwin = darwin.system;
}
