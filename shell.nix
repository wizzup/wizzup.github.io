# shell.nix for nix-shell
#
{ pkgs ? import <nixpkgs> {} }:

let
  hs = pkgs.haskellPackages.ghcWithHoogle (self: with self; [
          hakyll
          pandoc
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "haskell-shell";
  buildInputs = with pkgs.haskellPackages; [ hs cabal-install ghc-mod hlint hspec  ];

  shellHook = ''
    export PS1="\[\033[1;32m\][ns-hs:\w]\n$ \[\033[0m\]"
    eval "$(egrep ^export "$(type -p ghc)")"
  '';
}
