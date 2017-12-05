{ pkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
with pkgs;
with haskellPackages;

let
  hs = haskell.packages.${compiler}.ghcWithPackages (self: with self; [
          filepath
          hakyll
          pandoc
        ]);
in
stdenv.mkDerivation {
  name = "wizzup-github-io";

  src = ./.;

  buildInputs = [ cabal-install ghc-mod hlint hspec ];

  shellHook = ''
    export PS1="\[\033[1;32m\][ns-hs:\w]\n$ \[\033[0m\]"
    eval "$(egrep ^export "$(type -p ghc)")"
  '';
}
