# shell.nix
# NOTE: use hpack to generate .cabal file

{ pkgs ? import <nixpkgs>{} }:
with pkgs;
with haskellPackages;

let
  site = import ./. {};

  dev = site.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
               ++ [ cabal-install hpack
                    hlint hie84 ghc-mod84
                  ];
  });

in dev

# (import ./.).shellFor {
#   packages = p: with p; [ site cabal-install stack hlint hie84 ghc-mod84];
#   withHoogle = true;
# }
