# shell.nix
# NOTE: use `hpack` to generate .cabal file and `cabal new-build` to build
#       use `site watch --no-server` to auto update `_site`
#       use `livereload` to watch `_site`

{ pkgs ? import <nixpkgs>{} }:
with pkgs;

let
  site = import ./. {};

  hsPcks = with haskellPackages;
         [ cabal-install hpack
           hlint hie84 ghc-mod84
         ];

  pyPcks = with python3Packages;
         [ livereload ];

  dev = site.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
                ++ hsPcks ++ pyPcks;
  });

in dev

# (import ./.).shellFor {
#   packages = p: with p; [ site cabal-install stack hlint hie84 ghc-mod84];
#   withHoogle = true;
# }
