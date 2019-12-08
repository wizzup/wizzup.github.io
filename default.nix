## default.nix
## NOTE: There are two ways to build the site
##       1. use nix-build (outside nix-shell) to build site
##          (need to remove `./.ghc.environment.*` first)
##       2. use `cabal new-build` within `nix-shell`

{
pkgs ? import <nixpkgs> {},
compiler ? "ghc865",
}:

with pkgs;

let
  ghcides = import (fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {};
  ghcide = ghcides."ghcide-${compiler}";

  haskellPackages = haskell.packages.${compiler};

  drv = haskellPackages.callCabal2nix "wizzup-github-io" ./. {};

  hsPcks = with haskellPackages;
         [ cabal-install
           hpack
           ghcide
           hlint
         ];

  pyPcks = with python3Packages;
         [ livereload ];

  dev = drv.env.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
               ++ hsPcks
               ++ pyPcks;
  });

in
  if lib.inNixShell then dev else drv
