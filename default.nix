## default.nix
## NOTE: There are two ways to build the site
##       1. use nix-build (outside nix-shell) to build site
##          (need to remove `./.ghc.environment.*` first)
##       2. use `cabal new-build` within `nix-shell`

{
nixpkgs ? import <nixpkgs> {},
compiler ? "default",
doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callCabal2nix "wizzup-github-io" ./. {});

  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

  # TODO: don't forget to change the hie to matched version
  #       can't find the way to automate this
  hie = all-hies.selection {
    selector = p: { inherit (p) ghc864; };
  };

  hsPcks = with pkgs.haskellPackages;
         [ cabal-install
           hpack
           hlint
           hie
         ];

  pyPcks = with pkgs.python3Packages;
         [ livereload ];

  dev = drv.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
                ++ hsPcks ++ pyPcks;
  });

in

  if pkgs.lib.inNixShell then dev else drv
