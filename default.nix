# default.nix
# NOTE: There are two ways to build the site
#       1. use nix-build (outside nix-shell) to build site
#          (may need to remove ./.ghc.environment.*)
#       2. use `cabal new-build` within nix-shell

{ haskellPackages ? (import <nixpkgs>{}).haskellPackages }:

haskellPackages.developPackage {
  root = ./.;
}

# haskellPackages.extend (haskell.lib.packageSourceOverrides {
#   site = ./.;
# })

