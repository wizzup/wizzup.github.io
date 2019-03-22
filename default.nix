# default.nix
# NOTE: There are two ways to build the site
#       1. use nix-build (outside nix-shell) to build site
#          (need to remove `./.ghc.environment.*` first)
#       2. use `cabal new-build` within `nix-shell`

# Using haskell helper instead of mkDerivation
{ haskellPackages ? (import <nixpkgs>{}).haskellPackages }:
haskellPackages.developPackage {
  root = ./.;
}

# Using this method will get blocked with broken packages (if any)
# and will require `{ allowBroken = true; }` setting in config.nix
#
# { pkgs ? import <nixpkgs>{} }:
# with pkgs;
# with haskellPackages;
#
# haskellPackages.extend (haskell.lib.packageSourceOverrides {
#   site = ./.;
# })

