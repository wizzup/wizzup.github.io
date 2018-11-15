# default.nix
# NOTE: use nix-build (outside nix-shell) to build site

{ haskellPackages ? (import <nixpkgs>{}).haskellPackages }:

haskellPackages.developPackage {
  root = ./.;
}

# haskellPackages.extend (haskell.lib.packageSourceOverrides {
#   site = ./.;
# })

