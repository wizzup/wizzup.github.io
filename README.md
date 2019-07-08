wizzup.github.com
=================

my github page

# Build

There are 2 ways to build the site generator

## Nix build

`default.nix` can build with `nix-build`.

Output is `result/bin/site`

## Cabal

build with `cabal new-build` inside nix-shell.
and use `cabal new-run`



build result is in `_site` directory (master branch submodule)
