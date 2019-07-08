---
title: Develop a Haskell project (hpack/cabal) with Nix
date: 2019-06-28
---

# hello Cabal2nix

**Disclaimer** There are many ways to build a Haskell project on Nix, this approach use `cabal2nix` that just work but might not be the best way to do it.

## Contents

- default.nix : The Nix's expression

```
{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;

let
  drv = haskellPackages.callCabal2nix "hello" ./. {};

  hsTools = with haskellPackages; [
    cabal-install hpack
    hlint hdevtools
  ];

  dev = drv.env.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
               ++ hsTools;
  });

in
  if pkgs.lib.inNixShell then dev else drv
```

- Main.hs     : Program source code

```
main = print "Hello"
```

- package.yaml : Project description (hpack)

```
name: hello

dependencies:
  - base

executables:
  hello:
    main: Main.hs
```

## To build and run

```
$ nix-build

$ result/bin/hello
"Hello"
```

## To dev

The dev env for nix-shell included additional tools for development.

```
$ nix-shell

$ cabal new-run hello

$ hlint Main.hs
$ hdevtools Main.hs
```

## Gotcha

`cabal new-build` generate `.ghc.environment.xxxx` which block `nix-build`, remove that file if there is build error when running `nix-build`.
