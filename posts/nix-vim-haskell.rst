---
title: Nix, Vim and Haskell
date: 2017-05-31
---

This is how I setup nix-shell environments vim and hakyll.

This is *shelll.nix*

.. code-block:: nix

   { pkgs ? import <nixpkgs> {} }:

   let
     hs = pkgs.haskellPackages.ghcWithHoogle (self: with self; [
             hakyll
           ]);
   in
   pkgs.stdenv.mkDerivation {
     name = "haskell-shell";
     buildInputs = with pkgs.haskellPackages; [ hs cabal-install ghc-mod hlint hspec  ];

     shellHook = ''
       export PS1="\[\033[1;32m\][ns-hs:\w]\n$ \[\033[0m\]"
       eval "$(egrep ^export "$(type -p ghc)")"
     '';
   }

I use `cabal` to do the build

.. code-block:: bash

   $ cabal build
