---
date: 2017-07-29
title: Various ways to use GHC in nix-shell
---

There are various ways to use GHC in nix-shell

1. System's default GHC

```
$ nix-shell -p ghc
```

```
[nix-shell:~]$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.0.2

[nix-shell:~]$ ghc-pkg list
/nix/store/593fzzg81xpqrwniqfbvm9z1xhjivh8a-ghc-8.0.2/lib/ghc-8.0.2/package.conf.d
    Cabal-1.24.2.0
    array-0.5.1.1
    base-4.9.1.0
    binary-0.8.3.0
    bytestring-0.10.8.1
    containers-0.5.7.1
    deepseq-1.4.2.0
    directory-1.3.0.0
    filepath-1.4.1.1
    ghc-8.0.2
    ghc-boot-8.0.2
    ghc-boot-th-8.0.2
    ghc-prim-0.5.0.0
    ghci-8.0.2
    haskeline-0.7.3.0
    hoopl-3.10.2.1
    hpc-0.6.0.3
    integer-gmp-1.0.0.1
    pretty-1.1.3.3
    process-1.4.3.0
    rts-1.0
    template-haskell-2.11.1.0
    terminfo-0.4.0.2
    time-1.6.0.1
    transformers-0.5.2.0
    unix-2.7.2.1
    xhtml-3000.2.1
```

2. System's default GHC with additional libraries

```
$ nix-shell -p 'haskellPackages.ghcWithPackages (self: with self; [ random ])'
```

```
[nix-shell:~]$ ghc-pkg list
/nix/store/d58zrdfijpj60pbwvg4cqmv8n9qcdq0d-ghc-8.0.2-with-packages/lib/ghc-8.0.2/package.conf.d
    Cabal-1.24.2.0
    array-0.5.1.1
    base-4.9.1.0
    binary-0.8.3.0
    bytestring-0.10.8.1
    containers-0.5.7.1
    deepseq-1.4.2.0
    directory-1.3.0.0
    filepath-1.4.1.1
    ghc-8.0.2
    ghc-boot-8.0.2
    ghc-boot-th-8.0.2
    ghc-prim-0.5.0.0
    ghci-8.0.2
    haskeline-0.7.3.0
    hoopl-3.10.2.1
    hpc-0.6.0.3
    integer-gmp-1.0.0.1
    pretty-1.1.3.3
    process-1.4.3.0
    random-1.1
    rts-1.0
    template-haskell-2.11.1.0
    terminfo-0.4.0.2
    time-1.6.0.1
    transformers-0.5.2.0
    unix-2.7.2.1
    xhtml-3000.2.1
```

3. Specific `ghc` version with specific libraries

```
$ nix-shell -p 'haskell.packages.ghc7103.ghcWithPackages (self: with self; [ random ])'
```

```
[nix-shell:~]$ ghc-pkg list
/nix/store/r17mbj8100rp15v8xfq899lvjx7g80ir-ghc-7.10.3-with-packages/lib/ghc-7.10.3/package.conf.d
   Cabal-1.22.5.0
   array-0.5.1.0
   base-4.8.2.0
   bin-package-db-0.0.0.0
   binary-0.7.5.0
   bytestring-0.10.6.0
   containers-0.5.6.2
   deepseq-1.4.1.1
   directory-1.2.2.0
   filepath-1.4.0.0
   ghc-7.10.3
   ghc-prim-0.4.0.0
   haskeline-0.7.2.1
   hoopl-3.10.0.2
   hpc-0.6.0.2
   integer-gmp-1.0.0.0
   pretty-1.1.2.0
   process-1.2.3.0
   random-1.1
   rts-1.0
   template-haskell-2.10.0.0
   terminfo-0.4.0.1
   time-1.5.0.1
   transformers-0.4.2.0
   unix-2.7.1.0
   xhtml-3000.2.1
```

4. Writing `shell.nix` for more complex usage

```
# shell.nix for nix-shell (haskell)

{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithHoogle (self: with self; [
          hspec
          split
        ]);
in
pkgs.mkShell {
  name = "haskell-shell";
  buildInputs = with pkgs.haskellPackages; [ ghc cabal-install ghc-mod hlint ];

  shellHook = ''
    eval "$(egrep ^export "$(type -p ghc)")"
    export PS1="\[\033[1;32m\][ns-hs:\w]\n$ \[\033[0m\]"
  '';
}
```
