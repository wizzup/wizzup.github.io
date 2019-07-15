wizzup.github.io
=================

my github page: wizzup.github.io, wizzup.com

# Build the `site` generator

There are 2 ways to build the `site` generator

## Nix build

`default.nix` can build with `nix-build`.

Output is `result/bin/site`

## Cabal

build with `cabal new-build` inside nix-shell.
and use `cabal new-run`

build result is in `_site` directory (master branch)

# Building website and Running local server

`site` binary (result/bin/site) can be use to watching and serve the site locally

```bash
result/bin/site watch
```

Alternatively, use `livereload` on `_site` to auto-refresh the web browers 

```bash
result/bin/site watch --no-server
livereload _site
```
