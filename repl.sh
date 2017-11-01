#!/usr/bin/env bash

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist

#this is a workaround while cabal-repl is broken (https://github.com/haskell/cabal/issues/4602)
nix-shell -A vellian.env release.nix --run "cabal update; runhaskell Setup.hs configure; runhaskell Setup.hs repl lib:vellian"

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist
