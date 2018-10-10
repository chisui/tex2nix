{ pkgs ? import ./nixpkgs.pinned.nix }:
pkgs.haskell.packages.ghc843.callCabal2nix "tex2nix" ./. {}
