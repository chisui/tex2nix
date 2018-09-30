{ pkgs ? import ./nixpkgs.pinned.nix }:
let
  drv = pkgs.haskell.packages.ghc861.callCabal2nix "tex2nix" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv

