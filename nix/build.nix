{ sources ? import ./sources.nix 
}: let
  pkgs = import sources.nixpkgs {};
  hPkgs = pkgs.haskell.packages.ghc901;
in { inherit hPkgs; } // hPkgs.callCabal2nix "tex2nix" ../. {}

