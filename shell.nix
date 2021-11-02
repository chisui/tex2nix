{ sources ? import ./nix/sources.nix 
}: let
  drv = import ./nix/build.nix { inherit sources; };
in import ./nix/mkHaskellShell.nix { inherit sources; } drv

