let sources = import ./nix/sources.nix;
in
{ nixpkgs ? import ./nix/nixpkgs.nix { inherit sources ghcVer; }, ghcVer ? "ghc865" }:
nixpkgs.haskell.packages."${ghcVer}".callPackage ./lti13.nix { }
