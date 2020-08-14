let sources = import ./nix/sources.nix;
in
{ nixpkgs ? import ./nix/nixpkgs.nix { inherit sources ghcVer; }, ghcVer ? "ghc865" }:
nixpkgs.haskell.packages."${ghcVer}".shellFor {
  packages = p: [ (import ./default.nix { inherit nixpkgs ghcVer; }) ];
  nativeBuildInputs = with nixpkgs.haskell.packages."${ghcVer}"; [
    cabal-install
    Cabal
  ] ++ (with nixpkgs; [
    cabal2nix
  ]);
  withHoogle = true;
}
