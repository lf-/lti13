let sources = import ./nix/sources.nix;
in

{ nixpkgs ? import ./nix/nixpkgs.nix { inherit sources ghcVer; }, ghcVer ? "ghc865" }:

let mypkgs = import ./default.nix { inherit nixpkgs ghcVer; };
in
nixpkgs.haskell.packages."${ghcVer}".shellFor {
  packages = p: with mypkgs; [ lti13 yesod-auth-lti13 ];
  nativeBuildInputs = with nixpkgs.haskell.packages."${ghcVer}"; [
    cabal-install
    Cabal
  ] ++ (with nixpkgs; [
    cabal2nix
  ]);
  withHoogle = true;
}
