let sources = import ./nix/sources.nix;
in

{ nixpkgs ? import ./nix/nixpkgs.nix { inherit sources ghcVer example; }, ghcVer ? "ghc884", example ? true }:

let mypkgs = import ./default.nix { inherit nixpkgs ghcVer; };
in
nixpkgs.haskell.packages."${ghcVer}".shellFor {
  packages = p: with mypkgs; [ lti13 yesod-auth-lti13 ];
  nativeBuildInputs = with nixpkgs.haskell.packages."${ghcVer}"; [
    cabal-install
    Cabal
    brittany
    haskell-language-server
  ] ++ (with nixpkgs; [
    cabal2nix
  ]);
  withHoogle = true;
}
