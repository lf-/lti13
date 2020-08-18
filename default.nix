let sources = import ./nix/sources.nix;
in
{ nixpkgs ? import ./nix/nixpkgs.nix { inherit sources ghcVer; }, ghcVer ? "ghc865" }:

let hpkgs = nixpkgs.haskell.packages."${ghcVer}";
in
{
  inherit (hpkgs) lti13 yesod-auth-lti13;
}
