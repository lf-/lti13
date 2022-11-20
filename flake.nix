{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVer = "ghc925";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };

        in
        {
          packages = rec {
            default = lti13;
            inherit (pkgs.haskell.packages.${ghcVer}) lti13 yesod-auth-lti13;
          };

          # for debugging
          # inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: with self.packages.${system}; [ lti13 yesod-auth-lti13 ];
              withHoogle = true;
              buildInputs = with haskellPackages; [
                haskell-language-server
                cabal-install
              ] ++ (with pkgs; [
                hpack
              ]);
              # Change the prompt to show that you are in a devShell
              # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay
          (prev: hfinal: hprev:
            let hlib = prev.haskell.lib; in
            {
              lti13 = hprev.callCabal2nix "lti13" ./lti13 { };
              yesod-auth-lti13 = hprev.callCabal2nix "yesod-auth-lti13" ./yesod-auth-lti13 { };

              # For some reason the test suite hangs.
              ListLike = hlib.dontCheck hprev.ListLike;
            });
      };
    };
}
