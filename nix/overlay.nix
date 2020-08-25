{ ghcVer, example }:
self: super:
let inherit (super.haskell.lib) dontCheck;
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ghcVer}" = super.haskell.packages."${ghcVer}".override {
        overrides = hself: hsuper: {
          # tests try to call out to google, which, doesn't work lol
          oidc-client = dontCheck hsuper.oidc-client;
          # lti13 = hsuper.callPackage ../lti13/lti13.nix { };
          # yesod-auth-lti13 = hsuper.callPackage ../yesod-auth-lti13/yesod-auth-lti13.nix { };
          lti13 = hsuper.callCabal2nix "lti13" ../lti13 { };
          yesod-auth-lti13 = hsuper.callCabal2nixWithOptions
              "yesod-auth-lti13"
              ../yesod-auth-lti13
              (super.lib.optionalString example "-f example")
              { };
        };
      };
    };
  };
}

