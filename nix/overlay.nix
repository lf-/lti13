{ ghcVer }:
self: super:
let inherit (super.haskell.lib) dontCheck;
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ghcVer}" = super.haskell.packages."${ghcVer}".override {
        overrides = hself: hsuper: {
          # tests try to call out to google, which, doesn't work lol
          oidc-client = dontCheck hsuper.oidc-client;
        };
      };
    };
  };
}

