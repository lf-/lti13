{ mkDerivation, base, lti13, microlens, random, safe-exceptions
, stdenv, yesod-auth, yesod-core
}:
mkDerivation {
  pname = "yesod-auth-lti13";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base lti13 microlens random safe-exceptions yesod-auth yesod-core
  ];
  license = stdenv.lib.licenses.lgpl3;
}
