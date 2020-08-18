{ mkDerivation, aeson, base, bytestring, containers, http-client
, http-types, jose-jwt, oidc-client, safe-exceptions, stdenv, text
}:
mkDerivation {
  pname = "lti13";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers http-client http-types jose-jwt
    oidc-client safe-exceptions text
  ];
  license = stdenv.lib.licenses.lgpl3;
}
