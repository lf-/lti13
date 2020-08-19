{ mkDerivation, base, base64-bytestring, bytestring, containers
, cryptonite, http-client, lti13, microlens, random
, safe-exceptions, stdenv, text, yesod-auth, yesod-core
}:
mkDerivation {
  pname = "yesod-auth-lti13";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring containers cryptonite http-client
    lti13 microlens random safe-exceptions text yesod-auth yesod-core
  ];
  license = stdenv.lib.licenses.lgpl3;
}
