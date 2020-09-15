# lti13

This is a minimal implementation of LTI 1.3 authentication for Haskell. It
supports performing LTI launches and getting most of the interesting fields of
the [resource link request](http://www.imsglobal.org/spec/lti/v1p3/#examplelinkrequest).

This library is intended to be used in developing integrations with web
frameworks, although it can be used directly. A sample integration is
[yesod-auth-lti13](https://hackage.haskell.org/package/yesod-auth-lti13).

## Correct usage

Client code is expected to maintain a CSRF token, the `state` parameter, in
session storage and check it is the same as the one from `handleAuthResponse`,
failing authentication if it is not. Future versions of the library may
introduce a mandatory callback to ensure clients do this. For an example of
this, see the yesod-auth-lti13 sources.
