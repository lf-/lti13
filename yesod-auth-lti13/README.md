# yesod-auth-lti13

A [lti13](https://hackage.haskell.org/package/lti13) based authentication
provider for Yesod.

## Usage

Implement an `instance YesodAuthLTI13 App` for your Yesod site, using your
persistence mechanisms. See the example for details.

To build the example, pass `-f example` with your cabal commands. You can also
`cabal configure -f example` to make it apply to commands by default (and also
enable it for haskell-language-server).

A sample configuration of the LTI 1.3 reference implementation for a site using
this library is available here: https://lti-ri.imsglobal.org/platforms/1255/

The following configuration is used on the provider (LMS) side, assuming your
`AuthR` is `/auth`:

* `oidc_initiation_url`: https://YOURAPPROOT/auth/page/lti13/initiate
* `target_link_uri`: https://YOURAPPROOT
* Public JWK URL: https://YOURAPPROOT/auth/page/lti13/jwks
* Redirect URLs: https://YOURAPPROOT/auth/page/lti13/authenticate
