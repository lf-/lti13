# Research

This document represents the research I have done while building this library,
with useful links to documentation that may be of use to people working on it.

The [Canvas docs describing LTI launches](https://canvas.instructure.com/doc/api/file.lti_dev_key_config.html)
are a lot less dense than the spec. They are useful to get an overview of what
the relationship between the services actually needs to be.

The LTI 1.3 spec is available [here](http://www.imsglobal.org/spec/lti/v1p3/#examplelinkrequest).

Note that LTI 2.0 is a successor to LTI 1.1 using the same authentication
technology determined to be obsolete, whereas LTI 1.3 is a rewrite with new
auth technology.

## Basics

- One or more `client_id` per platform;
  [see Canvas docs](https://community.canvaslms.com/t5/Admin-Guide/How-do-I-configure-an-LTI-key-for-an-account/ta-p/140)
  for what this practically looks like
- One `deployment_id` per context (context: course/section/unit of instruction)
- Reference LTI 1.3 implementation available for testing uses
  [here](https://lti-ri.imsglobal.org/platforms)
- [Full link request](http://www.imsglobal.org/spec/lti/v1p3/#examplelinkrequest)

## State that basic Tool implementations have to keep for each issuer/Platform

- JWK public key JSON URL
- Authentication redirect URL: we need to validate this is the same as the
  token gives us
- `client_id`, because we need to validate that it is in `aud` of the JWKs
  issued by the Platform

## Validating the claims in the token

http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation

We also have to provide these things:

http://www.imsglobal.org/spec/lti/v1p3/#additional-login-parameters



