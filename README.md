# lti13

This is a minimal implementation of LTI 1.3 authentication for Haskell. It
supports performing LTI launches and getting most of the interesting fields of
the [resource link request](http://www.imsglobal.org/spec/lti/v1p3/#examplelinkrequest).

## Development setup

Conventions: lines entered in a regular shell start with `$`. Lines that go in
a `nix-shell` shell start with `[nix-shell]$`.

This project provides Nix files for your convenience in creating a working
development environment. If you would like to install Nix, [there is a guide on
the official documentation](https://nixos.org/nix/manual/#ch-installing-binary).

We provide haskell-language-server in our `nix-shell`s.

```
$ nix-shell
# now you can run commands such as:
[nix-shell]$ cabal new-build all
[nix-shell]$ cabal repl
[nix-shell]$ hoogle server
```

## If the LTI-RI gets broken again

See [./referencetool](./referencetool) for details on how to set it up again.

## TODOs

* We are not compliant with [LTI 1.3 §
  5.1.1.5](http://www.imsglobal.org/spec/security/v1p0/#authentication-error-response),
  which requires redirecting failing authentications to the provider. Client code
  can probably make this happen, but we just throw an exception.
* We should probably catch exceptions in our Yesod provider before they force
  client code to 500. Maybe translate them to 401s.
* We are not checking the target_link_uri is the same in initiate as in the
  token. The spec requires this check but it is not used during the auth process, and
  the value from initiate is untrusted and not given to client code anyway, so this is
  not a big problem.
* We should probably provide a method to decode the JWK blob our users are
  expected to store and/or support calling back to the Provider API ourselves.

## Project Information

This codebase is licensed under the GNU LGPLv3 license.

This project has adopted a [Code of Conduct](CODE_OF_CONDUCT.md) based on the
Contributor Covenant.
