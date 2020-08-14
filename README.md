# Minimal LTI 1.3 implementation for Haskell

This is a *very* minimal and experimental implementation of LTI 1.3
authentication for Haskell.

# Warning

This project has not been tested yet. Using it right now is probably foolish.

## Development setup

This project provides Nix files for your convenience in creating a working
environment.

Use e.g. `nix-shell` to get a shell with `cabal` and the appropriate dev
packages.

Build with `cabal new-build`. Get a development repl with `cabal repl`.

## TODOs

* We are not compliant with [LTI 1.3 §
  5.1.1.5](http://www.imsglobal.org/spec/security/v1p0/#authentication-error-response),
  which requires redirecting failing authentications to the provider. Client code
  can probably make this happen, but we just throw an exception.
* I am not sure if we handle nonces effectively: I am pretty sure it is not
  possible to replay the second half of an authentication request since we
  discard the state, so an attacker would not have a valid state (?) and would
  get stopped.
