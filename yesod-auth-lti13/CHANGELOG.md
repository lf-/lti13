# Revision history for yesod-auth-lti13

## 0.2.0.2 -- 2021-07-28

* lti13: add anonymization of tokens

## 0.2.0.1 -- 2021-02-26

* Fix version bounds
* Change the LTI-RI URL as our previous one got deleted (?!)

## 0.2.0.0 -- 2021-01-09

* BREAKING: Change the signatures of the `getLtiIss`, `getLtiSub` and
  `getLtiToken` functions to operate on `credsExtra` instead of sessions.

## 0.1.2.2 -- 2020-09-19

* No changes

## 0.1.2.1 -- 2020-09-16

* No changes

## 0.1.2.0 -- 2020-09-16

* Add `authLTI13WithWidget`

## 0.1.1.0 -- 2020-09-15

* Handle Canvas Cloud setting all their issuers the same.
* Remove dependency on changed jose-jwt. Thanks @tekul for the help on this.

## 0.1.0.0 -- 2020-08-13

* Unreleased

