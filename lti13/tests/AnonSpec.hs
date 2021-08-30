{-# LANGUAGE OverloadedStrings #-}
-- | Tests the various parsers in lti13 round-trip and parse the samples.
module AnonSpec (spec) where

import Test.Hspec hiding (context)
import Web.LTI13

exampleToken :: UncheckedLtiTokenClaims
exampleToken = UncheckedLtiTokenClaims
    { messageType = LtiResourceLinkRequest
    , ltiVersion = "1.3.0"
    , deploymentId = "07940580-b309-415e-a37c-914d387c1150"
    , targetLinkUri = "https://tool.example.com/lti/48320/ruix8782rs"
    , roles = [Other "http://purl.imsglobal.org/vocab/lis/v2/institution/person#Student",Learner,Mentor]
    , email = secret
    , displayName = secret
    , firstName = secret
    , lastName = secret
    , context = Just (
        ContextClaim
        { contextId = "c1d887f0-a1a3-4bca-ae25-c375edcc131a"
        , contextLabel = Just "ECON 1010"
        , contextTitle = Just "Economics as a Social Science"}
    )
    , lis = Just (
        LisClaim {
              personSourcedId = secret
            , outcomeServiceUrl = Nothing
            , courseOfferingSourcedId = Just "example.edu:SI182-F16"
            , courseSectionSourcedId = Just "example.edu:SI182-001-F16"
            , resultSourcedId = Nothing
        }
    )
    , tokAgs = Just AgsClaim
        { agsClaimScope = [AgsScopeLineItem, AgsScopeScore]
        , agsClaimLineItemUrl = Just "blah"
        , agsClaimLineItemsUrl = Just "blah"
        }
    , tokDlSettings = Just DeepLinkingSettings
        { dlsData = secret
        , dlsTitle = secret
        , dlsReturnUrl = "https://example.com"
        , dlsAutoCreate = Just False
        , dlsAcceptTypes = []
        , dlsAcceptMediaTypes = Just ""
        , dlsAcceptPresentationDocumentTargets = [TargetEmbed]
        , dlsAcceptMultiple = Just False
        }
    }
    where
        secret = Just "secret"

spec :: Spec
spec = do
    it "removes secrets" $ do
        show exampleToken `shouldContain` "secret"
        (show . anonymizeLtiTokenForLogging $ exampleToken) `shouldNotContain` "secret"
