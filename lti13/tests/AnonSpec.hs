{-# LANGUAGE OverloadedStrings #-}
-- | Tests the various parsers in lti13 round-trip and parse the samples.
module AnonSpec (spec) where

import Test.Hspec hiding (context)
import Web.LTI13

exampleToken :: UncheckedPlatformMessage
exampleToken = UncheckedPlatformMessage
    { platformGenericClaims = PlatformGenericClaims
        { ltiVersion = "1.3.0"
        , deploymentId = "07940580-b309-415e-a37c-914d387c1150"
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
        }
    , platformTypeSpecificClaims = ClaimsResourceLink ResourceLinkRequestClaims
        { rlTargetLinkUri = "https://tool.example.com/lti/48320/ruix8782rs"
        , rlAgs = Just AgsClaim
            { agsClaimScope = [AgsScopeLineItem, AgsScopeScore]
            , agsClaimLineItemUrl = Just "blah"
            , agsClaimLineItemsUrl = Just "blah"
            }
        }
    }
    where
        secret = Just "secret"

exampleTokenDl :: UncheckedPlatformMessage
exampleTokenDl = UncheckedPlatformMessage
    { platformGenericClaims = PlatformGenericClaims
        { ltiVersion = "1.3.0"
        , deploymentId = "07940580-b309-415e-a37c-914d387c1150"
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
        }
    , platformTypeSpecificClaims = ClaimsDeepLink DeepLinkRequestClaims
        { dlSettings = Just DeepLinkingSettings
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
    }
    where
        secret = Just "secret"

spec :: Spec
spec = do
    it "removes secrets" $ do
        show exampleToken `shouldContain` "secret"
        (show . anonymize $ exampleToken) `shouldNotContain` "secret"
    it "removes secrets from deep linking tokens" $ do
        show exampleTokenDl `shouldContain` "secret"
        (show . anonymize $ exampleTokenDl) `shouldNotContain` "secret"
