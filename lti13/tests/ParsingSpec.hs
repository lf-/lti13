{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- | Tests the various parsers in lti13 round-trip and parse the samples.
module ParsingSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.FileEmbed
import TH.RelativePaths
import Web.LTI13
import qualified Web.LTI13 as LTI
import Data.Aeson
import qualified Data.ByteString as BS

-- | This is lifted directly from section E of the LTI 1.3 spec.
ltiSpecExample :: BS.ByteString
ltiSpecExample = $(embedFile =<< pathRelativeToCabalPackage "tests/example.json")

specExampleOutput :: UncheckedLtiTokenClaims
specExampleOutput = UncheckedLtiTokenClaims {
      messageType = "LtiResourceLinkRequest"
    , ltiVersion = "1.3.0"
    , deploymentId = "07940580-b309-415e-a37c-914d387c1150"
    , targetLinkUri = "https://tool.example.com/lti/48320/ruix8782rs"
    , roles = [Other "http://purl.imsglobal.org/vocab/lis/v2/institution/person#Student",Learner,Mentor]
    , email = Just "jane@platform.example.edu"
    , displayName = Just "Ms Jane Marie Doe"
    , firstName = Just "Jane"
    , lastName = Just "Doe"
    , LTI.context = Just (
        ContextClaim
        { contextId = "c1d887f0-a1a3-4bca-ae25-c375edcc131a"
        , contextLabel = Just "ECON 1010"
        , contextTitle = Just "Economics as a Social Science"}
    )
    , lis = Just (
        LisClaim {
              personSourcedId = Just "example.edu:71ee7e42-f6d2-414a-80db-b69ac2defd4"
            , outcomeServiceUrl = Nothing
            , courseOfferingSourcedId = Just "example.edu:SI182-F16"
            , courseSectionSourcedId = Just "example.edu:SI182-001-F16"
            , resultSourcedId = Nothing
        }
    )}

spec :: Spec
spec = do
    describe "id" $ do
        prop "id" $ \x y ->
            add x y == add y x
    describe "main parser" $ do
        it "parses the file from section E of the lti spec" $ do
            let v = decodeStrict ltiSpecExample :: Maybe UncheckedLtiTokenClaims
            -- (putStrLn . show $ v) `shouldReturn` ()
            v `shouldBe` Just specExampleOutput
            let roundTripped = decode . encode =<< v
            roundTripped `shouldBe` Just specExampleOutput

add :: Int -> Int -> Int
add x y = x + y
