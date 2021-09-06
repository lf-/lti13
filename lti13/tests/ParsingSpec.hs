{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Tests the various parsers in lti13 round-trip and parse the samples.
module ParsingSpec (spec) where

import           Data.Aeson
import qualified Data.ByteString       as BS
import           Data.FileEmbed
import           TH.RelativePaths
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Web.LTI13
import qualified Web.LTI13             as LTI

-- | This is lifted directly from section E of the LTI 1.3 spec.
ltiSpecExample :: BS.ByteString
ltiSpecExample = $(embedFile =<< pathRelativeToCabalPackage "tests/testdata/example.json")
canvasExample :: BS.ByteString
canvasExample = $(embedFile =<< pathRelativeToCabalPackage "tests/testdata/canvas.json")
deepLinkingExample :: BS.ByteString
deepLinkingExample = $(embedFile =<< pathRelativeToCabalPackage "tests/testdata/deep-linking-req.json")

specExampleOutput :: UncheckedPlatformMessage
specExampleOutput = UncheckedPlatformMessage {
    platformGenericClaims = PlatformGenericClaims
        { ltiVersion = "1.3.0"
        , deploymentId = "07940580-b309-415e-a37c-914d387c1150"
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
        )
        }
    , platformTypeSpecificClaims = ClaimsResourceLink ResourceLinkRequestClaims
        { rlTargetLinkUri = "https://tool.example.com/lti/48320/ruix8782rs"
        , rlAgs = Nothing
        }
    }

unwrapRight :: Either String a -> a
unwrapRight (Right v) = v
unwrapRight (Left e) = error ("was left: " <> e)

spec :: Spec
spec = do
    describe "id" $ do
        prop "id" $ \x y ->
            add x y == add y x
    describe "main parser" $ do
        let checkRoundTrip file = do
                let v = unwrapRight (eitherDecodeStrict' file :: Either String UncheckedPlatformMessage)
                _ <- print v
                let roundTripped = decode . encode $ v
                roundTripped `shouldBe` Just v

        it "parses the file from section E of the lti spec" $ do
            let v = unwrapRight (eitherDecodeStrict' ltiSpecExample :: Either String UncheckedPlatformMessage)
            -- (putStrLn . show $ v) `shouldReturn` ()
            v `shouldBe` specExampleOutput
            let roundTripped = decode . encode $ v
            roundTripped `shouldBe` Just specExampleOutput
        it "roundtrips canvas" $ do
            checkRoundTrip canvasExample
        it "roundtrips the example from the deep linking spec" $ do
            checkRoundTrip deepLinkingExample

add :: Int -> Int -> Int
add x y = x + y
