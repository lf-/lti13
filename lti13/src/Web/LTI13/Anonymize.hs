-- | Anonymize various objects from @lti13@.
module Web.LTI13.Anonymize (ToAnonymized(..)) where
import           Data.Text       (Text)
import           Web.LTI13.Types

-- | Removes PII of the user from an object, retaining only information about
--   the system in general or the context.
--
--   Fields that are 'Maybe' are kept as 'Maybe', with the contents replaced
--   with @"**"@ if they were 'Just' and otherwise kept as 'Nothing'.
class ToAnonymized t where
    anonymize :: t -> t

anonymizedText :: Text -> Text
anonymizedText = const "**"

instance ToAnonymized UncheckedPlatformMessage where
    anonymize UncheckedPlatformMessage {..} =
        UncheckedPlatformMessage
            { platformGenericClaims = anonymize platformGenericClaims
            , platformTypeSpecificClaims = anonymize platformTypeSpecificClaims
            }

instance ToAnonymized LisClaim where
    anonymize LisClaim {..} = LisClaim
        -- we really don't know what they will put in this; it might be
        -- student specific
        { personSourcedId = anonymizedText <$> personSourcedId
        -- spec strongly suggests this be the same across launches ie only
        -- identifies the context
        , outcomeServiceUrl
        , courseOfferingSourcedId
        , courseSectionSourcedId
        -- likewise with personSourcedId, we don't know what will be put in
        -- here. it's probably a guid but let's be safe
        , resultSourcedId = anonymizedText <$> resultSourcedId
        }

instance ToAnonymized DeepLinkingSettings where
    anonymize DeepLinkingSettings {..} = DeepLinkingSettings
        { dlsReturnUrl
        -- these are probably not personally identifiable but we have no
        -- idea what's in them and we have no reason to have them for
        -- debugging purposes
        , dlsTitle = anonymizedText <$> dlsTitle
        , dlsData = anonymizedText <$> dlsData
        , dlsAcceptTypes
        , dlsAutoCreate
        , dlsAcceptMediaTypes
        , dlsAcceptMultiple
        , dlsAcceptPresentationDocumentTargets
        }

instance ToAnonymized PlatformTypeSpecificClaims where
    anonymize (ClaimsResourceLink v) = ClaimsResourceLink $ anonymize v
    anonymize (ClaimsDeepLink v)     = ClaimsDeepLink $ anonymize v

instance ToAnonymized ResourceLinkRequestClaims where
    anonymize ResourceLinkRequestClaims {..} =
        ResourceLinkRequestClaims
            { rlTargetLinkUri
            -- this should not identify the user; it is at most a class item
            , rlAgs
            }

instance ToAnonymized DeepLinkRequestClaims where
    anonymize DeepLinkRequestClaims {..} =
        DeepLinkRequestClaims
            { dlSettings = anonymize <$> dlSettings
            }

instance ToAnonymized PlatformGenericClaims where
    anonymize PlatformGenericClaims {..} = PlatformGenericClaims
        { ltiVersion
        , deploymentId
        , roles
        , displayName = anonymizedText <$> displayName
        , firstName = anonymizedText <$> firstName
        , lastName = anonymizedText <$> lastName
        , context
        , email = anonymizedText <$> email
        , lis = anonymize <$> lis
        }

