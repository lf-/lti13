-- | Data model for LTI 1.3
module Web.LTI13.Types (
      module Web.LTI13.Claims
      -- * Base
      , Role(..)
      , MessageType(..)
      , LisClaim(..)
      , ContextClaim(..)
      , UncheckedLtiTokenClaims(..)
      , LtiTokenClaims(..)
      , AnonymizedLtiTokenClaims(..)

      -- * Deep Linking
      -- ** Incoming (to tool)
      , DeepLinkingSettings(..)
      , DeepLinkType(..)
      , PresentationTarget(..)

      -- ** Outgoing (to platform)
      , DeepLinkingResponse(..)
      , TimeWindow(..)
      , Iframe(..)
      , ResourceLinkLineItem(..)
      , LtiResourceLinkInfo(..)
      , DeepLinkContentItem(..)

      -- * Assignment and Grade Services
      , AgsClaim(..)
      , AgsScope(..)
) where
import qualified Control.Monad.Fail  as Fail
import           Data.Aeson          (FromJSON (..), Object, ToJSON (..),
                                      constructorTagModifier,
                                      fieldLabelModifier, object, pairs,
                                      withObject, withText, (.:), (.:?), (.=))
import qualified Data.Aeson          as A
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Aeson.Types    (Parser)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (ZonedTime)
import           Data.Tuple          (swap)
import           GHC.Generics        (Generic)
import           Jose.Jwt            (JwtClaims (..))

import           Web.LTI13.Claims
import           Web.LTI13.THHelpers

-- | Parses a JSON text field to a fixed expected value, failing otherwise
parseFixed :: (FromJSON a, Eq a, Show a) => Object -> Text -> a -> Parser a
parseFixed obj field fixedVal = parseFixedAny obj field (pure fixedVal)

parseFixedAny :: (FromJSON a, Eq a, Show a) => Object -> Text -> [a] -> Parser a
parseFixedAny obj field fixedVals =
    obj .: field >>= \v ->
        if v `elem` fixedVals then
            return v
        else
            fail $ "field " <> show field <> " was not any of the accepted values " <> show fixedVals

-- | Makes a mapping between @(A | B | C | Other s)@ and some other type,
--   declaratively.
mkBijection :: (Eq a, Eq b) => (a -> b, b -> a) -> [(a, b)] -> (a -> b, b -> a)
mkBijection (defaultAToB, defaultBToA) mapping =
    -- FIXME: this should probably use DeriveGeneric or similar but I don't
    -- understand it.
    let aToB a = case lookup a mapping of
            Nothing -> defaultAToB a
            Just v  -> v
        bToA b = case lookup b (swap <$> mapping) of
            Nothing -> defaultBToA b
            Just v  -> v
    in (aToB, bToA)


------------------------------------------------------------
-- Deep Linking
------------------------------------------------------------

-- | Type of deep link that can be transferred to the platform
data DeepLinkType =
    -- | <https://www.imsglobal.org/spec/lti-dl/v2p0#link Web hyperlink>
      LinkTypeLink
    -- | <https://www.imsglobal.org/spec/lti-dl/v2p0#lti-resource-link Link to
    --    a LTI resource>. May have a gradebook entry.
    | LinkTypeLtiResourceLink
    -- | <https://www.imsglobal.org/spec/lti-dl/v2p0#file File> transferred
    --   from tool to platform.
    | LinkTypeFile
    -- | <https://www.imsglobal.org/spec/lti-dl/v2p0#html-fragment HTML
    --    fragment> to be embedded on the platform
    | LinkTypeHtml
    -- | <https://www.imsglobal.org/spec/lti-dl/v2p0#image Image> that will
    --   be embedded as an @<img>@ on the page.
    | LinkTypeImage
    -- | Any custom deep link type.
    | LinkTypeOther Text
    deriving (Eq, Show)

deepLinkTypeToString :: DeepLinkType -> Text
deepLinkTypeFromString :: Text -> DeepLinkType
(deepLinkTypeToString, deepLinkTypeFromString) = mkBijection (unOther, LinkTypeOther) [
        (LinkTypeLink, "link"),
        (LinkTypeLtiResourceLink, "ltiResourceLink"),
        (LinkTypeFile, "file"),
        (LinkTypeHtml, "html"),
        (LinkTypeImage, "image")
    ]
    where unOther (LinkTypeOther o) = o
          unOther _                 = error "bug: DeepLinkType missing"

instance FromJSON DeepLinkType where
    parseJSON = withText "DeepLinkType" $ return . deepLinkTypeFromString

instance ToJSON DeepLinkType where
    toJSON = A.String . deepLinkTypeToString

-- | Window of time that an item might be available for.
data TimeWindow = TimeWindow
    { timeWindowBegin :: ZonedTime
    , timeWindowEnd   :: ZonedTime
    }

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = \case
            "timeWindowBegin" -> "startDateTime"
            "timeWindowEnd"   -> "endDateTime"
            _                 -> error "missing item"
        }
    ''TimeWindow
    )

-- | Configures an iframe to embed into the provider.
data Iframe = Iframe
    { iframeWidth  :: Int
    , iframeHeight :: Int
    }

$(deriveJSON
    defaultOptions { fieldLabelModifier = unPrefix "iframe" }
    ''Iframe)

-- | Information to provide to create a line item in the gradebook with a deep
--   linking request.
data ResourceLinkLineItem = ResourceLinkLineItem
    { lineItemLabel        :: Maybe Text
    -- ^ Label to use on the line item. If absent, the title of the link will
    -- be used instead.
    , lineItemScoreMaximum :: Float
    -- ^ Maximum score. >0.
    , lineItemResourceId   :: Maybe Text
    -- ^ <https://www.imsglobal.org/spec/lti-ags/v2p0/#tool-resource-identifier-resourceid
    -- Resource ID used in the gradebook>. Multiple line items may share one @resourceId@.
    , lineItemTag          :: Maybe Text
    -- ^ <https://www.imsglobal.org/spec/lti-ags/v2p0/#tag Tag for the line item>.
    }

$(deriveJSON
    A.defaultOptions
        {
            fieldLabelModifier = unPrefix "lineItem"
        }
    ''ResourceLinkLineItem
    )

-- | Information that goes on a @ltiResourceLink@ type item returned in a deep linking response.
data LtiResourceLinkInfo = LtiResourceLinkInfo
    { rlinkUrl         :: Maybe Text
    -- ^ Target URL. If absent, the provider will link to the base URL of the tool.
    , rlinkTitle       :: Maybe Text
    -- ^ Title/heading of the content.
    , rlinkIframe      :: Maybe Iframe
    -- ^ Dimensions of an iframe to use with the link, if any.
    , rlinkLineItem    :: Maybe ResourceLinkLineItem
    -- ^ Details of the line item to create for this resource link.
    , rlinkAvailable   :: Maybe TimeWindow
    -- ^ When the link can be seen or clicked on.
    , rlinkSubmittable :: Maybe TimeWindow
    -- ^ When the link can take submissions.
    }

$(deriveJSON
    A.defaultOptions
        {
            fieldLabelModifier = unPrefix "rlink"
        }
    ''LtiResourceLinkInfo
    )

-- | Types defined in
--   <https://www.imsglobal.org/spec/lti-dl/v2p0#content-item-types section 2> of
--   the Deep Linking spec. See also 'DeepLinkType'.
newtype DeepLinkContentItem =
    -- FIXME: This is obviously missing most of the types. They should be
    -- added, at some point
    LinkItemLtiResourceLink LtiResourceLinkInfo

$(deriveJSON
    A.defaultOptions
        { A.sumEncoding = A.TaggedObject "type" (error "dont do this")
        , A.constructorTagModifier = error "a"
        }
    ''DeepLinkContentItem
    )

-- | Ways to show content on the platform
data PresentationTarget =
      TargetEmbed
    | TargetWindow
    | TargetIframe
    deriving (Show, Eq)

$(deriveJSON (defaultOptions {
    constructorTagModifier = \case
         "TargetEmbed"  -> "embed"
         "TargetWindow" -> "window"
         "TargetIframe" -> "iframe"
         v              -> error v
    }) ''PresentationTarget)

-- | Incoming linking settings claim
data DeepLinkingSettings = DeepLinkingSettings
    { dlsReturnUrl                         :: Text
    , dlsAcceptTypes                       :: [DeepLinkType]
    , dlsAcceptPresentationDocumentTargets :: [PresentationTarget]
    , dlsAcceptMediaTypes                  :: Maybe Text
    , dlsAcceptMultiple                    :: Maybe Bool
    , dlsAutoCreate                        :: Maybe Bool
    , dlsTitle                             :: Maybe Text
    , dlsData                              :: Maybe Text
    } deriving (Show, Eq)

$(deriveJSON (defaultOptions {
    fieldLabelModifier = \case
         "dlsReturnUrl" -> "deep_link_return_url"
         "dlsAcceptTypes" -> "accept_types"
         "dlsAcceptPresentationDocumentTargets" -> "accept_presentation_document_targets"
         "dlsAcceptMultiple" -> "accept_multiple"
         "dlsAutoCreate" -> "auto_create"
         "dlsTitle" -> "title"
         "dlsData" -> "data"
         "dlsAcceptMediaTypes" -> "accept_media_types"
         v -> error v
    }) ''DeepLinkingSettings)

data DeepLinkingResponseMessage = DeepLinkingResponseMessage
    { dlrmContent     :: DeepLinkingResponse
    , dlrmBasicClaims :: JwtClaims
    }

instance ToJSON DeepLinkingResponseMessage where
    toJSON DeepLinkingResponseMessage {..} =
        let A.Object basic = toJSON dlrmBasicClaims
            A.Object content = toJSON dlrmContent
        in A.Object $ basic <> content

data MessageType =
      LtiResourceLinkRequest
    | LtiDeepLinkingRequest
    | LtiDeepLinkingResponse
    deriving (Show, Eq, Generic)

instance FromJSON MessageType
instance ToJSON MessageType

-- | Response to a deep linking request. This is the content of a JWT.
data DeepLinkingResponse = DeepLinkingResponse
    { dlRespMessageType  :: MessageType
    -- ^ This is 'LtiDeepLinkingResponse'
    , dlRespLtiVersion   :: Text
    -- ^ @"1.3.0"@
    , dlRespDeploymentId :: Text
    -- ^ Deployment ID of this tool
    , dlRespData         :: Maybe Text
    -- ^ Arbitrary data given previously in the 'DeepLinkingSettings' of the
    -- deep linking request.
    , dlRespContentItems :: [DeepLinkContentItem]
    -- ^ Items that the user selected.
    }

$(deriveJSON
    defaultOptions
    {
        fieldLabelModifier = \case
            "dlRespMessageType"  -> T.unpack claimMessageType
            "dlRespLtiVersion"   -> T.unpack claimVersion
            "dlRespDeploymentId" -> T.unpack claimDeploymentId
            "dlRespData"         -> T.unpack claimDlData
            "dlRespContentItems" -> T.unpack claimDlContentItems
            _                    -> error "missing field"
    }
    ''DeepLinkingResponse
    )

------------------------------------------------------------
-- AGS
------------------------------------------------------------

-- | Permitted scope of access to grades data.
data AgsScope =
      AgsScopeLineItem
    -- ^ Can enumerate and create/manage line items.
    | AgsScopeLineItemReadOnly
    -- ^ Can enumerate line items.
    | AgsScopeScore
    -- ^ Can write scores to line items we own.
    | AgsScopeResult
    -- ^ Can access final results.
    | AgsScopeOther Text
    -- ^ This probably doesn't exist.
    deriving (Eq, Show)

agsScopeToString :: AgsScope -> Text
agsScopeFromString :: Text -> AgsScope
(agsScopeToString, agsScopeFromString) = mkBijection (unOther, AgsScopeOther) [
        (AgsScopeLineItem, "https://purl.imsglobal.org/spec/lti-ags/scope/lineitem"),
        (AgsScopeLineItemReadOnly, "https://purl.imsglobal.org/spec/lti-ags/scope/lineitem.readonly"),
        (AgsScopeScore, "https://purl.imsglobal.org/spec/lti-ags/scope/score"),
        (AgsScopeResult, "https://purl.imsglobal.org/spec/lti-ags/scope/result.readonly")
    ]
    where unOther (AgsScopeOther o) = o
          unOther _                 = error "bug: missing ags scope"

instance FromJSON AgsScope where
    parseJSON = withText "AgsScope" $ return . agsScopeFromString

instance ToJSON AgsScope where
    toJSON = A.String . agsScopeToString

data AgsClaim = AgsClaim
    { agsClaimScope        :: [AgsScope]
    -- ^ Scopes that the tool has authorization to get a token for.
    , agsClaimLineItemsUrl :: Maybe Text
    -- ^ URL for managing line items, using 'AgsScopeLineItem' or accessing
    --   scores with 'AgsScopeScore'.
    , agsClaimLineItemUrl  :: Maybe Text
    -- ^ URL for managing the line item for this resource link.
    }
    deriving (Show, Eq)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = \case
            "agsClaimScope"        -> "scope"
            "agsClaimLineItemUrl"  -> "lineitem"
            "agsClaimLineItemsUrl" -> "lineitems"
            v                      -> error v
        }
    ''AgsClaim
    )

------------------------------------------------------------
-- Base
------------------------------------------------------------

-- | Roles in the target context (≈ course/section); see
--   <http://www.imsglobal.org/spec/lti/v1p3/#lis-vocabulary-for-institution-roles LTI spec § A.2.2>
--   and <http://www.imsglobal.org/spec/lti/v1p3/#roles-claim LTI spec § 5.3.7>
--   for details
data Role = Administrator
          | ContentDeveloper
          | Instructor
          | Learner
          | Mentor
          | Other Text
          deriving (Show, Eq)

roleFromString :: Text -> Role
roleToString :: Role -> Text
(roleToString, roleFromString) = mkBijection (fromOther, Other) [
          (Administrator, "http://purl.imsglobal.org/vocab/lis/v2/membership#Administrator")
        , (ContentDeveloper, "http://purl.imsglobal.org/vocab/lis/v2/membership#ContentDeveloper")
        , (Instructor, "http://purl.imsglobal.org/vocab/lis/v2/membership#Instructor")
        , (Learner, "http://purl.imsglobal.org/vocab/lis/v2/membership#Learner")
        , (Mentor, "http://purl.imsglobal.org/vocab/lis/v2/membership#Mentor")
    ]
    where fromOther (Other v) = v
          fromOther _         = error "bug: missing role in roleFromString"

instance FromJSON Role where
    parseJSON = withText "Role" $ return . roleFromString

instance ToJSON Role where
    toJSON = A.String . roleToString

-- | <http://www.imsglobal.org/spec/lti/v1p3/#lislti LTI spec § D> LIS claim
data LisClaim = LisClaim
    { personSourcedId         :: Maybe Text
    -- ^ LIS identifier for the person making the request.
    , outcomeServiceUrl       :: Maybe Text
    -- ^ URL for the Basic Outcomes service, unique per-tool.
    , courseOfferingSourcedId :: Maybe Text
    -- ^ Identifier for the course
    , courseSectionSourcedId  :: Maybe Text
    -- ^ Identifier for the section.
    , resultSourcedId         :: Maybe Text
    -- ^ An identifier for the position in the gradebook associated with the
    --   assignment being viewed.
    } deriving (Show, Eq)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = \case
            "personSourcedId"         -> "person_sourcedid"
            "outcomeServiceUrl"       -> "outcome_service_url"
            "courseOfferingSourcedId" -> "course_offering_sourcedid"
            "courseSectionSourcedId"  -> "course_section_sourcedid"
            "resultSourcedId"         -> "result_sourcedid"
            v                         -> error v
        }
    ''LisClaim
    )

-- | <http://www.imsglobal.org/spec/lti/v1p3/#context-claim LTI spec § 5.4.1> context claim
data ContextClaim = ContextClaim
    { contextId    :: Text
    , contextLabel :: Maybe Text
    , contextTitle :: Maybe Text
    }
    deriving (Show, Eq)


$(deriveJSON
    defaultOptions
    { fieldLabelModifier = unPrefix "context"
    }
    ''ContextClaim
    )

-- | LTI specific claims on a token. You should not accept this type, and
--   instead prefer the @newtype@ 'LtiTokenClaims' which has had checking
--   performed on it.
data UncheckedLtiTokenClaims = UncheckedLtiTokenClaims
    { messageType   :: MessageType
    , ltiVersion    :: Text
    , deploymentId  :: Text
    , targetLinkUri :: Text
    , roles         :: [Role]
    , email         :: Maybe Text
    , displayName   :: Maybe Text
    , firstName     :: Maybe Text
    , lastName      :: Maybe Text
    , context       :: Maybe ContextClaim
    , lis           :: Maybe LisClaim
    , tokDlSettings :: Maybe DeepLinkingSettings
    , tokAgs        :: Maybe AgsClaim
    } deriving (Show, Eq)

-- | An object representing in the type system a token whose claims have been
--   validated.
newtype LtiTokenClaims = LtiTokenClaims { unLtiTokenClaims :: UncheckedLtiTokenClaims }
    deriving (Show, Eq)

-- | LTI token claims from which all student data has been removed. For logging.
newtype AnonymizedLtiTokenClaims = AnonymizedLtiTokenClaims UncheckedLtiTokenClaims
    deriving (Show, Eq)

limitLength :: (Fail.MonadFail m) => Int -> Text -> m Text
limitLength len string
    | T.length string <= len
    = return string
limitLength _ _ = fail "String is too long"

instance FromJSON UncheckedLtiTokenClaims where
    parseJSON = withObject "LtiTokenClaims" $ \v ->
        UncheckedLtiTokenClaims
            -- FIXME: this validation probably should be elsewhere?
            <$> parseFixedAny v claimMessageType [LtiResourceLinkRequest, LtiDeepLinkingRequest]
            <*> parseFixed v claimVersion "1.3.0"
            <*> (v .: claimDeploymentId >>= limitLength 255)
            <*> v .: claimTargetLinkUri
            <*> v .: claimRoles
            <*> v .:? "email"
            <*> v .:? "name"
            <*> v .:? "given_name"
            <*> v .:? "family_name"
            <*> v .:? claimContext
            <*> v .:? claimLis
            <*> v .:? claimDlSettings
            <*> v .:? claimAgs

instance ToJSON UncheckedLtiTokenClaims where
    toJSON UncheckedLtiTokenClaims {
              messageType, ltiVersion, deploymentId
            , targetLinkUri, roles, email, displayName
            , firstName, lastName, context, lis, tokDlSettings, tokAgs} =
        object [
              claimMessageType .= messageType
            , claimVersion .= ltiVersion
            , claimDeploymentId .= deploymentId
            , claimTargetLinkUri .= targetLinkUri
            , claimRoles .= roles
            , "email" .= email
            , "name" .= displayName
            , "given_name" .= firstName
            , "family_name" .= lastName
            , claimContext .= context
            , claimLis .= lis
            , claimDlSettings .= tokDlSettings
            , claimAgs .= tokAgs
          ]
    toEncoding UncheckedLtiTokenClaims {
              messageType, ltiVersion, deploymentId
            , targetLinkUri, roles, email, displayName
            , firstName, lastName, context, lis, tokDlSettings, tokAgs} =
        pairs (
               claimMessageType .= messageType
            <> claimVersion .= ltiVersion
            <> claimDeploymentId .= deploymentId
            <> claimTargetLinkUri .= targetLinkUri
            <> claimRoles .= roles
            <> "email" .= email
            <> "name" .= displayName
            <> "given_name" .= firstName
            <> "family_name" .= lastName
            <> claimContext .= context
            <> claimLis .= lis
            <> claimDlSettings .= tokDlSettings
            <> claimAgs .= tokAgs
          )
