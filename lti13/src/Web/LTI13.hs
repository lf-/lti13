-- | A simple LTI 1.3 library.
--   It's intended to be used by implementing routes for 'initiate' and
--   'handleAuthResponse', and work out the associated parameters thereof.
--
--   This is written based on the LTI 1.3 specification
--   <http://www.imsglobal.org/spec/lti/v1p3/ available from the IMS Global
--   website>. Users will probably also find the <https://lti-ri.imsglobal.org/
--   LTI Reference Implementation> helpful.
module Web.LTI13 (
      -- * Token contents/data model
        Role(..)
      , LisClaim(..)
      , ContextClaim(..)
      , UncheckedLtiTokenClaims(..)
      , LtiTokenClaims(..)

      -- * Anonymizing tokens for logging
      , AnonymizedLtiTokenClaims(..)
      , anonymizeLtiTokenForLogging

      -- * Validation and auth
      , validateLtiToken
      , LTI13Exception(..)
      , PlatformInfo(..)
      , Issuer
      , ClientId
      , SessionStore(..)
      , AuthFlowConfig(..)
      , RequestParams
      , initiate
      , handleAuthResponse
    ) where
import           Control.Exception.Safe             (Exception, MonadCatch,
                                                     MonadThrow, Typeable,
                                                     catch, throw, throwM)
import           Control.Monad                      (when, (>=>))
import qualified Control.Monad.Fail                 as Fail
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Data.Aeson                         (FromJSON (parseJSON),
                                                     Object,
                                                     ToJSON (toEncoding, toJSON),
                                                     eitherDecode, object,
                                                     pairs, withObject,
                                                     withText, (.:), (.:?),
                                                     (.=))
import qualified Data.Aeson                         as A
import           Data.Aeson.Types                   (Parser)
import qualified Data.Map.Strict                    as Map
import           Data.String                        (IsString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Jose.Jwa                           (JwsAlg (RS256))
import qualified Jose.Jwk                           as Jwk
import           Network.HTTP.Client                (HttpException, Manager,
                                                     httpLbs, parseRequest,
                                                     responseBody)
import qualified Network.HTTP.Types.URI             as URI
import qualified Web.OIDC.Client.Discovery.Provider as P
import           Web.OIDC.Client.IdTokenFlow        (getValidIdTokenClaims)
import qualified Web.OIDC.Client.Settings           as O
import           Web.OIDC.Client.Tokens             (IdTokenClaims, aud, iss,
                                                     nonce, otherClaims)
import           Web.OIDC.Client.Types              (Nonce, SessionStore (..))

-- | Parses a JSON text field to a fixed expected value, failing otherwise
parseFixed :: (FromJSON a, Eq a, Show a) => Object -> A.Key -> a -> Parser a
parseFixed obj field fixedVal =
    obj .: field >>= \v ->
        if v == fixedVal then
            return v
        else
            fail $ "field " ++ show field ++ " was not the required value " ++ show fixedVal

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
roleFromString "http://purl.imsglobal.org/vocab/lis/v2/membership#Administrator"
    = Administrator
roleFromString "http://purl.imsglobal.org/vocab/lis/v2/membership#ContentDeveloper"
    = ContentDeveloper
roleFromString "http://purl.imsglobal.org/vocab/lis/v2/membership#Instructor"
    = Instructor
roleFromString "http://purl.imsglobal.org/vocab/lis/v2/membership#Learner"
    = Learner
roleFromString "http://purl.imsglobal.org/vocab/lis/v2/membership#Mentor"
    = Mentor
roleFromString s = Other s

roleToString :: Role -> Text
roleToString Administrator = "http://purl.imsglobal.org/vocab/lis/v2/membership#Administrator"
roleToString ContentDeveloper = "http://purl.imsglobal.org/vocab/lis/v2/membership#ContentDeveloper"
roleToString Instructor = "http://purl.imsglobal.org/vocab/lis/v2/membership#Instructor"
roleToString Learner = "http://purl.imsglobal.org/vocab/lis/v2/membership#Learner"
roleToString Mentor = "http://purl.imsglobal.org/vocab/lis/v2/membership#Mentor"
roleToString (Other s) = s

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

instance FromJSON LisClaim where
    parseJSON = withObject "LisClaim" $ \v ->
        LisClaim
            <$> v .:? "person_sourcedid"
            <*> v .:? "outcome_service_url"
            <*> v .:? "course_offering_sourcedid"
            <*> v .:? "course_section_sourcedid"
            <*> v .:? "result_sourcedid"

instance ToJSON LisClaim where
    toJSON LisClaim {personSourcedId, outcomeServiceUrl,
                courseOfferingSourcedId, courseSectionSourcedId,
                resultSourcedId} =
        object [
            "person_sourcedid" .= personSourcedId
          , "outcome_service_url" .= outcomeServiceUrl
          , "course_offering_sourcedid" .= courseOfferingSourcedId
          , "course_section_sourcedid" .= courseSectionSourcedId
          , "result_sourcedid" .= resultSourcedId
          ]
    toEncoding LisClaim {personSourcedId, outcomeServiceUrl,
                    courseOfferingSourcedId, courseSectionSourcedId,
                    resultSourcedId} =
        pairs (
            "person_sourcedid" .= personSourcedId <>
            "outcome_service_url" .= outcomeServiceUrl <>
            "course_offering_sourcedid" .= courseOfferingSourcedId <>
            "course_section_sourcedid" .= courseSectionSourcedId <>
            "result_sourcedid" .= resultSourcedId
        )

-- | <http://www.imsglobal.org/spec/lti/v1p3/#context-claim LTI spec § 5.4.1> context claim
data ContextClaim = ContextClaim
    { contextId    :: Text
    , contextLabel :: Maybe Text
    , contextTitle :: Maybe Text
    }
    deriving (Show, Eq)

instance FromJSON ContextClaim where
    parseJSON = withObject "ContextClaim" $ \v ->
        ContextClaim
            <$> (v .: "id" >>= limitLength 255)
            <*> v .:? "label"
            <*> v .:? "title"

instance ToJSON ContextClaim where
    toJSON ContextClaim {contextId, contextLabel, contextTitle} =
        object [
            "id" .= contextId
          , "label" .= contextLabel
          , "title" .= contextTitle
          ]
    toEncoding ContextClaim {contextId, contextLabel, contextTitle} =
        pairs (
            "id" .= contextId <>
            "label" .= contextLabel <>
            "title" .= contextTitle
        )

-- | LTI specific claims on a token. You should not accept this type, and
--   instead prefer the @newtype@ 'LtiTokenClaims' which has had checking
--   performed on it.
data UncheckedLtiTokenClaims = UncheckedLtiTokenClaims
    { messageType   :: Text
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

claimMessageType :: IsString t => t
claimMessageType = "https://purl.imsglobal.org/spec/lti/claim/message_type"
claimVersion :: IsString t => t
claimVersion = "https://purl.imsglobal.org/spec/lti/claim/version"
claimDeploymentId :: IsString t => t
claimDeploymentId = "https://purl.imsglobal.org/spec/lti/claim/deployment_id"
claimTargetLinkUri :: IsString t => t
claimTargetLinkUri = "https://purl.imsglobal.org/spec/lti/claim/target_link_uri"
claimRoles :: IsString t => t
claimRoles = "https://purl.imsglobal.org/spec/lti/claim/roles"
claimContext :: IsString t => t
claimContext = "https://purl.imsglobal.org/spec/lti/claim/context"
claimLis :: IsString t => t
claimLis = "https://purl.imsglobal.org/spec/lti/claim/lis"

instance FromJSON UncheckedLtiTokenClaims where
    parseJSON = withObject "LtiTokenClaims" $ \v ->
        UncheckedLtiTokenClaims
            <$> parseFixed v claimMessageType "LtiResourceLinkRequest"
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

instance ToJSON UncheckedLtiTokenClaims where
    toJSON UncheckedLtiTokenClaims {
              messageType, ltiVersion, deploymentId
            , targetLinkUri, roles, email, displayName
            , firstName, lastName, context, lis} =
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
          ]
    toEncoding UncheckedLtiTokenClaims {
              messageType, ltiVersion, deploymentId
            , targetLinkUri, roles, email, displayName
            , firstName, lastName, context, lis} =
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
          )

-- | A direct implementation of <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation Security § 5.1.3>
validateLtiToken
    :: PlatformInfo
    -> IdTokenClaims UncheckedLtiTokenClaims
    -> Either Text (IdTokenClaims LtiTokenClaims)
validateLtiToken pinfo claims =
    valid .
        (issuerMatches
         >=> audContainsClientId
         >=> hasNonce) $ claims
    where
        -- step 1 handled before we are called
        -- step 2
        issuerMatches c
            | iss c == platformIssuer pinfo
                = Right claims
            | otherwise
                = Left "issuer does not match platform issuer"
        -- step 3
        audContainsClientId c
            -- "The Tool MUST reject the ID Token if it does not list the
            -- client_id as a valid audience, or if it contains additional
            -- audiences not trusted by the Tool."
            -- Game on, I don't trust anyone else.
            | length  (aud c) == 1 && platformClientId pinfo `elem` aud c
                = Right claims
            | otherwise
                = Left "aud is invalid"
        -- step 4 and 5 elided -> we can ignore azp because we don't accept >1 aud entries
        -- step 6 is performed elsewhere, probably
        -- step 7 elided because it is handled by 'validateClaims'
        -- step 8 optional
        -- step 9 nonce checking "The ID Token MUST contain a nonce Claim."
        hasNonce c =
            case nonce c of
                Just _  -> Right claims
                Nothing -> Left "nonce missing"
        valid :: Either Text (IdTokenClaims UncheckedLtiTokenClaims) -> Either Text (IdTokenClaims LtiTokenClaims)
        -- unwrap a validated token and rewrap it as a valid token
        valid (Left e) = Left e
        valid (Right tok) =
            Right tok { otherClaims = LtiTokenClaims $ otherClaims tok }


-----------------------------------------------------------
-- Helpers for the endpoints you have to implement
-----------------------------------------------------------

-- | (most of) the exceptions that can arise in LTI 1.3 handling. Some may have
--   been forgotten, and this is a bug that should be fixed.
data LTI13Exception
    = InvalidHandshake Text
    -- ^ Error in the handshake format
    | DiscoveryException Text
    | GotHttpException HttpException
    | InvalidLtiToken Text
    -- ^ Token validation error. Per <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation Security § 5.1.3>
    --   if you get this, you should return a 401.
    deriving (Show, Typeable)
instance Exception LTI13Exception

-- | @client_id@, one or more per platform; <https://www.imsglobal.org/spec/lti/v1p3/#tool-deployment LTI spec § 3.1.3>
type ClientId = Text

-- | Preregistered information about a learning platform
data PlatformInfo = PlatformInfo
    {
    -- | Issuer value
      platformIssuer           :: Issuer
    -- | @client_id@
    , platformClientId         :: ClientId
    -- | URL the client is redirected to for <http://www.imsglobal.org/spec/security/v1p0/#step-3-authentication-response auth stage 2>.
    --   See also <http://www.imsglobal.org/spec/security/v1p0/#openid_connect_launch_flow Security spec § 5.1.1>
    , platformOidcAuthEndpoint :: Text
    -- | URL for a JSON object containing the JWK signing keys for the platform
    , jwksUrl                  :: String
    }

-- | Issuer/@iss@ field
type Issuer = Text

-- | Structure you have to provide defining integration points with your app
data AuthFlowConfig m = AuthFlowConfig
    { getPlatformInfo :: (Issuer, Maybe ClientId) -> m PlatformInfo
    -- ^ Access some persistent storage of the configured platforms and return the
    --   PlatformInfo for a given platform by name
    , haveSeenNonce   :: Nonce -> m Bool
    , myRedirectUri   :: Text
    , sessionStore    :: SessionStore m
    -- ^ Note that as in the example for haskell-oidc-client, this is intended to
    --   be partially parameterized already with some separate cookie you give
    --   the browser. You should also store the @iss@ from the 'initiate' stage
    --   in the session somewhere for the 'handleAuthResponse' stage.
    }

rethrow :: (MonadCatch m) => HttpException -> m a
rethrow = throwM . GotHttpException

-- | Grab the JWK set from a URL
getJwkSet
    :: Manager
    -> String
    -> IO [Jwk.Jwk]
getJwkSet manager fromUrl = do
    json <- getJwkSetJson fromUrl `catch` rethrow
    case jwks json of
        Right keys -> return keys
        Left  err  -> throwM $ DiscoveryException ("Failed to decode JwkSet: " <> T.pack err)
  where
    getJwkSetJson url = do
        req <- parseRequest url
        res <- httpLbs req manager
        return $ responseBody res

    jwks j = Jwk.keys <$> eitherDecode j

lookupOrThrow :: (MonadThrow m) => Text -> Map.Map Text Text -> m Text
lookupOrThrow name map_ =
    case Map.lookup name map_ of
        Nothing -> throw $ InvalidHandshake $ "Missing `" <> name <> "`"
        Just a  -> return a

-- | Parameters to a request, either in the URL with a @GET@ or in the body
--   with a @POST@
type RequestParams = Map.Map Text Text

-- | Makes the URL for <http://www.imsglobal.org/spec/security/v1p0/#step-1-third-party-initiated-login IMS Security spec § 5.1.1.2>
--   upon the § 5.1.1.1 request coming in
--
--   Returns @(Issuer, RedirectURL)@.
initiate :: (MonadIO m) => AuthFlowConfig m -> RequestParams -> m (Issuer, ClientId, Text)
initiate cfg params = do
    -- we don't care about target link uri since we only support one endpoint
    iss <- liftIO $ lookupOrThrow "iss" params
    loginHint <- liftIO $ lookupOrThrow "login_hint" params
    _targetLinkUri <- liftIO $ lookupOrThrow "target_link_uri" params

    let messageHint = Map.lookup "lti_message_hint" params
    -- "This allows for a platform to support multiple registrations from a
    -- single issuer, without relying on the initiate_login_uri as a key."
    --
    -- Canvas puts the same issuer on all their messages (wat)
    -- (https://community.canvaslms.com/thread/36682-lti13-how-to-identify-clientid-and-deploymentid-on-launch)
    -- so we need to be able to distinguish these. Our client code must
    -- therefore key its platform info store by @(Issuer, Maybe ClientId)@
    let gotCid = Map.lookup "client_id" params
    PlatformInfo
        { platformOidcAuthEndpoint = endpoint
        , platformClientId = clientId } <- getPlatformInfo cfg (iss, gotCid)

    let ss = sessionStore cfg
    nonce <- sessionStoreGenerate ss
    state <- sessionStoreGenerate ss
    sessionStoreSave ss state nonce

    let query = URI.simpleQueryToQuery $
                [ ("scope", "openid")
                , ("response_type", "id_token")
                , ("client_id", encodeUtf8 clientId)
                , ("redirect_uri", encodeUtf8 $ myRedirectUri cfg)
                , ("login_hint", encodeUtf8 loginHint)
                , ("state", state)
                , ("response_mode", "form_post")
                , ("nonce", nonce)
                , ("prompt", "none")
                ] ++ maybe [] (\mh -> [("lti_message_hint", encodeUtf8 mh)]) messageHint
    return (iss, clientId, endpoint <> (decodeUtf8 . URI.renderQuery True) query)

-- | Makes a fake OIDC object with the bare minimum attributes to hand to
--   verification library functions
fakeOidc :: [Jwk.Jwk] -> O.OIDC
fakeOidc jset = O.OIDC
        { O.oidcProvider = P.Provider
                { P.configuration = P.Configuration
                    { P.idTokenSigningAlgValuesSupported = [ P.JwsAlgJson RS256 ]
                    , P.issuer = undefined
                    , P.authorizationEndpoint = undefined
                    , P.tokenEndpoint = undefined
                    , P.userinfoEndpoint = undefined
                    , P.revocationEndpoint = undefined
                    , P.jwksUri = undefined
                    , P.responseTypesSupported = undefined
                    , P.subjectTypesSupported = undefined
                    , P.scopesSupported = undefined
                    , P.tokenEndpointAuthMethodsSupported = undefined
                    , P.claimsSupported = undefined
                    }
                , P.jwkSet = jset
                }
        , O.oidcAuthorizationServerUrl = undefined
        , O.oidcTokenEndpoint = undefined
        , O.oidcClientId = undefined
        , O.oidcClientSecret = undefined
        , O.oidcRedirectUri = undefined
        }

-- | Handle the <http://www.imsglobal.org/spec/security/v1p0/#step-3-authentication-response § 5.1.1.3 Step 3>
--   response sent to the 'AuthFlowConfig.myRedirectUri'
--
--   Returns @(State, Token)@
handleAuthResponse :: (MonadIO m)
    => Manager
    -> AuthFlowConfig m
    -> RequestParams
    -> PlatformInfo
    -> m (Text, IdTokenClaims LtiTokenClaims)
handleAuthResponse mgr cfg params pinfo = do
    state <- liftIO $ lookupOrThrow "state" params
    idToken <- liftIO $ lookupOrThrow "id_token" params

    let PlatformInfo { jwksUrl } = pinfo
    jwkSet <- liftIO $ getJwkSet mgr jwksUrl

    let ss = sessionStore cfg
        oidc = fakeOidc jwkSet
    toCheck <- getValidIdTokenClaims ss oidc (encodeUtf8 state) (pure $ encodeUtf8 idToken)

    -- present nonce but seen -> error
    -- present nonce unseen -> good
    -- absent nonce -> different error
    nonceSeen <- case nonce toCheck of
        Just n  -> haveSeenNonce cfg n
        Nothing -> liftIO $ throw $ InvalidLtiToken "missing nonce"
    when nonceSeen (liftIO $ throw $ InvalidLtiToken "nonce seen before")

    case validateLtiToken pinfo toCheck of
        Left err  -> liftIO $ throw $ InvalidLtiToken err
        Right tok -> return (state, tok)

-- | Removes PII of the user from the token, retaining only information about
--   the system in general or the context.
--
--   Fields that are 'Maybe' are kept as 'Maybe', with the contents replaced
--   with @"**"@ if they were 'Just' and otherwise kept as 'Nothing'.
anonymizeLtiTokenForLogging :: UncheckedLtiTokenClaims -> AnonymizedLtiTokenClaims
anonymizeLtiTokenForLogging UncheckedLtiTokenClaims {..} =
    AnonymizedLtiTokenClaims $ UncheckedLtiTokenClaims
        { messageType
        , ltiVersion
        , deploymentId
        -- this should not identify the user; it is at most a class item
        , targetLinkUri
        , roles
        , displayName = anonymized <$> displayName
        , firstName = anonymized <$> firstName
        , lastName = anonymized <$> lastName
        , context
        , email = anonymized <$> email
        , lis = anonymizedLis <$> lis
        }
    where
        anonymized _ = "**"
        anonymizedLis LisClaim {..} = LisClaim
            -- we really don't know what they will put in this; it might be
            -- student specific
            { personSourcedId = anonymized <$> personSourcedId
            -- spec strongly suggests this be the same across launches ie only
            -- identifies the context
            , outcomeServiceUrl
            , courseOfferingSourcedId
            , courseSectionSourcedId
            -- likewise with personSourcedId, we don't know what will be put in
            -- here. it's probably a guid but let's be safe
            , resultSourcedId = anonymized <$> resultSourcedId
            }
