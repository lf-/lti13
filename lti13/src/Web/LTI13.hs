{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A basic LTI 1.3 library.
--   It's intended to be used by implementing routes for 'initiate' and
--   'handleAuthResponse', and work out the associated parameters thereof.
module Web.LTI13 (
        Role(..)
      , ContextClaim(..)
      , UncheckedLtiTokenClaims(..)
      , LtiTokenClaims(..)
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
import qualified Web.OIDC.Client.Settings as O
import qualified Web.OIDC.Client.Discovery.Provider as P
import Web.OIDC.Client.Tokens (nonce, aud, otherClaims, iss, IdTokenClaims)
import Web.OIDC.Client.IdTokenFlow (getValidIdTokenClaims)
import Web.OIDC.Client.Types (Nonce, SessionStore(..))
import Jose.Jwa (JwsAlg(RS256))
import qualified Jose.Jwk as Jwk
import Control.Monad (when, (>=>))
import qualified Control.Monad.Fail as Fail
import Control.Exception.Safe (MonadCatch, catch, throwM, Typeable, Exception, MonadThrow, throw)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (eitherDecode, FromJSON (parseJSON), ToJSON(toJSON, toEncoding), Object,
                   object, pairs, withObject, withText, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Network.HTTP.Types.URI as URI
import Network.HTTP.Client (responseBody, Manager, HttpException, parseRequest, httpLbs)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- | Parses a JSON text field to a fixed expected value, failing otherwise
parseFixed :: (FromJSON a, Eq a, Show a) => Object -> Text -> a -> Parser a
parseFixed obj field fixedVal =
    obj .: field >>= \v ->
        if v == fixedVal then
            return v
        else
            fail $ "field " ++ (show field) ++ " was not the required value " ++ (show fixedVal)

-- | Roles in the target context (≈ course/section); see
--   <http://www.imsglobal.org/spec/lti/v1p3/#lis-vocabulary-for-institution-roles LTI spec § A.2.2>
--   and <http://www.imsglobal.org/spec/lti/v1p3/#roles-claim LTI spec § 5.3.7>
--   for details
data Role = Administrator
          | ContentDeveloper
          | Instructor
          | Learner
          | Mentor
          | Other (Text)
          deriving (Show)

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

-- | <http://www.imsglobal.org/spec/lti/v1p3/#context-claim LTI spec § 5.4.1> context claim
data ContextClaim = ContextClaim
    { contextId :: Text
    , contextLabel :: Maybe Text
    , contextTitle :: Maybe Text
    }
    deriving (Show)

instance FromJSON ContextClaim where
    parseJSON = withObject "ContextClaim" $ \v ->
        ContextClaim
            <$> (v .: "id" >>= limitLength 255)
            <*> v .:? "label"
            <*> v .:? "title"

instance ToJSON ContextClaim where
    toJSON (ContextClaim {contextId, contextLabel, contextTitle}) =
        object [
            "id" .= contextId
          , "label" .= contextLabel
          , "title" .= contextTitle
          ]
    toEncoding (ContextClaim {contextId, contextLabel, contextTitle}) =
        pairs (
            "id" .= contextId <>
            "label" .= contextLabel <>
            "title" .= contextTitle
        )

-- | LTI specific claims on a token. You should not accept this type, and
--   instead prefer the @newtype@ 'LtiTokenClaims' which has had checking
--   performed on it.
data UncheckedLtiTokenClaims = UncheckedLtiTokenClaims
    { messageType :: Text
    , ltiVersion :: Text
    , deploymentId :: Text
    , targetLinkUri :: Text
    , roles :: [Role]
    , email :: Maybe Text
    , context :: Maybe ContextClaim
    } deriving (Show)

-- | An object representing in the type system a token whose claims have been
--   validated.
newtype LtiTokenClaims = LtiTokenClaims UncheckedLtiTokenClaims
    deriving (Show)

limitLength :: (Fail.MonadFail m) => Int -> Text -> m Text
limitLength len string
    | (T.length string) <= len
    = return string
limitLength _ _ = fail "String is too long"

claimMessageType :: Text
claimMessageType = "https://purl.imsglobal.org/spec/lti/claim/message_type"
claimVersion :: Text
claimVersion = "https://purl.imsglobal.org/spec/lti/claim/version"
claimDeploymentId :: Text
claimDeploymentId = "https://purl.imsglobal.org/spec/lti/claim/deployment_id"
claimTargetLinkUri :: Text
claimTargetLinkUri = "https://purl.imsglobal.org/spec/lti/claim/target_link_uri"
claimRoles :: Text
claimRoles = "https://purl.imsglobal.org/spec/lti/claim/roles"
claimContext :: Text
claimContext = "https://purl.imsglobal.org/spec/lti/claim/context"

instance FromJSON UncheckedLtiTokenClaims where
    parseJSON = withObject "LtiTokenClaims" $ \v ->
        UncheckedLtiTokenClaims
            <$> (parseFixed v claimMessageType "LtiResourceLinkRequest")
            <*> (parseFixed v claimVersion "1.3.0")
            <*> (v .: claimDeploymentId >>= limitLength 255)
            <*> v .: claimTargetLinkUri
            <*> v .: claimRoles
            <*> v .:? "email"
            <*> v .:? claimContext

instance ToJSON UncheckedLtiTokenClaims where
    toJSON (UncheckedLtiTokenClaims {
              messageType, ltiVersion, deploymentId
            , targetLinkUri, roles, email, context}) =
        object [
              claimMessageType .= messageType
            , claimVersion .= ltiVersion
            , claimDeploymentId .= deploymentId
            , claimTargetLinkUri .= targetLinkUri
            , claimRoles .= roles
            , "email" .= email
            , claimContext .= context
          ]
    toEncoding (UncheckedLtiTokenClaims {
              messageType, ltiVersion, deploymentId
            , targetLinkUri, roles, email, context}) =
        pairs (
               claimMessageType .= messageType
            <> claimVersion .= ltiVersion
            <> claimDeploymentId .= deploymentId
            <> claimTargetLinkUri .= targetLinkUri
            <> claimRoles .= roles
            <> "email" .= email
            <> claimContext .= context
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
            | (length $ aud c) == 1 && (platformClientId pinfo) `elem` (aud c)
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
            Right tok { otherClaims = (LtiTokenClaims $ otherClaims tok) }


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
      platformIssuer :: Issuer
    -- | @client_id@
    , platformClientId :: ClientId
    -- | URL the client is redirected to for <http://www.imsglobal.org/spec/security/v1p0/#step-3-authentication-response auth stage 2>.
    --   See also <http://www.imsglobal.org/spec/security/v1p0/#openid_connect_launch_flow Security spec § 5.1.1>
    , platformOidcAuthEndpoint :: Text
    -- | URL for a JSON object containing the JWK signing keys for the platform
    , jwksUrl :: String
    }

-- | Issuer/@iss@ field
type Issuer = Text

-- | Object you have to provide defining integration points with your app
data AuthFlowConfig m = AuthFlowConfig
    { getPlatformInfo       :: (Issuer, Maybe ClientId) -> m PlatformInfo
    -- ^ Access some persistent storage of the configured platforms and return the
    --   PlatformInfo for a given platform by name
    , haveSeenNonce         :: Nonce -> m Bool
    , myRedirectUri         :: Text
    , sessionStore          :: SessionStore m
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
        Just a -> return a

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
    res <- liftIO $ mapM (flip lookupOrThrow params) ["iss", "login_hint", "target_link_uri"]
    -- not actually fallible
    let [iss, loginHint, _] = res
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
        , platformClientId = clientId } <- (getPlatformInfo cfg) (iss, gotCid)

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
    return $ (iss, clientId, endpoint <> (decodeUtf8 . URI.renderQuery True) query)

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
    params' <- liftIO $ mapM (flip lookupOrThrow params) ["state", "id_token"]
    let [state, idToken] = params'

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
