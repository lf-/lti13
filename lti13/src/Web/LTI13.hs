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
      , Lti13Exception(..)
      , PlatformInfo(..)
      , Issuer
      , GetPlatformInfo
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
import Control.Exception.Safe (MonadCatch, catch, throwM, Typeable, Exception, MonadThrow, throw)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Fail (MonadFail)
import Data.Aeson (eitherDecode, FromJSON (parseJSON), Object, withObject, withText, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Network.HTTP.Types.URI as URI
import Network.HTTP.Client (defaultManagerSettings, newManager, responseBody, Manager, HttpException, parseRequest, httpLbs)
import Data.ByteString (ByteString)
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

instance FromJSON Role where
    parseJSON = withText "Role" $ return . roleFromString

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

limitLength :: (Monad m) => Int -> Text -> m Text
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

-- | A direct implementation of <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation Security § 5.1.3>
validateLtiToken :: PlatformInfo -> IdTokenClaims UncheckedLtiTokenClaims -> Either Text (IdTokenClaims LtiTokenClaims)
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
data Lti13Exception
    = InvalidHandshake Text
    -- ^ Error in the handshake format
    | DiscoveryException Text
    | GotHttpException HttpException
    | InvalidLtiToken Text
    -- ^ Token validation error. Per <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation Security § 5.1.3>
    --   if you get this, you should return a 401.
    deriving (Show, Typeable)
instance Exception Lti13Exception

-- | Preregistered information about a learning platform
data PlatformInfo = PlatformInfo
    {
    -- |Issuer value
      platformIssuer :: Issuer
    -- | @deployment_id@ <https://www.imsglobal.org/spec/lti/v1p3/#tool-deployment LTI spec § 3.1.3>
    , platformDeploymentId :: ByteString
    -- | @client_id@, one or more per platform; <https://www.imsglobal.org/spec/lti/v1p3/#tool-deployment LTI spec § 3.1.3>
    , platformClientId :: Text
    -- | URL the client is redirected to for <http://www.imsglobal.org/spec/security/v1p0/#step-3-authentication-response auth stage 2>.
    --   See also <http://www.imsglobal.org/spec/security/v1p0/#openid_connect_launch_flow Security spec § 5.1.1>
    , platformOidcAuthEndpoint :: ByteString
    -- | URL for a JSON object containing the JWK signing keys for the platform
    , jwksUrl :: String
    }

-- | Issuer/@iss@ field
type Issuer = Text

-- | Access some persistent storage of the configured platforms and return the
--   PlatformInfo for a given platform by name
type GetPlatformInfo = Issuer -> IO PlatformInfo

-- | Object you have to provide defining integration points with your app
data AuthFlowConfig m = AuthFlowConfig
    { getPlatformInfo :: GetPlatformInfo
    , haveSeenNonce   :: Nonce -> m Bool
    , myRedirectUri   :: ByteString
    , sessionStore    :: SessionStore m
    -- ^ Note that as in the example for haskell-oidc-client, this is intended to
    --   be partially parameterized already with some separate cookie you give
    --   the browser. You should also store the `iss` in your actual implementation.
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

lookupOrThrow :: (MonadThrow m) => Text -> Map.Map Text ByteString -> m ByteString
lookupOrThrow name map_ =
    case Map.lookup name map_ of
        Nothing -> throw $ InvalidHandshake $ "Missing `" <> name <> "`"
        Just a -> return a

-- | Parameters to a request, either in the URL with a @GET@ or in the body
--   with a @POST@
type RequestParams = Map.Map Text ByteString

-- | Makes the URL for <http://www.imsglobal.org/spec/security/v1p0/#step-1-third-party-initiated-login IMS Security spec § 5.1.1.2>
--   upon the § 5.1.1.1 request coming in
initiate :: (MonadIO m, MonadThrow m, MonadFail m) => AuthFlowConfig IO -> RequestParams -> m ByteString
initiate cfg params = do
    -- we don't care about target link uri since we only support one endpoint
    [iss', loginHint, _] <- mapM (flip lookupOrThrow params) ["iss", "login_hint", "target_link_uri"]
    let iss = decodeUtf8 iss'
        messageHint = Map.lookup "lti_mesage_hint" params
    PlatformInfo
        { platformOidcAuthEndpoint = endpoint
        , platformClientId = clientId
        } <- liftIO $ (getPlatformInfo cfg) iss

    let ss = sessionStore cfg
    nonce <- liftIO $ sessionStoreGenerate ss
    state <- liftIO $ sessionStoreGenerate ss
    liftIO $ sessionStoreSave ss state nonce

    let query = URI.simpleQueryToQuery $
                [ ("scope", "openid")
                , ("response_type", "id_token")
                , ("client_id", encodeUtf8 clientId)
                , ("redirect_uri", myRedirectUri cfg)
                , ("login_hint", loginHint)
                , ("state", state)
                , ("response_mode", "form_post")
                , ("nonce", nonce)
                , ("prompt", "none")
                ] ++ maybe [] (\mh -> [("lti_message_hint", mh)]) messageHint
    return $ endpoint <> URI.renderQuery True query

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
handleAuthResponse :: (MonadIO m, MonadThrow m, MonadFail m) => AuthFlowConfig IO -> RequestParams -> Issuer -> m (IdTokenClaims LtiTokenClaims)
handleAuthResponse cfg params iss = do
    [state, idToken] <- mapM (flip lookupOrThrow params) ["state", "id_token"]
    pinfo@PlatformInfo { jwksUrl } <- liftIO $ getPlatformInfo cfg iss
    -- TODO: this should really probably be cached and take the user's manager
    mgr <- liftIO $ newManager defaultManagerSettings
    jwkSet <- liftIO $ getJwkSet mgr jwksUrl

    let ss = sessionStore cfg
        oidc = fakeOidc jwkSet
    toCheck <- liftIO $ getValidIdTokenClaims ss oidc state (pure idToken)

    -- present nonce but seen -> error
    -- present nonce unseen -> good
    -- absent nonce -> different error
    nonceSeen <- case nonce toCheck of
        Just n  -> liftIO $ haveSeenNonce cfg n
        Nothing -> throw $ InvalidLtiToken "missing nonce"
    when nonceSeen (throw $ InvalidLtiToken "nonce seen before")

    case validateLtiToken pinfo toCheck of
        Left err  -> throw $ InvalidLtiToken err
        Right tok -> return tok
