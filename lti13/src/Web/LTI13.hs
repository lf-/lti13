{-# OPTIONS_GHC -Wno-unused-imports #-}
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
      module Web.LTI13.Types

      -- * Anonymizing tokens for logging
      , module Web.LTI13.Anonymize

      -- * Validation and auth
      , validatePlatformMessage
      , LTI13Exception(..)
      , PlatformInfo(..)
      , Issuer
      , ClientId
      , SessionStore(..)
      , AuthFlowConfig(..)
      , RequestParams
      , initiate
      , handleAuthResponse

      -- ** Authenticating to the platform
      , getAuthToken

      -- ** Deep Linking
      , makeDeepLinkingResponse

      -- ** Assignment and Grade Service
      , submitScore

      -- * Internal
      , makeJwt
    ) where
import           Control.Error
import           Control.Monad                      (when, (>=>))
import           Control.Monad.Catch                (MonadCatch (catch))
import qualified Control.Monad.Fail                 as Fail
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Crypto.Random                      (MonadRandom (getRandomBytes))
import           Crypto.Random.Types                (MonadRandom)
import           Data.Aeson                         (FromJSON (parseJSON),
                                                     Object,
                                                     ToJSON (toEncoding, toJSON),
                                                     eitherDecode, object,
                                                     pairs, withObject,
                                                     withText, (.:), (.:?),
                                                     (.=))
import qualified Data.Aeson                         as A
import           Data.Aeson.Types                   (Parser)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base64             as B64
import           Data.ByteString.Lazy               (toStrict)
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Time                          (ZonedTime,
                                                     nominalDiffTimeToSeconds,
                                                     secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX              (getPOSIXTime)
import           Data.Tuple                         (swap)
import           GHC.Generics                       (Generic)
import           Jose.Jwa                           (JwsAlg (RS256))
import qualified Jose.Jwk                           as Jwk
import qualified Jose.Jwt                           as Jwt
import           Network.HTTP.Client                (HttpException, Manager,
                                                     Request (..),
                                                     RequestBody (..), httpLbs,
                                                     parseRequest, responseBody,
                                                     throwErrorStatusCodes, urlEncodedBody, setRequestCheckStatus)
import qualified Network.HTTP.Types.URI             as URI
import           Web.LTI13.Anonymize
import           Web.LTI13.Types
import qualified Web.OIDC.Client.Discovery.Provider as P
import           Web.OIDC.Client.IdTokenFlow        (getValidIdTokenClaims)
import qualified Web.OIDC.Client.Settings           as O
import           Web.OIDC.Client.Tokens             (IdTokenClaims, aud, iss,
                                                     nonce, otherClaims)
import           Web.OIDC.Client.Types              (Nonce, SessionStore (..))
import Data.Functor (void)


-- | A direct implementation of <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation Security § 5.1.3>
validatePlatformMessage
    :: PlatformInfo
    -> IdTokenClaims UncheckedPlatformMessage
    -> Either Text (IdTokenClaims PlatformMessage)
validatePlatformMessage pinfo claims =
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
            | length (aud c) == 1 && platformClientId pinfo `elem` aud c
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
        valid
            :: Either Text (IdTokenClaims UncheckedPlatformMessage)
            -> Either Text (IdTokenClaims PlatformMessage)
        -- unwrap a validated token and rewrap it as a valid token
        valid (Left e) = Left e
        valid (Right tok) =
            Right tok { otherClaims = PlatformMessage $ otherClaims tok }


-----------------------------------------------------------
-- Helpers for the endpoints you have to implement
-----------------------------------------------------------

type Result a = Either LTI13Exception a

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
    | NoPlatformInfo (Issuer, Maybe ClientId)
    | NoTokenEndpoint
    | JsonDecodeError Text
    | GotJwtError Jwt.JwtError
    deriving (Show)

-- | @client_id@, one or more per platform; <https://www.imsglobal.org/spec/lti/v1p3/#tool-deployment LTI spec § 3.1.3>
type ClientId = Text

-- | Preregistered information about a learning platform
data PlatformInfo = PlatformInfo
    {
    -- | Issuer value
      platformIssuer               :: Issuer
    -- | @client_id@
    , platformClientId             :: ClientId
    -- | URL the client is redirected to for <http://www.imsglobal.org/spec/security/v1p0/#step-3-authentication-response auth stage 2>.
    --   See also <http://www.imsglobal.org/spec/security/v1p0/#openid_connect_launch_flow Security spec § 5.1.1>
    , platformOidcAuthEndpoint     :: Text
    -- | URL to request tokens from. If not present, requesting tokens will
    --   fail.
    , platformTokenEndpoint        :: Maybe Text
    -- | The audience claim value for requesting authentication tokens is
    --   separate from the 'platformIssuer' or anything else like that, for
    --   some reason, so you need to collect this as well. Have fun.
    --
    --   For Canvas, this is @https://<canvas_domain>/login/oauth2/token@.
    , platformTokenRequestAudience :: [Text]
    -- | URL for a JSON object containing the JWK signing keys for the platform
    , jwksUrl                      :: String
    }

-- | Issuer/@iss@ field
type Issuer = Text

-- | Structure you have to provide defining integration points with your app
data AuthFlowConfig m = AuthFlowConfig
    { getPlatformInfo :: (Issuer, Maybe ClientId) -> m (Maybe PlatformInfo)
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

-- | Grab the JWK set from a URL
getJwkSet
    :: Manager
    -> String
    -> IO (Result [Jwk.Jwk])
getJwkSet manager fromUrl = runExceptT $ do
    json <- handleExceptT GotHttpException $ getJwkSetJson fromUrl
    case jwks json of
        Right keys -> return keys
        Left  er   -> throwE
            $ DiscoveryException ("Failed to decode JwkSet: " <> T.pack er)
  where
    getJwkSetJson url = do
        req <- parseRequest url
        let req' = req
                { checkResponse = throwErrorStatusCodes
                }
        res <- httpLbs req' manager
        return $ responseBody res

    jwks j = Jwk.keys <$> eitherDecode j

lookupOrThrow :: Text -> Map.Map Text Text -> Result Text
lookupOrThrow name map_ =
    case Map.lookup name map_ of
        Nothing -> Left $ InvalidHandshake $ "Missing `" <> name <> "`"
        Just a  -> return a

-- | Parameters to a request, either in the URL with a @GET@ or in the body
--   with a @POST@
type RequestParams = Map.Map Text Text

-- | Makes the URL for <http://www.imsglobal.org/spec/security/v1p0/#step-1-third-party-initiated-login IMS Security spec § 5.1.1.2>
--   upon the § 5.1.1.1 request coming in
--
--   Returns @(Issuer, ClientId, RedirectURL)@.
initiate :: (MonadIO m) => AuthFlowConfig m -> RequestParams -> m (Result (Issuer, ClientId, Text))
initiate cfg params = runExceptT $ do
    -- we don't care about target link uri since we only support one endpoint
    res <- except $ mapM (`lookupOrThrow` params) ["iss", "login_hint", "target_link_uri"]
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
        , platformClientId = clientId }
            <- getPlatformInfo cfg (iss, gotCid)
                !? NoPlatformInfo (iss, gotCid)

    let ss = sessionStore cfg
    nonce <- lift $ sessionStoreGenerate ss
    state <- lift $ sessionStoreGenerate ss
    lift $ sessionStoreSave ss state nonce

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
    -> m (Result (Text, IdTokenClaims PlatformMessage))
handleAuthResponse mgr cfg params pinfo = runExceptT $ do
    params' <- except $ mapM (`lookupOrThrow` params) ["state", "id_token"]
    let [state, idToken] = params'

    let PlatformInfo { jwksUrl } = pinfo
    jwkSet <- ExceptT . liftIO $ getJwkSet mgr jwksUrl

    let ss = sessionStore cfg
        oidc = fakeOidc jwkSet
    toCheck <- lift $ getValidIdTokenClaims ss oidc (encodeUtf8 state) (pure $ encodeUtf8 idToken)

    -- present nonce but seen -> error
    -- present nonce unseen -> good
    -- absent nonce -> different error
    nonceSeen <- case nonce toCheck of
        Just n  -> lift $ haveSeenNonce cfg n
        Nothing -> throwE $ InvalidLtiToken "missing nonce"
    when nonceSeen (throwE $ InvalidLtiToken "nonce seen before")

    case validatePlatformMessage pinfo toCheck of
        Left er   -> throwE $ InvalidLtiToken er
        Right tok -> return (state, tok)

------------------------------------------------------------
-- Auth token requesting machinery
------------------------------------------------------------

-- | Gets an auth token from the authorization server of the platform.
--
--   This token can then be used for performing requests to Assignment and
--   Grade Service and (not currently supported) Name and Role Provisioning
--   Service APIs.
getAuthToken
    :: (MonadIO m, MonadRandom m)
    => Manager
    -> PlatformInfo
    -> [AuthScope]
    -> [Jwk.Jwk]
    -> m (Either LTI13Exception (Jti, AuthTokenGrantResp))
getAuthToken manager pinfo scopes jwks = runExceptT $ do
    basicClaims <- makeJwtBasicClaims pinfo
    jti <- decodeUtf8 . B64.encode <$> lift (getRandomBytes 33)
    let basicClaims' = basicClaims
            { Jwt.jwtJti = Just jti
            , Jwt.jwtAud = Just $ platformTokenRequestAudience pinfo
            }
    jwt <- ExceptT $ fmapL GotJwtError <$> makeJwt jwks basicClaims'

    endp <- platformTokenEndpoint pinfo ?? NoTokenEndpoint
    let body =
            [ ("grant_type", "client_credentials")
            , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
            , ("client_assertion", Jwt.unJwt jwt)
            , ("scope", encodeUtf8 $ makeAuthScopesString scopes)
            ]
    resp <- mapExceptT liftIO
        $ handleExceptT GotHttpException $ doRequest endp body

    (jti,) <$> except (fmapL (JsonDecodeError . T.pack)
        $ A.eitherDecode
        $ responseBody resp)

    where
        doRequest url body = do
            req <- parseRequest $ T.unpack url
            let req' = urlEncodedBody body . setRequestCheckStatus $ req
            httpLbs req' manager


-- | Encodes a RS256 JWT from the given ToJSON message.
--
--   This message must have the basic claims in it already, e.g.
--   'DeepLinkingResponseMessage' rather than 'DeepLinkingResponse'.
makeJwt
    :: (MonadRandom m, ToJSON content)
    => [Jwk.Jwk]
    -> content
    -> m (Either Jwt.JwtError Jwt.Jwt)
makeJwt keys value =
    let encoded = toStrict $ A.encode value
        enc = Jwt.JwsEncoding RS256
        content = Jwt.Claims encoded
    in Jwt.encode keys enc content


-- | Makes the basic claims to send a message to the platform.
makeJwtBasicClaims :: MonadIO m => PlatformInfo -> m Jwt.JwtClaims
makeJwtBasicClaims PlatformInfo {..} = do
    timeNow <- liftIO getPOSIXTime
    let minute = secondsToNominalDiffTime 60
        expInterval = 60 * minute
        now = Jwt.IntDate timeNow
        expTime = Jwt.IntDate $ timeNow + expInterval
    return $ Jwt.JwtClaims
        { jwtIss = Just platformClientId
        , jwtAud = Just [platformIssuer]
        , jwtExp = Just expTime
        , jwtIat = Just now
        , jwtNbf = Nothing
        , jwtSub = Nothing
        , jwtJti = Nothing
        }

------------------------------------------------------------
-- Deep Linking
------------------------------------------------------------

-- | Makes a <https://www.imsglobal.org/spec/security/v1p0/#tool-originating-messages tool to platform message>
--   of deep linking data.
--
--   It will expire 60 minutes after running this.
makeDeepLinkingResponse
    :: (MonadRandom m, MonadIO m)
    => [Jwk.Jwk]
    -> PlatformInfo
    -> DeepLinkingResponse
    -> m (Either Jwt.JwtError Jwt.Jwt)
makeDeepLinkingResponse keys pinfo dlResponse = do
    basicClaims <- makeJwtBasicClaims pinfo
    makeJwt keys (JwtWithContent dlResponse basicClaims)

------------------------------------------------------------
-- Assignment and Grade Service
------------------------------------------------------------

submitScore
    :: (MonadIO m)
    => Manager
    -> AccessToken
    -> LineItemUrl
    -> AgsScoreUpdate
    -> m (Result ())
submitScore manager tok lineItem scoreUpdate = runExceptT $ do
    -- https://www.imsglobal.org/spec/lti-ags/v2p0/#service-endpoint
    let endp = lineItem <> "/scores"
        body = RequestBodyLBS $ A.encode scoreUpdate

    void $ mapExceptT liftIO
         $ handleExceptT GotHttpException
         $ doRequest endp body

    where
        ct = "Content-Type"
        myCt = "application/vnd.ims.lis.v1.score+json"
        applyAuth req = req
            { requestHeaders =
                ("Authentication", "Bearer " <> encodeUtf8 tok)
                    : requestHeaders req
            }
        applyContentType req = req
            { requestHeaders =
                (ct, myCt) : requestHeaders req
            }
        doRequest ep body = do
            req <- parseRequest $ T.unpack ep
            let req' = applyAuth
                    . applyContentType
                    . setRequestCheckStatus
                    $ req
                    { method = "POST"
                    , requestBody = body
                    }
            httpLbs req' manager

