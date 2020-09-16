{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}


-- | A Yesod authentication module for LTI 1.3
--   See @example/Main.hs@ for a sample implementation.
--
--   Configuration:
--
--       * Login initiation URL: http://localhost:3000/auth/page/lti13/initiate
--
--       * JWKs URL: http://localhost:3000/auth/page/lti13/jwks
--
--       * Tool link URL: http://localhost:3000
module Yesod.Auth.LTI13 (
      PlatformInfo(..)
    , Issuer
    , ClientId
    , Nonce
    , authLTI13
    , authLTI13WithWidget
    , YesodAuthLTI13(..)
    , getLtiIss
    , getLtiSub
    , getLtiToken
    , LtiTokenClaims(..)
    , UncheckedLtiTokenClaims(..)
    , ContextClaim(..)
    , Role(..)
    ) where

import Yesod.Core.Widget
import Yesod.Auth (Auth, Route(PluginR), setCredsRedirect, Creds(..), authHttpManager, AuthHandler, AuthPlugin(..), YesodAuth)
import Web.LTI13
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Crypto.Random (getRandomBytes)
import qualified Crypto.PubKey.RSA as RSA
import Yesod.Core.Types (TypedContent)
import Yesod.Core (toTypedContent, permissionDenied, setSession, lookupSession, redirect,
        deleteSession, lookupSessionBS, setSessionBS, runRequestBody,
        getRequest, MonadHandler, SessionMap, notFound, getUrlRender)
import qualified Data.ByteString.Base64.URL as B64
import Web.OIDC.Client.Tokens (IdTokenClaims(..))
import Yesod.Core (YesodRequest(reqGetParams))
import Control.Exception.Safe (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.OIDC.Client (Nonce)
import Yesod.Core.Handler (getRouteToParent)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Jose.Jwk (JwkSet(..), Jwk(..), generateRsaKeyPair, KeyUse(Sig))
import Data.Time (getCurrentTime)
import Jose.Jwt (KeyId(UTCKeyId))
import Jose.Jwa (Alg(Signed), JwsAlg(RS256))

data YesodAuthLTI13Exception
    = LTIException Text LTI13Exception
    -- ^ Issue with the token
    --   Plugin name and the original exception
    | BadRequest Text Text
    -- ^ Issue with the request
    --   Plugin name and an error message
    | CorruptJwks Text Text
    -- ^ The jwks stored in the database are corrupt. Wat.
    deriving (Show)

instance Exception YesodAuthLTI13Exception

dispatchAuthRequest
    :: YesodAuthLTI13 master
    => PluginName
    -- ^ Name of the auth provider
    -> Text
    -- ^ Method
    -> [Text]
    -- ^ Path parts
    -> AuthHandler master TypedContent
dispatchAuthRequest name "GET" ["initiate"] =
    unifyParams GET  >>= dispatchInitiate name
dispatchAuthRequest name "POST" ["initiate"] =
    unifyParams POST >>= dispatchInitiate name
dispatchAuthRequest name "POST" ["authenticate"] =
    dispatchAuthenticate name
dispatchAuthRequest name "GET" ["jwks"] =
    dispatchJwks name
dispatchAuthRequest _ _ _ = notFound

-- | HTTP method for @unifyParams@
data Method = GET
            | POST

-- | Turns parameters from their respective request type to a simple map.
unifyParams
    :: MonadHandler m
    => Method
    -> m RequestParams
unifyParams GET = do
    req <- getRequest
    return $ Map.fromList $ reqGetParams req
unifyParams POST = do
    (params, _) <- runRequestBody
    return $ Map.fromList params

-- | Makes a name for a saved session piece
prefixSession :: Text -> Text -> Text
prefixSession name datum =
    "_lti13_" <> name <> "_" <> datum

-- | Makes the name for the @clientId@ cookie
myCid :: Text -> Text
myCid = flip prefixSession $ "clientId"

-- | Makes the name for the @iss@ cookie
myIss :: Text -> Text
myIss = flip prefixSession $ "iss"

-- | Makes the name for the @state@ cookie
myState :: Text -> Text
myState = flip prefixSession $ "state"

-- | Makes the name for the @nonce@ cookie
myNonce :: Text -> Text
myNonce = flip prefixSession $ "nonce"

mkSessionStore :: MonadHandler m => Text -> SessionStore m
mkSessionStore name =
    SessionStore
        { sessionStoreGenerate = gen
        , sessionStoreSave     = sessionSave
        , sessionStoreGet      = sessionGet
        , sessionStoreDelete   = sessionDelete
        }
    where
        -- we make only url safe stuff to not cause chaos elsewhere
        gen = liftIO $ (B64.encode <$> getRandomBytes 33)
        sname = myState name
        nname = myNonce name
        sessionSave state nonce = do
            setSessionBS sname state
            setSessionBS nname nonce
            return ()
        sessionGet = do
            state <- lookupSessionBS sname
            nonce <- lookupSessionBS nname
            return (state, nonce)
        sessionDelete = do
            deleteSession sname
            deleteSession nname

type PluginName = Text

makeCfg
    :: MonadHandler m
    => PluginName
    -> ((Issuer, Maybe ClientId) -> m PlatformInfo)
    -> (Nonce -> m Bool)
    -> Text
    -> AuthFlowConfig m
makeCfg name pinfo seenNonce callback =
    AuthFlowConfig
        { getPlatformInfo = pinfo
        , haveSeenNonce = seenNonce
        , myRedirectUri = callback
        , sessionStore = mkSessionStore name
        }

createNewJwk :: IO Jwk
createNewJwk = do
    kid <- UTCKeyId <$> getCurrentTime
    let use = Sig
        alg = Signed RS256
    (_, priv) <- generateRsaKeyPair 256 kid use $ Just alg
    return priv

dispatchJwks
    :: YesodAuthLTI13 master
    => PluginName
    -> AuthHandler master TypedContent
dispatchJwks name = do
    jwks <- retrieveOrInsertJwks makeJwks
    JwkSet privs <- maybe (liftIO $ throwIO $ CorruptJwks name "json decode failed")
                    pure (A.decodeStrict jwks)
    let pubs = JwkSet $ map rsaPrivToPub privs
    return $ toTypedContent $ A.toJSON pubs
    where makeJwks = (LBS.toStrict . A.encode) <$> makeJwkSet
          makeJwkSet = fmap (\jwk -> JwkSet {keys = [jwk]}) createNewJwk

rsaPrivToPub :: Jwk -> Jwk
rsaPrivToPub (RsaPrivateJwk privKey mId mUse mAlg) =
    RsaPublicJwk (RSA.private_pub privKey) mId mUse mAlg
rsaPrivToPub _ = error "rsaPrivToPub called on a Jwk that's not a RsaPrivateJwk"

dispatchInitiate
    :: YesodAuthLTI13 master
    => PluginName
    -- ^ Name of the provider
    -> RequestParams
    -- ^ Request parameters
    -> AuthHandler master TypedContent
dispatchInitiate name params = do
    -- TODO: this should be refactored into a function but I don't know how
    let url = PluginR name ["authenticate"]
    tm <- getRouteToParent
    render <- getUrlRender
    let authUrl = render $ tm url

    let cfg = makeCfg name retrievePlatformInfo checkSeenNonce authUrl
    (iss, cid, redir) <- initiate cfg params
    setSession (myIss name) iss
    setSession (myCid name) cid
    redirect redir

type State = Text

checkCSRFToken :: MonadHandler m => State -> Maybe State -> m ()
checkCSRFToken state state' = do
    -- they do not match or the state is wrong
    if state' /= Just state then do
        permissionDenied "Bad CSRF token"
    else
        return ()

-- | Makes a user ID that is not an email address (and should thus be safe from
--   [possible security problem] collisions with email based auth systems)
makeUserId :: Issuer -> Text -> Text
makeUserId iss name = name <> "@@" <> iss

dispatchAuthenticate :: YesodAuthLTI13 m => PluginName -> AuthHandler m TypedContent
dispatchAuthenticate name = do
    mgr <- authHttpManager
    -- first, find who the issuer was
    -- this is safe, least of which because Yesod has encrypted session cookies
    maybeIss <- lookupSession $ myIss name
    iss <- maybe (liftIO . throwIO $ BadRequest name "missing `iss` cookie")
                 pure
                 maybeIss
    cid <- lookupSession $ myCid name
    deleteSession $ myIss name
    deleteSession $ myCid name

    state' <- lookupSession $ myState name

    pinfo <- retrievePlatformInfo (iss, cid)

    -- we don't care about having a callback URL here since we *are* the callback
    let cfg = makeCfg name retrievePlatformInfo checkSeenNonce undefined
    (params', _) <- runRequestBody
    let params = Map.fromList params'
    (state, tok) <- handleAuthResponse mgr cfg params pinfo

    -- check CSRF token against the state in the request
    checkCSRFToken state state'

    let LtiTokenClaims ltiClaims = otherClaims tok
        ltiClaimsJson = E.decodeUtf8 $ LBS.toStrict $ A.encode ltiClaims

    let IdTokenClaims { sub } = tok
        myCreds = Creds {
              credsPlugin = name
            , credsIdent = makeUserId iss sub
            , credsExtra = [("ltiIss", iss), ("ltiSub", sub), ("ltiToken", ltiClaimsJson)]
        }

    setCredsRedirect myCreds

-- | Gets the @iss@ for the given sesssion
getLtiIss :: SessionMap -> Maybe Issuer
getLtiIss sess =
    E.decodeUtf8 <$> Map.lookup "ltiIss" sess

-- | Gets the @sub@ for the given session
getLtiSub :: SessionMap -> Maybe Issuer
getLtiSub sess =
    E.decodeUtf8 <$> Map.lookup "ltiSub" sess

-- | Gets and decodes the extra token claims with the full LTI launch
--   information from a session
--
--   Signature slightly inaccurate: the claims have been checked at this stage.
getLtiToken :: SessionMap -> Maybe UncheckedLtiTokenClaims
getLtiToken sess =
    (Map.lookup "ltiToken" sess)
    >>= A.decodeStrict

-- | Callbacks into your site for LTI 1.3
class (YesodAuth site)
    => YesodAuthLTI13 site where
        -- | Check if a nonce has been seen in the last validity period. It is
        --  expected that nonces given to this function are stored somewhere,
        --  returning False, then when seen again, True should be returned.
        --  See the <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation
        --  relevant section of the IMS security specification> for details.
        checkSeenNonce :: Nonce -> AuthHandler site (Bool)

        -- | Get the configuration for the given platform.
        --
        --   It is possible that the relation between Issuer and ClientId is 1
        --   to n rather than 1 to 1, for instance in the case of cloud hosted
        --   Canvas. You *must* therefore key your 'PlatformInfo' retrieval
        --   with the pair of both and throw an error if there are multiple
        --   'ClientId' for the given 'Issuer' and the 'ClientId' is 'Nothing'.
        retrievePlatformInfo :: (Issuer, Maybe ClientId) -> AuthHandler site (PlatformInfo)

        -- | Retrieve JWKs list from the database or other store. If not
        --   present, please create a new one by evaluating the given 'IO', store
        --   it, and return it.
        retrieveOrInsertJwks
            :: (IO BS.ByteString)
            -- ^ an 'IO' which, if evaluated, will make a new 'Jwk' set
            -> AuthHandler site (BS.ByteString)

-- | Auth plugin. Add this to @appAuthPlugins@ to enable this plugin.
authLTI13 :: YesodAuthLTI13 m => AuthPlugin m
authLTI13 = authLTI13WithWidget login
    where
        login _ = [whamlet|<p>Go to your Learning Management System to log in via LTI 1.3|]

-- | Auth plugin. The same as 'authLTI13' but you can provide your own template
--   for the login hint page.
authLTI13WithWidget :: YesodAuthLTI13 m => ((Route Auth -> Route m) -> WidgetFor m ()) -> AuthPlugin m
authLTI13WithWidget login = do
    AuthPlugin name (dispatchAuthRequest name) login
    where
        name = "lti13"
