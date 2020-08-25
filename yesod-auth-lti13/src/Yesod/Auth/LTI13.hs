{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Yesod.Auth.LTI13 (
      PlatformInfo(..)
    , Issuer
    , YesodAuthLTI13Exception(..)
    , authLTI13
    , YesodAuthLTI13(..)
    , Nonce
    ) where

import Yesod.Core.Widget
import Yesod.Auth (MonadAuthHandler, Route(PluginR), setCredsRedirect, Creds(..), authHttpManager, AuthHandler, AuthPlugin(..), YesodAuth)
import Web.LTI13 (handleAuthResponse,
        LTI13Exception, initiate, RequestParams, PlatformInfo(..),
        Issuer, SessionStore(..), AuthFlowConfig(..)
        )
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Crypto.Random (getRandomBytes)
import Yesod.Core.Types (TypedContent)
import Yesod.Core (getUrlRender, getRouteToParent, permissionDenied,
                   setSession, lookupSession, redirect, deleteSession, lookupSessionBS,
                   setSessionBS, runRequestBody, getRequest, MonadHandler, notFound)
import qualified Data.ByteString.Base64.URL as B64
import Web.OIDC.Client.Tokens (IdTokenClaims(..))
import Yesod.Core (YesodRequest(reqGetParams))
import Control.Exception.Safe (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.OIDC.Client (Nonce)

data YesodAuthLTI13Exception
    = LTIException Text LTI13Exception
    -- ^ Issue with the token
    --   Plugin name and the original exception
    | BadRequest Text Text
    -- ^ Issue with the request
    --   Plugin name and an error message
    deriving (Show)

instance Exception YesodAuthLTI13Exception

dispatchAuthRequest
    :: (YesodAuthLTI13 master, MonadAuthHandler master m)
    => AuthFlowConfig m
    -> Text
    -- ^ Name of the auth provider
    -> Text
    -- ^ Method
    -> [Text]
    -- ^ Path parts
    -> AuthHandler master TypedContent
dispatchAuthRequest cfg name "GET" ["initiate"] =
    unifyParams GET  >>= dispatchInitiate name cfg
dispatchAuthRequest cfg name "POST" ["initiate"] =
    unifyParams POST >>= dispatchInitiate name cfg
dispatchAuthRequest cfg name "POST" ["authenticate"] =
    dispatchAuthenticate name cfg
dispatchAuthRequest _ _ _ _ = notFound

-- | HTTP method for 'unifyParams'
data Method = GET
            | POST

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

-- | Makes the name for the @iss@ cookie
myIss :: Text -> Text
myIss = flip prefixSession $ "iss"

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
        sname = prefixSession name "state"
        nname = prefixSession name "nonce"
        sessionSave state nonce = do
            setSessionBS sname state
            setSessionBS nname nonce
        sessionGet = do
            state <- lookupSessionBS sname
            nonce <- lookupSessionBS nname
            return (state, nonce)
        sessionDelete = do
            deleteSession sname
            deleteSession nname

makeCfg
    :: MonadHandler m
    => Text
    -> (Issuer -> m PlatformInfo)
    -> (Nonce -> m Bool)
    -> m (AuthFlowConfig m)
makeCfg name pinfo seenNonce = do
    let url = PluginR name ["authenticate"]
    tm <- getRouteToParent
    render <- getUrlRender
    return AuthFlowConfig
        { getPlatformInfo = pinfo
        , haveSeenNonce = seenNonce
        , myRedirectUri = render $ tm url
        , sessionStore = mkSessionStore name
        }

dispatchInitiate
    :: (YesodAuthLTI13 master, MonadAuthHandler master m)
    => Text
    -- ^ Name of the provider
    -> AuthFlowConfig m
    -> RequestParams
    -- ^ Request parameters
    -> AuthHandler master TypedContent
dispatchInitiate name cfg params = do
    (iss, redir) <- initiate cfg params
    setSession (myIss name) iss
    redirect redir


checkCSRFToken :: MonadHandler m => Text -> m ()
checkCSRFToken state = do
    state' <- lookupSession "state"
    -- they do not match or the state is wrong
    if state' /= Just state then
        permissionDenied "Bad CSRF token"
    else
        return ()

-- | Makes a user ID that is not an email address (and should thus be safe from
--   [possible security problem] collisions with email based auth systems)
makeUserId :: Issuer -> Text -> Text
makeUserId iss name = name <> "@@" <> iss

dispatchAuthenticate :: (YesodAuthLTI13 m, MonadAuthHandler m n) => Text -> AuthFlowConfig n -> AuthHandler m TypedContent
dispatchAuthenticate name cfg = do
    mgr <- authHttpManager
    -- first, find who the issuer was
    -- this is safe, least of which because Yesod has encrypted session cookies
    maybeIss <- lookupSession $ myIss name
    iss <- maybe (liftIO $ throwIO $ BadRequest name "missing `iss` cookie")
                 pure
                 maybeIss
    deleteSession $ myIss name

    (params', _) <- runRequestBody
    let params = Map.fromList params'
    (state, tok) <- handleAuthResponse mgr cfg params iss

    -- check CSRF token against the state in the request
    checkCSRFToken state

    let IdTokenClaims { sub } = tok
        myCreds = Creds {
              credsPlugin = name
            , credsIdent = makeUserId iss sub
            , credsExtra = []
            -- TODO: we should probably give the user some of the stuff we
            --       parsed from the LTI token, but I am not 100% sure how yet
        }

    setCredsRedirect myCreds

class (YesodAuth site)
    => YesodAuthLTI13 site where
        -- | Check if a nonce has been seen in the last validity period. It is
        --  expected that nonces given to this function are stored somewhere,
        --  returning False, then when seen again, True should be returned.
        --  See the <http://www.imsglobal.org/spec/security/v1p0/#authentication-response-validation
        --  relevant section of the IMS security specification> for details.
        checkSeenNonce :: Nonce -> AuthHandler site (Bool)

        -- | Get the configuration for the given platform
        retrievePlatformInfo :: Issuer -> AuthHandler site (PlatformInfo)

authLTI13 :: YesodAuthLTI13 m => AuthPlugin m
authLTI13 = do
    AuthPlugin name (dispatchAuthRequest name) login
    where
        name = "lti13"
        login _ = [whamlet|Login via your Learning Management System|]
