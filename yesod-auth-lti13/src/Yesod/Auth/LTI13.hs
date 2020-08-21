{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.LTI13 (
      PlatformInfo
    , GetPlatformInfo
    , Issuer
    , authLTI13
    ) where

import Yesod.Core.Widget
import Yesod.Auth (MonadAuthHandler, AuthHandler, AuthPlugin(..), YesodAuth)
import Web.LTI13 (handleAuthResponse, 
        LTI13Exception, initiate, RequestParams, PlatformInfo, GetPlatformInfo,
        Issuer, SessionStore(..), AuthFlowConfig(..)
        )
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Crypto.Random (getRandomBytes, drgNew, DRG(randomBytesGenerate))
import Yesod.Core.Types (TypedContent)
import Yesod.Core (permissionDenied, setSession, lookupSession, redirect,
        deleteSession, lookupSessionBS, setSessionBS, runRequestBody,
        getRequest, MonadHandler, notFound)
import qualified Data.ByteString.Base64.URL as B64
import Network.HTTP.Client (Manager)
import Yesod.Core (YesodRequest(reqGetParams))
import Control.Exception.Safe (Exception, MonadThrow, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Control.Monad.Fail (MonadFail)
import Control.Monad ((>=>))

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
    :: (MonadHandler m)
    => Text
    -- ^ Name of the auth provider
    -> Manager
    -- ^ HTTP 'Manager' to use for authentication calls
    -> GetPlatformInfo IO
    -- ^ Function to get the platform details
    -> Text
    -- ^ Method
    -> [Text]
    -- ^ Path parts
    -> m b
dispatchAuthRequest name _ platformInfo "GET" ["initiate"] =
    unifyParams GET  >>= dispatchInitiate name platformInfo
dispatchAuthRequest name _ platformInfo "POST" ["initiate"] =
    unifyParams POST >>= dispatchInitiate name platformInfo
dispatchAuthRequest name mgr platformInfo "POST" ["authenticate"] =
    dispatchAuthenticate name mgr platformInfo
dispatchAuthRequest _ _ _ _ _ = notFound

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

makeCfg :: MonadHandler m => Text -> GetPlatformInfo IO -> AuthFlowConfig m
makeCfg name pinfo =
    AuthFlowConfig
        { getPlatformInfo = liftIO . pinfo
        -- TODO: FIX THIS IT IS REALLY BAD
        , haveSeenNonce = \_ -> return False
        , myRedirectUri = "http://localhost:3000/auth/authenticate"
        , sessionStore = mkSessionStore name
        }

dispatchInitiate
    :: MonadHandler m
    => Text
    -- ^ Name of the provider
    -> GetPlatformInfo IO
    -- ^ Function to get the parameters for a given issuer
    -> RequestParams
    -- ^ Request parameters
    -> m b
dispatchInitiate name platformInfo params = do
    let cfg = makeCfg name platformInfo
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

dispatchAuthenticate :: (MonadHandler m) => Text -> Manager -> GetPlatformInfo IO -> m b
dispatchAuthenticate name mgr platformInfo = do
    -- first, find who the issuer was
    -- this is safe, least of which because Yesod has encrypted session cookies
    maybeIss <- lookupSession $ myIss name
    iss <- maybe (liftIO $ throwIO $ BadRequest name "missing `iss` cookie")
                 pure
                 maybeIss
    deleteSession $ myIss name

    let cfg = makeCfg name platformInfo
    (params', _) <- runRequestBody
    let params = Map.fromList params'
    (state, tok) <- handleAuthResponse mgr cfg params iss

    -- check CSRF token against the state in the request
    checkCSRFToken state

    notFound

authLTI13 :: YesodAuth m => GetPlatformInfo IO -> AuthPlugin m
authLTI13 platformInfo =
    AuthPlugin name (dispatchAuthRequest name authHttpManager platformInfo) login
    where
        name = "lti13"
        login _ = [whamlet|Login via your Learning Management System|]
authHttpManager :: Manager
authHttpManager = error "not implemented"

