{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Auth.LTI13 (
      PlatformInfo(..)
    , Issuer
    , Nonce
    , authLTI13
    , YesodAuthLTI13(..)
    ) where

import Yesod.Core.Widget
import Yesod.Auth (Route(PluginR), setCredsRedirect, Creds(..), authHttpManager, AuthHandler, AuthPlugin(..), YesodAuth)
import Web.LTI13 (handleAuthResponse,
        LTI13Exception, initiate, RequestParams, PlatformInfo(..),
        Issuer, SessionStore(..), AuthFlowConfig(..)
        )
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Crypto.Random (getRandomBytes)
import Yesod.Core.Types (TypedContent)
import Yesod.Core (permissionDenied, setSession, lookupSession, redirect,
        deleteSession, lookupSessionBS, setSessionBS, runRequestBody,
        getRequest, MonadHandler, notFound, getUrlRender)
import qualified Data.ByteString.Base64.URL as B64
import Web.OIDC.Client.Tokens (IdTokenClaims(..))
import Yesod.Core (YesodRequest(reqGetParams))
import Control.Exception.Safe (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.OIDC.Client (Nonce)
import Yesod.Core.Handler (getRouteToParent)

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
    :: YesodAuthLTI13 master
    => Text
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
dispatchAuthRequest _ _ _ = notFound

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
    -> (Issuer -> m PlatformInfo)
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
    (iss, redir) <- initiate cfg params
    setSession (myIss name) iss
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
    iss <- maybe (liftIO $ throwIO $ BadRequest name "missing `iss` cookie")
                 pure
                 maybeIss
    deleteSession $ myIss name

    state' <- lookupSession $ myState name

    -- we don't care about having a callback URL here since we *are* the callback
    let cfg = makeCfg name retrievePlatformInfo checkSeenNonce undefined
    (params', _) <- runRequestBody
    let params = Map.fromList params'
    (state, tok) <- handleAuthResponse mgr cfg params iss

    -- check CSRF token against the state in the request
    checkCSRFToken state state'

    let IdTokenClaims { sub } = tok
        myCreds = Creds {
              credsPlugin = name
            , credsIdent = makeUserId iss sub
            , credsExtra = [("ltiIss", iss), ("ltiSub", sub)]
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

