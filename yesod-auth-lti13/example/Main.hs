{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.IORef (atomicModifyIORef', newIORef, IORef)
import Data.Text.Encoding (decodeUtf8)
import LoadEnv
import Network.HTTP.Conduit
import Network.Wai.Handler.Warp (runEnv)
import System.Environment (getEnv)
import Yesod
import Yesod.Auth
import Yesod.Auth.LTI13 (Nonce, authLTI13, YesodAuthLTI13(..), PlatformInfo(..))
import System.IO.Unsafe (unsafePerformIO)


type SeenNonces = Set.Set Nonce

data App = App
    { appHttpManager :: Manager
    , appAuthPlugins :: [AuthPlugin App]
    }

mkYesod "App" [parseRoutes|
    / RootR GET
    /auth AuthR Auth getAuth
|]

instance Yesod App where
    -- see https://github.com/thoughtbot/yesod-auth-oauth2/issues/87
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    loginDest _ = RootR
    logoutDest _ = RootR

    -- Disable any attempt to read persisted authenticated state
    maybeAuthId = return Nothing

    -- Copy the Creds response into the session for viewing after
    authenticate c = do
        mapM_ (uncurry setSession) $
            [ ("credsIdent", credsIdent c)
            , ("credsPlugin", credsPlugin c)
            ] ++ credsExtra c

        return $ Authenticated "1"

    authPlugins = appAuthPlugins

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- brittany-disable-next-binding

getRootR :: Handler Html
getRootR = do
    sess <- getSession

    let
        prettify
            = decodeUtf8
            . toStrict
            . encodePretty
            . fromJust
            . decode @Value
            . fromStrict

        mCredsIdent = decodeUtf8 <$> M.lookup "credsIdent" sess
        mCredsPlugin = decodeUtf8 <$> M.lookup "credsPlugin" sess
        mAccessToken = decodeUtf8 <$> M.lookup "accessToken" sess
        mUserResponse = prettify <$> M.lookup "userResponse" sess

    defaultLayout [whamlet|
        <h1>Yesod Auth OAuth2 Example
        <h2>
            <a href=@{AuthR LoginR}>Log in

        <h2>Credentials

        <h3>Plugin / Ident
        <p>#{show mCredsPlugin} / #{show mCredsIdent}

        <h3>Access Token
        <p>#{show mAccessToken}

        <h3>User Response
        <pre>
            $maybe userResponse <- mUserResponse
                #{userResponse}
    |]

-- This is strictly wrong but I have no idea how to properly get the state into
-- the `instance YesodAuthLTI13 App` if I put it in the App. PRs welcome.
seenNonces :: IORef SeenNonces
seenNonces = unsafePerformIO $ newIORef Set.empty

instance YesodAuthLTI13 App where
    checkSeenNonce nonce = do
        -- how do I get the App in this function?! literally WHAT
        seen <- liftIO $ atomicModifyIORef' seenNonces (\s -> (Set.insert nonce s, Set.member nonce s))
        return seen

    -- You should actually put a database here
    retrievePlatformInfo "a" = return $ PlatformInfo {
          platformIssuer = "a"
        , platformDeploymentId = "aaa"
        , platformClientId = "bbb"
        , platformOidcAuthEndpoint = "https://ccc.com"
        , jwksUrl = "https://ddd.com"
        }
    retrievePlatformInfo _ = fail $ "unknown platform"

mkFoundation :: IO App
mkFoundation = do
    loadEnv
    appHttpManager <- newManager tlsManagerSettings
    appAuthPlugins <- return $ [ authLTI13 ]
    return App {..}
