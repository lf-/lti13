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
import qualified Data.Set as Set
import Data.IORef (atomicModifyIORef', newIORef, IORef)
import Data.Text.Encoding (decodeUtf8)
import LoadEnv
import Network.HTTP.Conduit
import Network.Wai.Handler.Warp (runEnv)
import Yesod
import Yesod.Auth
import Yesod.Auth.LTI13 (
    Nonce, authLTI13, YesodAuthLTI13(..), PlatformInfo(..))
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
    approot = ApprootStatic "http://localhost:3000"
    shouldLogIO _ _ level = return $ level >= LevelDebug

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
        mIss = decodeUtf8 <$> M.lookup "ltiIss" sess
        mSub = decodeUtf8 <$> M.lookup "ltiSub" sess

    defaultLayout [whamlet|
        <h1>Yesod Auth LTI1.3 Example
        <h2>Credentials

        <h3>Plugin / Ident
        <p>#{show mCredsPlugin} / #{show mCredsIdent}

        <h3>Issuer
        <p>#{show mIss}

        <h3>Subject
        <p>#{show mSub}
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
    retrievePlatformInfo "aaaaa" = return $ PlatformInfo {
          platformIssuer = "aaaaa"
        , platformDeploymentId = "aaa"
        , platformClientId = "abcde"
        , platformOidcAuthEndpoint = "https://lti-ri.imsglobal.org/platforms/1255/authorizations/new"
        , jwksUrl = "https://lti-ri.imsglobal.org/platforms/1255/platform_keys/1248.json"
        }
    retrievePlatformInfo iss = do
        $logWarn $ "unknown platform " <> iss
        liftIO $ fail "unknown platform"

mkFoundation :: IO App
mkFoundation = do
    loadEnv
    appHttpManager <- newManager tlsManagerSettings
    appAuthPlugins <- return $ [ authLTI13 ]
    return App {..}

main :: IO ()
main = runEnv 3000 =<< toWaiApp =<< mkFoundation
