{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.IORef (readIORef, atomicModifyIORef', newIORef, IORef)
import Data.Text.Encoding (decodeUtf8)
import LoadEnv
import Network.HTTP.Conduit
import Network.Wai.Handler.Warp (runEnv)
import Yesod
import Yesod.Auth
import Yesod.Auth.LTI13
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

    onLogout = do
        $logDebug . T.pack . show =<< getSession

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
        mCredsIdent = decodeUtf8 <$> M.lookup "credsIdent" sess
        mCredsPlugin = decodeUtf8 <$> M.lookup "credsPlugin" sess
        mIss = M.lookup "ltiIss" sess
        mSub = M.lookup "ltiSub" sess
        mTok = M.lookup "ltiToken" sess

    defaultLayout [whamlet|
        <h1>Yesod Auth LTI1.3 Example
        <h2>Credentials

        <p><a href=@{AuthR LoginR}>Login page</a>
        <!-- XXX: logout doesn't work. It works in real apps, I promise! -->
        <!--<p><a href=@{AuthR LogoutR}>Logout</a>-->

        <h3>Plugin / Ident
        <p>#{show mCredsPlugin} / #{show mCredsIdent}

        <h3>Issuer
        <p>#{show mIss}

        <h3>Subject
        <p>#{show mSub}

        <h3>Token info
        <p>#{show mTok}

        <h3>The entire session
        <p>#{show $ M.toList sess}
    |]

-- This is strictly wrong but I have no idea how to properly get the state into
-- the `instance YesodAuthLTI13 App` if I put it in the App. PRs welcome.
seenNonces :: IORef SeenNonces
seenNonces = unsafePerformIO $ newIORef Set.empty

jwks :: IORef (Maybe BS.ByteString)
jwks = unsafePerformIO $ newIORef Nothing

instance YesodAuthLTI13 App where
    checkSeenNonce nonce = do
        -- how do I get the App in this function?! literally WHAT
        seen <- liftIO $ atomicModifyIORef' seenNonces (\s -> (Set.insert nonce s, Set.member nonce s))
        return seen

    -- You should actually put a database here
    retrievePlatformInfo ("aaaaa", Just "abcde") = return $ PlatformInfo {
          platformIssuer = "aaaaa"
        , platformClientId = "abcde"
        , platformOidcAuthEndpoint = "https://lti-ri.imsglobal.org/platforms/1255/authorizations/new"
        , jwksUrl = "https://lti-ri.imsglobal.org/platforms/1255/platform_keys/1248.json"
        }
    retrievePlatformInfo (iss, cid) = do
        $logWarn $ "unknown platform " <> iss <> " with client id " <> (T.pack $ show cid)
        liftIO $ fail "unknown platform"

    retrieveOrInsertJwks new = do
        -- possibly not thread safe. Also you should actually persist this.
        cur <- liftIO $ readIORef jwks
        makeIt <- liftIO $ maybe new pure cur
        liftIO $ atomicModifyIORef' jwks
            (\case
                Nothing -> (Just makeIt, makeIt)
                Just j -> (Just j, j))

mkFoundation :: IO App
mkFoundation = do
    loadEnv
    appHttpManager <- newManager tlsManagerSettings
    appAuthPlugins <- return $ [ authLTI13WithWidget login ]
    return App {..}
    where
        login _ = [whamlet|
            <p><a href="https://lti-ri.imsglobal.org/platforms/1255/resource_links">
                Start your login via the LTI reference implementation</a>
        |]

main :: IO ()
main = runEnv 3000 =<< toWaiApp =<< mkFoundation
