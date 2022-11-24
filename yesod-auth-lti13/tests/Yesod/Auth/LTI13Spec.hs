{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Yesod.Auth.LTI13Spec (spec) where

import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.ByteString
import Data.Default
import Data.IORef
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text
import GHC.Generics (Generic)
import Language.Haskell.TH (Type (..))
import Network.Wai qualified as Wai
import Test.Hspec
import Yesod
import Yesod.Auth
import Yesod.Auth.LTI13
import Yesod.Core.Types
import Yesod.Test qualified as YT
import Prelude

data FakeSchema = FakeSchema
  { seenNonces :: Set Nonce
  , jwks :: Maybe ByteString
  }
  deriving stock (Generic)
  deriving anyclass (Default)

newtype FakeDb = FakeDb {unFakeDb :: IORef FakeSchema}

data App = App
  { appDB :: FakeDb
  , appAuthPlugins :: [AuthPlugin App]
  }

instance PersistCore FakeDb where
  data BackendKey FakeDb = FakeDbKey Int
    deriving stock (Generic, Eq, Show, Read, Ord)

instance PersistField (BackendKey FakeDb) where
  toPersistValue (FakeDbKey a) = toPersistValue a
  fromPersistValue a = FakeDbKey <$> fromPersistValue a

instance FromJSON (BackendKey FakeDb) where
  parseJSON a = FakeDbKey <$> parseJSON a

instance HasPersistBackend FakeDb where
  type BaseBackend FakeDb = FakeDb
  persistBackend = id

instance PersistStoreRead FakeDb where
  get = error "get"

instance PersistStoreWrite FakeDb where
  insert = error "insert"
  insertKey = error "insertKey"
  repsert = error "repsert"
  replace = error "replace"
  delete = error "delete"
  update = error "update"

instance ToJSON (BackendKey FakeDb) where
  toEncoding (FakeDbKey a) = toEncoding a

-- XXX: this is literally just here to make yesod-auth happy
mkPersist
  (mkPersistSettings (ConT ''FakeDb))
  [persistLowerCase|
  User
    Id Text
    name Text
  |]

instance RenderRoute App where
  data Route App = AuthR AuthRoute | RootR
    deriving stock (Show, Eq, Read)
  renderRoute (AuthR authR) = let (parts, params) = renderRoute authR in ("auth" : parts, params)
  renderRoute RootR = ([], [])

instance ParseRoute App where
  parseRoute ("auth" : rest, x) = AuthR <$> parseRoute (rest, x)
  parseRoute ([], _) = Just RootR
  parseRoute _ = Nothing

instance YesodPersist App where
  type YesodPersistBackend App = FakeDb
  runDB :: YesodDB App a -> HandlerFor App a
  runDB act = do
    -- act is a ReaderT (IORef FakeDb) (HandlerFor app a)
    db <- asks (appDB . rheSite . handlerEnv)
    runReaderT act db

instance YesodAuthPersist App where
  type AuthEntity App = User

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = RootR
  logoutDest _ = RootR

  authenticate _creds = do
    error "authenticate"

  authPlugins = appAuthPlugins

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance Yesod App

-- FIXME: idk if reimplementing this was actually a good idea rather than just
-- using the template haskell, lmao, however, it was fun!
instance YesodDispatch App where
  yesodDispatch yre req = do
    go $ Wai.pathInfo req
   where
    go [] = yesodRunner (pure . toTypedContent @Text $ "nya") yre (Just RootR) req
    go ("auth" : rest) = yesodSubDispatch ysre (setPathInfo rest req)
    go _ = yesodRunner (notFound @_ @Html) yre Nothing req

    setPathInfo newPathInfo theReq = theReq {Wai.pathInfo = newPathInfo}
    ysre =
      YesodSubRunnerEnv
        { ysreParentRunner = yesodRunner
        , ysreGetSub = getAuth
        , ysreToParentRoute = AuthR
        , ysreParentEnv = yre
        }

instance YesodAuthLTI13 App where
  checkSeenNonce = error "checkSeenNonce"
  retrievePlatformInfo ("https://fakeplatform.example.com", _) =
    pure
      PlatformInfo
        { platformIssuer = issuer
        , platformClientId = "clientId"
        , platformOidcAuthEndpoint = "https://fakeplatform.example.com/oidc"
        , jwksUrl = error "FIXME: needs to have stubs to do this so it will actually work"
        }
  retrievePlatformInfo _ = error "nope"
  retrieveOrInsertJwks = error "retrieveOrInsertJwks"

withApp :: YT.YesodSpec App -> Spec
withApp =
  YT.yesodSpecWithSiteGenerator
    ( do
        appDB <- FakeDb <$> newIORef def
        pure App {appDB, appAuthPlugins = [authLTI13]}
    )

issuer :: Text
issuer = "https://fakeplatform.example.com"
loginHint :: Text
loginHint = "login_hint"
myTargetLinkUri :: Text
myTargetLinkUri = "https://faketool.example.com"

spec :: Spec
spec = withApp $ YT.ydescribe "Auth" do
  YT.ydescribe "initiate" do
    YT.yit "Returns 400 if any of the request parameters are missing" do
      doInitiate [("login_hint", loginHint), ("target_link_uri", myTargetLinkUri)]
      YT.statusIs 400
      doInitiate [("iss", issuer), ("target_link_uri", myTargetLinkUri)]
      YT.statusIs 400
      doInitiate [("iss", issuer), ("login_hint", loginHint)]
      YT.statusIs 400

    YT.yit "Redirects" do
      doInitiate [("iss", issuer), ("login_hint", loginHint), ("target_link_uri", myTargetLinkUri)]
      YT.statusIs 303
      pure ()
 where
  doInitiate params = do
    YT.request do
      YT.setMethod "POST"
      mapM_ (uncurry YT.addPostParam) params
      YT.setUrl $ AuthR (fromJust $ parseRoute @Auth (["page", "lti13", "initiate"], []))
