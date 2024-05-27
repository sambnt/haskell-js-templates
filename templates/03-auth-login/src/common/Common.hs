{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
{-# LANGUAGE JavaScriptFFI #-}
#endif

module Common where

import Servant.API
    ( type (:<|>)(..), JSON, ReqBody, type (:>), Get, Post, Headers, Header, QueryParam)
import Web.Cookie (SetCookie)
import Data.Proxy ( Proxy(..) )
import qualified Network.URI as Network
import Servant.Links (linkURI)
import GHC.Generics (Generic)
import Servant.Client.Core (clientIn, HasClient(Client), RunClient)
import URI.ByteString (parseURI, laxURIParserOptions, URIRef, Absolute)
import Servant.API (AuthProtect, (:-), (:>), Get, JSON, NamedRoutes)
import qualified Data.ByteString as ByteString
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (IdentityT, runIdentityT)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow, try)
import qualified Control.Concurrent.STM as STM
import Servant.Client.Generic (AsClientT, genericClient)

#if defined(ghcjs_HOST_OS)
import qualified GHCJS.DOM.Types as DOM
#endif

type OpaqueCode = Text

data AuthInputs = AuthInputs { authOpaque :: Maybe OpaqueCode
                             , authCode   :: Maybe Text
                             , authNonce  :: Maybe Text
                             }

data AuthStateCompleted = AuthStateCompleted { authStateCompletedIdToken      :: Text
                                             , authStateCompletedAccessToken  :: Text
                                             , authStateCompletedRefreshToken :: Text
                                             -- auth user?
                                             }

data AuthInfo = AuthInfo { authIdToken      :: Text
                         , authAccessToken  :: Text
                         , authRefreshToken :: Text
                         -- , authUser?
                         }

data AuthState = InFlight OpaqueCode
               | Done Text AuthInfo

data AuthKey = Cookie OpaqueCode
             | Nonce Text
  deriving (Eq, Ord)

data Database = Database
  { dbCounter :: TVar Int
  , dbAuth :: TVar (Map AuthKey AuthState)
  }

getAuthState :: Database -> AuthKey -> IO (Maybe AuthState)
getAuthState db k = do
  authStateMap <- STM.readTVarIO (dbAuth db)
  pure $ Map.lookup k authStateMap
-- getAuthState _ (AuthInputs Nothing _ _) =
--   pure Nothing
-- getAuthState db (AuthInputs (Just opaqueCode) _ _) = do
    -- Nothing -> pure Nothing
    -- Just (InFlight nonce') ->
    --   if nonce /= nonce'
    --   then error "Incorrect state returned from auth provider"
    --   else

parseURI' :: ByteString.ByteString -> URIRef Absolute
parseURI' = either (\err -> error $ show err) id . parseURI laxURIParserOptions

writeAuthState :: Database -> AuthKey -> AuthState -> IO ()
writeAuthState db k newState =
  STM.atomically $ STM.modifyTVar (dbAuth db) (Map.insert k newState)

data Cognito = Cognito deriving (Eq, Show)

-- | Users must provide an opaque access token to authenticate.
type AuthAccess = AuthProtect "access-token'"

newtype App a = App (IdentityT IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)

data AuthUser
  = AuthUser { authUserSub           :: !Text
             , authUserName          :: !Text
             , authUserEmail         :: !Text
             , authUserEmailVerified :: !Bool
             }
  deriving (Eq, Show, Generic)
  deriving (ToJSON)

instance FromJSON AuthUser where
  parseJSON = Aeson.withObject "AuthUser" $ \v -> AuthUser
    <$> v Aeson..: "sub"
    <*> v Aeson..: "name"
    <*> v Aeson..: "email"
    <*> v Aeson..: "email_verified"

data Api mode = Api
  { getCounter
    :: mode
    :- "counter"
    :> Header "Cookie" Text
    :> Get '[JSON] Int
  , login
    :: mode
    :- "login"
    :> Header "Cookie" Text
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())
  , secured
    :: mode
    :- AuthAccess
    :> NamedRoutes SecuredApi
  }
  deriving Generic

data SecuredApi mode = SecuredApi
  { setCounter
    :: mode
    :- "counter"
    :> ReqBody '[JSON] Int
    :> Post '[JSON] ()
  }
  deriving Generic

client :: RunClient m => Api (AsClientT m)
client = genericClient

#if defined(ghcjs_HOST_OS)
-- Interruptible indicates an asynchronous import.
foreign import javascript interruptible "((x, $c) => { return digestMessage(x).then(function(v) { $c(null, v); }, function(e) { $c(e, null); }); })"
  js_digestMessage :: DOM.JSString -> IO (DOM.JSVal, DOM.JSString)

digestMessage :: DOM.JSString -> IO DOM.JSString
digestMessage msg = DOM.checkPromiseResult =<< js_digestMessage msg
#endif

-- foreign import javascript interruptible
--         "(($1, $c) => { return $1[\"arrayBuffer\"]().then(function(s) { $c(null, s);}, function(e) { $c(e, null);}); })"
--         js_arrayBuffer :: Body -> IO (JSVal, ArrayBuffer)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Body.arrayBuffer Mozilla Body.arrayBuffer documentation>
-- arrayBuffer :: (MonadIO m, IsBody self) => self -> m ArrayBuffer
-- arrayBuffer self
--   = liftIO ((js_arrayBuffer (toBody self)) >>= checkPromiseResult)

-- foreign import javascript unsafe "((x) => { return window.crypto.subtle.digest('SHA-256', x)};)"
--   js_digestMessage :: DOM.JSString -> IO (DOM.JSVal, DOM.JSString)

-- digestMessage :: DOM.JSString -> IO DOM.JSString
-- digestMessage msg = DOM.checkPromiseResult =<< js_digestMessage msg


-- REACT_APP_COGNITO_CLIENT_ID=4bbo68nrfgf1vif5jl0gt4hk71
-- REACT_APP_COGNITO_USER_POOL_ID=ap-southeast-2_wmCNNCByv
-- REACT_APP_COGNITO_DOMAIN_NAME=bnt-test.auth.ap-southeast-2.amazoncognito.com

-- x =
--   let application =
--         AuthorizationCodeApplication
--           { acName = "foo-cognito-app"
--           , acClientId = ""
--           , acClientSecret :: ClientSecret
--           , acScope :: Set Scope
--           , acRedirectUri :: URI
--           , acAuthorizeState :: AuthorizeState
--           , acAuthorizeRequestExtraParams :: Map Text Text
--           , acTokenRequestAuthenticationMethod :: ClientAuthenticationMethod
--           }
