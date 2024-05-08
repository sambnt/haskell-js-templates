{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Common where

import Servant.API
    ( type (:<|>)(..), JSON, ReqBody, type (:>), Get, Post )
import Data.Proxy ( Proxy(..) )
import qualified Network.URI as Network
import Servant.Links (linkURI)
import GHC.Generics (Generic)
import Servant.Client.Core (clientIn, HasClient(Client))
import Servant.API (AuthProtect, (:-), (:>), Get, JSON, NamedRoutes)
import Servant.Server.Generic (AsServerT)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Servant (Handler(..), Post, ReqBody, err401)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (IdentityT, runIdentityT)
import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow, try)
import qualified Control.Concurrent.STM as STM
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Exception (throw)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler(AuthHandler))

type Database = TVar Int

-- | Users must provide an opaque access token to authenticate.
type AuthAccess = AuthProtect "access-token'"

type instance AuthServerData AuthAccess = Maybe AuthUser

newtype App a = App (IdentityT IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

data AuthUser = AuthUser
  -- = AuthUser { authUserSub           :: !Text
  --            , authUserName          :: !Text
  --            , authUserEmail         :: !Text
  --            , authUserEmailVerified :: !Bool
  --            }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)

-- instance FromJSON AuthUser where
--   parseJSON = Aeson.withObject "AuthUser" $ \v -> AuthUser
--     <$> v Aeson..: "sub"
--     <*> v Aeson..: "name"
--     <*> v Aeson..: "email"
--     <*> v Aeson..: "email_verified"

data Api mode = Api
  { getCounter
    :: mode
    :- "counter"
    :> Get '[JSON] Int
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

getCounterHandler :: (MonadThrow m, MonadIO m) => Database -> m Int
getCounterHandler = liftIO . STM.readTVarIO

setCounterHandler :: (MonadIO m) => Database -> AuthUser -> Int -> m ()
setCounterHandler db _ = liftIO . STM.atomically . STM.writeTVar db

api :: Database -> Api (AsServerT App)
api db = Api { getCounter = getCounterHandler db
             , secured = securedHandlers db
             }

securedHandlers :: Database -> Maybe AuthUser -> SecuredApi (AsServerT App)
securedHandlers db (Just authUser) =
  SecuredApi { setCounter = setCounterHandler db authUser
             }
securedHandlers _ _ =
  throw err401

-- data CounterAPIClient m
--   = CounterAPIClient
--     { setCount :: !(Client m SetCounter))
--     , getCount :: !(Client m GetCounter)
--     }

-- client :: forall m . HasClient m CounterAPI => CounterAPIClient m
-- client = CounterAPIClient{..}
--   where
--     setCount :<|> getCount = Proxy @CounterAPI `clientIn` Proxy @m
