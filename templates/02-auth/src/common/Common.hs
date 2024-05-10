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

module Common where

import Servant.API
    ( type (:<|>)(..), JSON, ReqBody, type (:>), Get, Post )
import Data.Proxy ( Proxy(..) )
import qualified Network.URI as Network
import Servant.Links (linkURI)
import GHC.Generics (Generic)
import Servant.Client.Core (clientIn, HasClient(Client), RunClient)
import Servant.API (AuthProtect, (:-), (:>), Get, JSON, NamedRoutes)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (IdentityT, runIdentityT)
import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow, try)
import qualified Control.Concurrent.STM as STM
import Servant.Client.Generic (AsClientT, genericClient)

type Database = TVar Int

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

client :: RunClient m => Api (AsClientT m)
client = genericClient
