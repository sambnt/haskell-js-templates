{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api where

import Servant.API ((:-), (:>), Get, JSON, NamedRoutes, ReqBody, Post)
import GHC.Generics (Generic)
import Servant.API.Experimental.Auth (AuthProtect)
import Control.Monad.Identity (IdentityT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import Servant.Client.Core (clientIn, HasClient(Client), RunClient)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Users must provide an opaque access token to authenticate.
type AuthAccess = AuthProtect "access-token'"

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

newtype App a = App (IdentityT IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)

client :: RunClient m => Api (AsClientT m)
client = genericClient
