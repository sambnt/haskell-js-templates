{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.OAuth where

import Servant.API ((:-), (:>), Get, JSON, NamedRoutes, ReqBody, Post)
import GHC.Generics (Generic)
import Servant.API.Experimental.Auth (AuthProtect)
import Control.Monad.Identity (IdentityT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import Servant.Client.Core (clientIn, HasClient(Client), RunClient)
import Servant.Client.Generic (AsClientT, genericClient)
import Data.Text (Text)
import Data.Aeson (FromJSON, parseJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Network.URI (URI)

data Api mode = Api
  { loginStart
    :: mode
    :- "oauth-agent"
    :> "login"
    :> "start"
    :> Post '[JSON] AuthRedirect
  , loginEnd
    :: mode
    :- "oauth-agent"
    :> "login"
    :> "end"
    :> ReqBody '[JSON] CallbackInfo
    :> Post '[JSON] AuthState
  , userInfo
    :: mode
    :- "oauth-agent"
    :> "userInfo"
    :> Get '[JSON] Aeson.Value
  , claims
    :: mode
    :- "oauth-agent"
    :> "claims"
    :> Get '[JSON] ()
  , refresh
    :: mode
    :- "oauth-agent"
    :> "refresh"
    :> Post '[JSON] ()
  , logout
    :: mode
    :- "oauth-agent"
    :> "logout"
    :> Post '[JSON] ()
  }
  deriving Generic

data AuthRedirect =
  -- TODO: authorization -> authentication
  AuthRedirect { authorizationRequestUrl :: URI
               }
  deriving (Eq, Show, Generic)

instance FromJSON AuthRedirect where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON AuthRedirect where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

data AuthState
  = AuthState { handled :: Bool
              , isLoggedIn :: Bool
              , csrf :: Maybe Text
              }
  deriving (Eq, Show, Generic)

instance FromJSON AuthState where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON AuthState where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

data CallbackInfo = CallbackInfo { pageUrl :: Text
                                 }
  deriving (Eq, Show, Generic)

instance FromJSON CallbackInfo where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON CallbackInfo where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

client :: RunClient m => Api (AsClientT m)
client = genericClient
