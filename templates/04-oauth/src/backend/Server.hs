{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server where

import Api (App(App), Api(Api), getCounter, secured, SecuredApi(SecuredApi), setCounter, AuthAccess)
import Servant (Handler (..), err401)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.Catch (try, MonadThrow)
import Control.Monad.Identity (runIdentityT)
import Control.Concurrent.STM (TVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Client (Manager)
import Servant.Server.Generic (AsServerT)
import Control.Exception (throw)
import qualified Control.Concurrent.STM as STM
import Servant.Server.Experimental.Auth (AuthHandler (AuthHandler), AuthServerData)
import JWT (JWKSCache, verifyJWT)
import Crypto.JWT (SignedJWT, JWTError, ClaimsSet)
import qualified Crypto.JWT as JOSE
import Control.Monad (guard, void)
import qualified Data.ByteString.UTF8 as ByteString
import Control.Arrow ((>>>), second)
import Config.JWT (ConfigJWT)
import qualified Data.ByteString.Lazy as ByteStringLazy
import Network.Wai (Request, requestHeaders)

data Database = Database
  { dbCounter :: TVar Int
  }

type instance AuthServerData AuthAccess = Maybe ()

api :: Database -> Api (AsServerT App)
api db = Api { getCounter = getCounterHandler db
             , secured = securedHandlers db
             }

getCounterHandler :: (MonadThrow m, MonadIO m) => Database -> m Int
getCounterHandler =
  liftIO . STM.readTVarIO . dbCounter

securedHandlers :: Database -> Maybe () -> SecuredApi (AsServerT App)
securedHandlers db (Just authData) =
  SecuredApi { setCounter = setCounterHandler db authData
             }
securedHandlers _ _ =
  throw err401

setCounterHandler :: (MonadIO m) => Database -> () -> Int -> m ()
setCounterHandler db _authData =
  liftIO . STM.atomically . STM.writeTVar (dbCounter db)

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

authHandler :: ConfigJWT -> Manager -> JWKSCache -> AuthHandler Request (Maybe ())
authHandler cfgJwt manager jwksCache = AuthHandler $ \req -> do
  case getToken req of
    Nothing    -> do
      pure Nothing
    Just token -> liftIO $ do
      (result :: Maybe ClaimsSet) <- verifyJWT manager cfgJwt jwksCache token
      pure $ void result

getToken :: Request -> Maybe SignedJWT
getToken req = do
  let
    split = ByteString.break (== ' ') >>> second (ByteString.drop 1)
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  case JOSE.decodeCompact (ByteStringLazy.fromStrict token) of
    Left (err :: JWTError) -> Nothing
    Right jwt -> Just jwt
