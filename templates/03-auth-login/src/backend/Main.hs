{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Text
import Servant.API
import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), simpleCorsResourcePolicy, simpleMethods, simpleHeaders, cors)
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)
import Servant.Server.Generic (genericServeTWithContext)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Base64 as ByteStringBase64
import qualified Data.Set as Set
import Network.Wai (Request, requestHeaders)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler(AuthHandler))
import Servant.Server.Generic (AsServerT)
import Control.Monad (guard)
import Control.Arrow ((>>>), second)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, try)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Exception (throw)
import Control.Monad.Identity (runIdentityT)
import Lens.Micro ((.~), (%~))
import Data.Function ((&))
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookieHttpOnly, setCookieSameSite, setCookieMaxAge, sameSiteStrict)

import Common

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

type instance AuthServerData AuthAccess = Maybe AuthUser

getCounterHandler :: (MonadThrow m, MonadIO m) => Database -> m Int
getCounterHandler = liftIO . STM.readTVarIO

setCounterHandler :: (MonadIO m) => Database -> AuthUser -> Int -> m ()
setCounterHandler db _ = liftIO . STM.atomically . STM.writeTVar db

loginHandler :: MonadIO m => m (Headers '[Header "Set-Cookie" SetCookie] ())
loginHandler = do
  let
    authCookie =
      defaultSetCookie
        { setCookieName = "X-Auth"
        , setCookieValue = "deadbeef"
        , setCookieHttpOnly = True
        , setCookieSameSite = Just sameSiteStrict
        -- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie
        -- Indicates the number of seconds until the cookie expires. A zero or negative number will expire the cookie immediately. If both Expires and Max-Age are set, Max-Age has precedence.
        , setCookieMaxAge = Just 30 -- seconds
        }
  pure $ addHeader authCookie ()

api :: Database -> Api (AsServerT App)
api db = Api { getCounter = getCounterHandler db
             , login = loginHandler
             , secured = securedHandlers db
             }

securedHandlers :: Database -> Maybe AuthUser -> SecuredApi (AsServerT App)
securedHandlers db (Just authUser) =
  SecuredApi { setCounter = setCounterHandler db authUser
             }
securedHandlers _ _ =
  throw err401

newtype AccessToken = AccessToken ByteString

mkAuthHandler :: (req -> Handler usr) -> AuthHandler req usr
mkAuthHandler = AuthHandler

authHandler :: AuthHandler Request (Maybe AuthUser)
authHandler = mkAuthHandler $ \req -> do
  case getToken req of
    Nothing    -> pure Nothing
    Just token -> liftIO $ verifyToken token

getToken :: Request -> Maybe AccessToken
getToken req = do
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  pure $ AccessToken token
  where
    split = ByteString.break (== ' ') >>> second (ByteString.drop 1)

-- | A simple, unsafe verification scheme where we succeed if we are able to
-- decode an "AuthUser" from the JWT payload.
verifyToken :: AccessToken -> IO (Maybe AuthUser)
verifyToken (AccessToken token) = do
  -- WARNING: None of this is safe. Use "verifyJWS" from the jose package
  -- in production.
  let
    split = ByteString.break (== '.') >>> second (ByteString.drop 1)
    (hdr, (payload, sig)) =
      split >>> second split $ token

  case Aeson.eitherDecodeStrict $ ByteStringBase64.decodeLenient payload of
    Left err -> error $ err
    Right r  -> pure $ Just r

main :: IO ()
main = do
  counter <- STM.newTVarIO 0

  let
    ctx =
      authHandler -- @AuthUser
      :. EmptyContext

    corsPolicy = simpleCorsResourcePolicy{
      corsOrigins = Nothing,
      corsMethods = simpleMethods,
      corsRequestHeaders = simpleHeaders <> ["Content-Type", "Authorization"]
    }
  run 8081
    $ cors (const $ Just corsPolicy)
    $ genericServeTWithContext appToHandler (api counter) ctx
