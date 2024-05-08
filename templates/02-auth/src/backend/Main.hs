{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import Network.Wai (Request, requestHeaders)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler(AuthHandler))
import Control.Monad (guard)
import Control.Arrow ((>>>), second)

import Common

-- server :: TVar Int -> Server CounterAPI
-- server counter = setCountHandler counter :<|> getCountHandler counter

-- counterAPI :: Proxy CounterAPI
-- counterAPI = Proxy

-- app :: TVar Int -> Application
-- app counter = serve counterAPI (server counter)

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

verifyToken :: AccessToken -> IO (Maybe AuthUser)
verifyToken (AccessToken token) = do
  -- body <- HTTP.responseBody <$> mkRequest (getUserInfo uri token)
  -- case Aeson.eitherDecode (ByteStringLazy.fromStrict token) of
  --   Left err -> error err
  --   Right r  -> pure r
  pure $ Just AuthUser

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
