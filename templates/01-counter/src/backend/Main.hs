{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Servant.API
import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), simpleCorsResourcePolicy, simpleMethods, simpleHeaders, cors)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)

import Common

getCountHandler :: TVar Int -> Handler Int
getCountHandler = liftIO . STM.readTVarIO

setCountHandler :: TVar Int -> Int -> Handler ()
setCountHandler var = liftIO . STM.atomically . STM.writeTVar var

server :: TVar Int -> Server CounterAPI
server counter = setCountHandler counter :<|> getCountHandler counter

counterAPI :: Proxy CounterAPI
counterAPI = Proxy

app :: TVar Int -> Application
app counter = serve counterAPI (server counter)

main :: IO ()
main = do
  counter <- STM.newTVarIO 0
  let
    corsPolicy = simpleCorsResourcePolicy{
      corsOrigins = Nothing,
      corsMethods = simpleMethods,
      corsRequestHeaders = simpleHeaders <> ["Content-Type", "Authorization"]
    }
  run 8081
    $ cors (const $ Just corsPolicy)
    $ app counter
