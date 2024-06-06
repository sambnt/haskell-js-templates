{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (parseConfig, cfgHMAC)
import Middleware.HMAC (hmacVerify)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseLBS, requestHeaders)
import Network.HTTP.Types (status200)
import Control.Monad.IO.Class (liftIO)

main = do
  cfg <- parseConfig
  run 8081
    $ hmacVerify (cfgHMAC cfg)
    $ logRequestHeaders
    $ \req respond -> respond $ responseLBS status200 [] "Hello world!"
  print cfg

logRequestHeaders :: Application -> Application
logRequestHeaders incoming request outgoing = do
   let headerList = requestHeaders request
   liftIO $ mapM_ print headerList
   incoming request outgoing
