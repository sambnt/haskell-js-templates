{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (parseConfig, cfgHMAC, cfgCSRF, cfgAuthorization)
import Middleware.HMAC (hmacVerify)
import Middleware.CSRF (csrfProtect)
import Middleware.Authorization (authCookieToHeader)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseLBS, requestHeaders)
import Network.HTTP.Types (status200)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  cfg <- parseConfig
  run 8081
    -- First verify HMAC cookies
    $ hmacVerify (cfgHMAC cfg)
    -- Then protect against CSRF (depends on HMAC verification happening first
    -- to be safe)
    $ csrfProtect (cfgCSRF cfg)
    -- Move auth cookie into the Authorization header
    $ authCookieToHeader (cfgAuthorization cfg)
    $ logRequestHeaders
    -- Then run the app
    $ \req respond -> respond $ responseLBS status200 [] "Hello world!"
  print cfg

logRequestHeaders :: Application -> Application
logRequestHeaders incoming request outgoing = do
   let headerList = requestHeaders request
   liftIO $ mapM_ print headerList
   incoming request outgoing
