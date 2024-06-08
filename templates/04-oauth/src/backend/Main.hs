{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config (parseConfig, cfgHMAC, cfgCSRF, cfgAuthorization, cfgJWT)
import Middleware.HMAC (hmacVerify)
import Middleware.CSRF (csrfProtect)
import Middleware.Authorization (authCookieToHeader)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseLBS, requestHeaders)
import Network.HTTP.Types (status200)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Data.ByteString.Lazy as ByteStringLazy

import JWT (newJWKSCache, verifyJWT)
import qualified Crypto.JWT as JOSE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = do
  cfg <- parseConfig

  jwksCache <- newJWKSCache
  manager <- newTlsManager

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
    $ \req respond -> do
      let (Just auth) = lookup "Authorization" $ requestHeaders req
          (Right jwt :: Either JOSE.JWTError JOSE.SignedJWT) = JOSE.decodeCompact $ ByteStringLazy.fromStrict auth
      ((Just claimsSet) :: Maybe JOSE.ClaimsSet) <-
        verifyJWT manager (cfgJWT cfg) jwksCache jwt
      respond $ responseLBS status200 [] (ByteStringLazy.fromStrict $ T.encodeUtf8 $ T.pack $ show claimsSet)
  print cfg

logRequestHeaders :: Application -> Application
logRequestHeaders incoming request outgoing = do
   let headerList = requestHeaders request
   liftIO $ mapM_ print headerList
   incoming request outgoing
