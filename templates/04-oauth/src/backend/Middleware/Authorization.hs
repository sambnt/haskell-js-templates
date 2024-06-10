{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Middleware.Authorization where

import Config.Authorization (ConfigAuthorization, cfgAuthorizationCookie)
import Control.Monad (forM, when)
import Control.Monad.Except (MonadError, catchError, throwError)
import qualified Crypto.Hash as Crypto
import qualified Crypto.Hash.Algorithms as Crypto
import qualified Crypto.MAC.HMAC as Crypto
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Function ((&))
import Data.List (partition, find)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (HeaderName, RequestHeaders)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Middleware, mapRequestHeaders, requestHeaders, responseLBS, Request, requestMethod)
import Web.Cookie (Cookies, parseCookies, renderCookies, renderCookiesBS)
import Config.CSRF (ConfigCSRF, cfgCSRFCookie, cfgCSRFHeader)
import qualified Data.CaseInsensitive as CI

authCookieToHeader :: ConfigAuthorization -> Middleware
authCookieToHeader cfg app r respond = do
  let hdrs = requestHeaders r
      authCookie = cfgAuthorizationCookie cfg

  case (lookup authCookie . parseCookies) =<< lookup "Cookie" hdrs of
    Nothing -> do
      app r respond
    Just jwtBase64 ->
      case Base64URL.decode jwtBase64 of
        Left err ->
          respond $
            responseLBS HTTP.status400 [] $
              ByteStringLazy.fromStrict $
                "Failed to decode cookie '" <> authCookie
                <> "', is it in Base64 encoding? Error was: "
                <> T.encodeUtf8 (T.pack err)
        Right jwt -> do
          let newHdrs = insertHeader "Authorization" ("Bearer " <> jwt) hdrs
          app (mapRequestHeaders (const newHdrs) r) respond

insertHeader ::
  HeaderName ->
  ByteString ->
  RequestHeaders ->
  RequestHeaders
insertHeader hdrName hdrValue hdrs =
  Map.fromList hdrs
  & Map.insert hdrName hdrValue
  & Map.toList
