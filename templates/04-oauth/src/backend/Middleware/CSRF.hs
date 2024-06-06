{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Middleware.CSRF where

import Config.HMAC (ConfigHMAC, CookieName, HMACSecret, cfgHMACCookies, cfgHMACSecretKey)
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

csrfProtect :: ConfigCSRF -> Middleware
csrfProtect cfg app r respond = do
  case validateCSRF cfg r of
    Left err ->
      respond $
        responseLBS HTTP.status400 []
          (ByteStringLazy.fromStrict $ T.encodeUtf8 err)
    Right () ->
      app r respond

findCookie :: ByteString -> RequestHeaders -> Either Text ByteString
findCookie n hdrs =
  case (lookup n . parseCookies) =<< lookup "Cookie" hdrs of
    Nothing -> Left $ "Missing cookie '" <> T.decodeUtf8 n <> "'"
    Just cookieValue ->
      case Base64URL.decode cookieValue of
        Left err ->
          Left $
            "Failed to decode '" <> T.decodeUtf8 n <> "' cookie, error was: "
            <> T.pack err
        Right v ->
          pure v

findHeader :: ByteString -> RequestHeaders -> Either Text ByteString
findHeader n hdrs =
  case lookup (CI.mk n) hdrs of
    Nothing -> Left $ "Missing header '" <> T.decodeUtf8 n <> "'"
    Just cookieValue -> pure cookieValue

validateCSRF :: ConfigCSRF -> Request -> Either Text ()
validateCSRF cfg r = do
  let
    safeMethods =
      [ HTTP.methodGet
      , HTTP.methodTrace
      , HTTP.methodHead
      , HTTP.methodOptions
      ]
    unsafeRequest = requestMethod r `notElem` safeMethods

  when unsafeRequest $ do
    csrfCookie <- findCookie (cfgCSRFCookie cfg) (requestHeaders r)
    csrfHeader <- findHeader (cfgCSRFHeader cfg) (requestHeaders r)
    when (csrfCookie /= csrfHeader) $
      Left "CSRF cookie does not match CSRF header, suspected CSRF attack"
