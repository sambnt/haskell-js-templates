{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Middleware.HMAC where

import Config.HMAC (ConfigHMAC, CookieName, HMACSecret, cfgHMACCookies, cfgHMACSecretKey)
import Control.Monad (forM)
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
import Data.List (partition)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (HeaderName, RequestHeaders)
import Network.HTTP.Types.Status (status400)
import Network.Wai (Middleware, mapRequestHeaders, requestHeaders, responseLBS)
import Web.Cookie (Cookies, parseCookies, renderCookies, renderCookiesBS)

hmacVerify :: ConfigHMAC -> Middleware
hmacVerify cfg app r respond = do
  let cookieNames = T.encodeUtf8 <$> cfgHMACCookies cfg
      reqHdrs = requestHeaders r
      keyBytes = cfgHMACSecretKey cfg

  case modifyCookies (hmacVerifyCookies keyBytes cookieNames) reqHdrs of
    Left err ->
      respond $
        responseLBS status400 [] (ByteStringLazy.fromStrict $ T.encodeUtf8 err)
    Right newReqHdrs -> do
      let finalRequest =
            mapRequestHeaders (const newReqHdrs) r
      app finalRequest respond

modifyCookies ::
  (Monad m) =>
  (Cookies -> m Cookies) ->
  RequestHeaders ->
  m RequestHeaders
modifyCookies f =
  modifyHeader (fromString "Cookie") (fmap renderCookiesBS . f . parseCookies)

modifyHeader ::
  (Monad m) =>
  HeaderName ->
  (ByteString -> m ByteString) ->
  RequestHeaders ->
  m RequestHeaders
modifyHeader hdrName f hdrs = do
  forM hdrs $ \(name, value) -> do
    if name /= hdrName
      then pure (name, value)
      else (name,) <$> f value

hmacVerifyCookies ::
  (MonadError Text m) =>
  -- | Secret key bytes
  ByteString ->
  -- | Names of cookies to verify
  [ByteString] ->
  -- | Cookies in request
  Cookies ->
  -- | Cookies to return in modified request
  m Cookies
hmacVerifyCookies key cookiesToVerify cookies = do
  forM cookies $ \(cookieName, cookieValue) -> do
    if cookieName `notElem` cookiesToVerify
      then pure (cookieName, cookieValue)
      else cookieFailure cookieName $ do
        hmac <- decodeHMACCookie cookieValue
        msg <- verifyHMACCookie key hmac
        pure (cookieName, Base64URL.encode msg)

cookieFailure :: (MonadError Text m) => ByteString -> m a -> m a
cookieFailure cookieName =
  flip catchError $ \t ->
    throwError $ "Failure in cookie '" <> T.decodeUtf8 cookieName <> "': " <> t

data HMACCookie
  = HMACCookie
  { hmacDigest :: Crypto.Digest Crypto.SHA256,
    hmacRandom :: ByteString,
    hmacMessage :: ByteString
  }

instance Show HMACCookie where
  show hmac =
    ByteArray.convert (hmacDigest hmac)
      <> "."
      <> hmacRandom hmac
      <> hmacMessage hmac
      & Base64URL.encode
      & T.decodeUtf8
      & T.unpack

decodeHMACCookie :: (MonadError Text m) => ByteString -> m HMACCookie
decodeHMACCookie bytes64 = do
  bytes <- case Base64URL.decode bytes64 of
    Left err ->
      throwError $
        "Failed to decode HMAC cookie, is it Base64URL encoded? Error was: '"
          <> T.pack err
          <> "'"
    Right bs ->
      pure bs

  case ByteString.break (== (fromIntegral $ fromEnum '.')) bytes of
    (_, xs)
      | ByteString.length xs < 1 ->
          throwError "Expected two parts: 'hash.message', but found none"
    (digestBytes, rest) ->
      let (random, message) =
            ByteString.drop 1 rest -- Get rid of the separator
              & ByteString.splitAt 12 -- Then split the random byte from the message
          mDigest = Crypto.digestFromByteString digestBytes
       in case mDigest of
            Nothing -> throwError "Failed to construct HMAC hash from input."
            Just digest ->
              pure $ HMACCookie digest random message

encodeHMACCookie :: HMACCookie -> ByteString
encodeHMACCookie hmac =
  ByteArray.convert (hmacDigest hmac)
    <> T.encodeUtf8 "."
    <> hmacRandom hmac
    <> hmacMessage hmac

verifyHMACCookie ::
  (MonadError Text m, ByteArrayAccess key) =>
  key ->
  HMACCookie ->
  m ByteString
verifyHMACCookie key hmac =
  let expectedDigest :: Crypto.Digest Crypto.SHA256
      expectedDigest =
        Crypto.hmac key (hmacRandom hmac <> hmacMessage hmac)
          & Crypto.hmacGetDigest
   in if hmacDigest hmac /= expectedDigest
        then throwError "Expected hash didn't match given hash"
        else pure (hmacMessage hmac)

-- Generate key1
-- Generate key2
-- if key1 == key2; discard
-- else
--   Generate msg
--   hmac key1 msg != hmac key2 msg
