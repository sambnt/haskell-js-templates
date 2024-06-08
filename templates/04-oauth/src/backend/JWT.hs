{-# LANGUAGE ScopedTypeVariables #-}
module JWT where

import Config.JWT (ConfigJWT, cfgJWKSUri, cfgIssuer)
import Crypto.JWT (JWTValidationSettings, SignedJWT, HasClaimsSet, JWKSet, JWTError)
import Data.Text
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON)
import qualified Network.HTTP.Client as HTTP
import Control.Concurrent.STM (TVar)
import qualified Crypto.JOSE as JOSE
import Control.Monad.Except (runExceptT)
import qualified Crypto.JWT as JOSE
import qualified Control.Concurrent.STM as STM
import qualified Network.HTTP.Types.Status as HTTP
import qualified Data.Text as T
import Lens.Micro ((^.))
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Aeson.KeyMap as Aeson
import Data.String (fromString)
import qualified Data.ByteString.UTF8 as ByteString
import Control.Arrow ((>>>), second)

type JWKSCache = TVar JWKSet

newJWKSCache :: IO JWKSCache
newJWKSCache = STM.newTVarIO $ JOSE.JWKSet []

jwtValidationSettings :: ConfigJWT -> JWTValidationSettings
jwtValidationSettings cfg =
  JOSE.defaultJWTValidationSettings
    (== fromString (T.unpack $ cfgIssuer cfg))

data KidError = MalformedInput Text

getKid :: SignedJWT -> Either KidError Text
getKid jwt = do
  let bytes = ByteStringLazy.toStrict $ JOSE.encodeCompact jwt
      split = ByteString.break (== '.') >>> second (ByteString.drop 1)
      (hdr64, _) = split bytes

      eKid = do
        hdr <- Base64.decode hdr64
        keyMap <- Aeson.eitherDecode $ ByteStringLazy.fromStrict hdr
        case Aeson.lookup (fromString "kid") keyMap of
          (Just (Aeson.String kid)) -> Right kid
          _ -> Left "Unable to find 'kid'"
  case eKid of
    Left err -> Left $ MalformedInput (T.pack err)
    Right kid -> Right kid

verifyJWT ::
  (HasClaimsSet a, FromJSON a) =>
  HTTP.Manager ->
  ConfigJWT ->
  JWKSCache ->
  SignedJWT ->
  IO (Maybe a)
verifyJWT manager cfg jwksCache jwt = do
  let settings = jwtValidationSettings cfg

  (JOSE.JWKSet jwks) <- STM.readTVarIO jwksCache
  jwkSet <- case getKid jwt of
    -- If the kid is not present, we don't do anything
    Left (MalformedInput _) -> pure (JOSE.JWKSet jwks)
    Right kid -> do
      -- If the kid is present in the JWT and in the JWKSet, do nothing
      if kid `Prelude.elem` mapMaybe (^. JOSE.jwkKid) jwks
        then pure (JOSE.JWKSet jwks)
        -- If the kid is present in the JWT but not in the JWKSet, we refresh the
        -- JWKSet
        else do
          req <- HTTP.parseRequest $ T.unpack $ cfgJWKSUri cfg
          response <- HTTP.httpLbs req manager
          let status = HTTP.responseStatus response
          if status == HTTP.ok200
            then case Aeson.eitherDecode (HTTP.responseBody response) of
              Left err -> pure (JOSE.JWKSet jwks)
              Right jwks' -> do
                STM.atomically $ STM.writeTVar jwksCache jwks'
                pure jwks'
            else
              pure (JOSE.JWKSet jwks)

  result <- runExceptT $ JOSE.verifyJWT settings jwkSet jwt
  case result of
    Left (err :: JWTError) -> pure Nothing
    Right x -> pure $ Just x
